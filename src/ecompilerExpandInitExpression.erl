-module(ecompilerExpandInitExpression).

-export([expandInitExpressionInFunctions/2, expandInitExpressions/2]).

-include("ecompilerFrameDef.hrl").

%% for now, array and struct init expression is only allowed in assignment
checkInitExpressionPosition(#operatorExpression2{operator = assign, operand2 = Operand2}) when is_record(Operand2, structInitializeExpression); is_record(Operand2, arrayInitializeExpression) ->
    ok;
checkInitExpressionPosition(#structInitializeExpression{line = Line}) ->
    throw({Line, "struct init expression is only allowed in assignments"});
checkInitExpressionPosition(#arrayInitializeExpression{line = Line}) ->
    throw({Line, "array init expression is only allowed in assignments"});
checkInitExpressionPosition(_) ->
    ok.

expandInitExpressionInFunctions([#function{statements = Expressions} = F | Rest], StructMap) ->
    ecompilerUtil:expressionMap(fun checkInitExpressionPosition/1, Expressions),
    [F#function{statements = expandInitExpressions(Expressions, StructMap)} | expandInitExpressionInFunctions(Rest, StructMap)];
expandInitExpressionInFunctions([Any | Rest], StructMap) ->
    [Any | expandInitExpressionInFunctions(Rest, StructMap)];
expandInitExpressionInFunctions([], _) ->
    [].

expandInitExpressions(Expressions, StructMap) ->
    expandInitExpression(Expressions, [], StructMap).

expandInitExpression([#ifStatement{then = Then, else = Else} = E | Rest], NewAst, StructMap) ->
    expandInitExpression(Rest, [E#ifStatement{then = expandInitExpression(Then, [], StructMap), else = expandInitExpression(Else, [], StructMap)} | NewAst], StructMap);
expandInitExpression([#whileStatement{statements = Expressions} = E | Rest], NewAst, StructMap) ->
    expandInitExpression(Rest, [E#whileStatement{statements = expandInitExpression(Expressions, [], StructMap)} | NewAst], StructMap);
expandInitExpression([#operatorExpression2{} = Op | Rest], NewAst, StructMap) ->
    expandInitExpression(Rest, replaceInitOps(Op, StructMap) ++ NewAst, StructMap);
expandInitExpression([Any | Rest], NewAst, StructMap) ->
    expandInitExpression(Rest, [Any | NewAst], StructMap);
expandInitExpression([], NewAst, _) ->
    lists:reverse(NewAst).

replaceInitOps(#operatorExpression2{operator = assign, operand1 = Operand1, operand2 = #structInitializeExpression{name = Name, line = Line, fieldValueMap = FieldValues}}, StructMap) ->
    case maps:find(Name, StructMap) of
        {ok, #struct{fieldNames = FieldNames, fieldTypeMap = FieldTypes, fieldDefaultValueMap = FieldDefaults}} ->
            FieldValueMap = maps:merge(FieldDefaults, FieldValues),
            structInitToOps(Operand1, FieldNames, FieldValueMap, FieldTypes, [], StructMap);
        error ->
            throw({Line, ecompilerUtil:fmt("struct ~s is not found", [Name])})
    end;
replaceInitOps(#operatorExpression2{operator = assign, operand1 = Operand1, operand2 = #arrayInitializeExpression{elements = Elements, line = Line}}, StructMap) ->
    arrayInitToOps(Operand1, Elements, 0, Line, [], StructMap);
replaceInitOps(Any, _) ->
    [Any].

structInitToOps(Target, [#variableReference{line = Line, name = Fname} = Field | Rest], FieldInitMap, FieldTypes, Newcode, StructMap) ->
    Operand2 = case maps:find(Fname, FieldInitMap) of
                   {ok, InitOp} ->
                       InitOp;
                   error ->
                       defaultInitValueOf(maps:get(Fname, FieldTypes), Line)
               end,
    NewAssign = #operatorExpression2{operator = assign, operand2 = Operand2, line = Line, operand1 = #operatorExpression2{operator = '.', operand1 = Target, operand2 = Field, line = Line}},
    Ops = replaceInitOps(NewAssign, StructMap),
    structInitToOps(Target, Rest, FieldInitMap, FieldTypes, Ops ++ Newcode, StructMap);
structInitToOps(_, [], _, _, Newcode, _) ->
    Newcode.

defaultInitValueOf(#arrayType{elemtype = Etype, length = Len}, Line) ->
    #arrayInitializeExpression{elements = lists:duplicate(Len, defaultInitValueOf(Etype, Line)), line = Line};
defaultInitValueOf(#basicType{class = struct, tag = Tag, pdepth = 0}, Line) ->
    #structInitializeExpression{name = Tag, line = Line, fieldValueMap = #{}, fieldNames = []};
defaultInitValueOf(#basicType{class = integer, pdepth = 0}, Line) ->
    {integer, Line, 0};
defaultInitValueOf(#basicType{class = float, pdepth = 0}, Line) ->
    {float, Line, 0.0};
defaultInitValueOf(#basicType{pdepth = Pdepth}, Line) when Pdepth > 0 ->
    {integer, Line, 0}.

arrayInitToOps(Target, [E | Rest], Cnt, Line, Newcode, StructMap) ->
    A = #operatorExpression1{operator = '@', line = Line, operand = Target},
    B = #operatorExpression2{operator = '+', operand2 = {integer, Line, Cnt}, line = Line, operand1 = A},
    C = #operatorExpression1{operator = '^', line = Line, operand = B},
    NewAssign = #operatorExpression2{operator = assign, operand1 = C, operand2 = E, line = Line},
    Ops = replaceInitOps(NewAssign, StructMap),
    arrayInitToOps(Target, Rest, Cnt + 1, Line, Ops ++ Newcode, StructMap);
arrayInitToOps(_, [], _, _, Newcode, _) ->
    Newcode.
