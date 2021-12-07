-module(eInitExpression).

-export([expandInFunctions/2, expandInitExpressions/2]).

-include("eRecordDefinition.hrl").

%% for now, array and struct init expression is only allowed in assignment
checkPosition(#operatorExpression2{operator = assign, operand2 = Operand2}) when is_record(Operand2, structInitializeExpression); is_record(Operand2, arrayInitializeExpression) ->
    ok;
checkPosition(#structInitializeExpression{line = Line}) ->
    throw({Line, "struct init expression is only allowed in assignments"});
checkPosition(#arrayInitializeExpression{line = Line}) ->
    throw({Line, "array init expression is only allowed in assignments"});
checkPosition(_) ->
    ok.

expandInFunctions([#function{statements = Expressions} = F | Rest], StructMap) ->
    eUtil:expressionMap(fun checkPosition/1, Expressions),
    [F#function{statements = expandInitExpressions(Expressions, StructMap)} | expandInFunctions(Rest, StructMap)];
expandInFunctions([Any | Rest], StructMap) ->
    [Any | expandInFunctions(Rest, StructMap)];
expandInFunctions([], _) ->
    [].

expandInitExpressions(Expressions, StructMap) ->
    expandInitExpression(Expressions, [], StructMap).

expandInitExpression([#ifStatement{then = Then, else = Else} = E | Rest], NewAST, StructMap) ->
    expandInitExpression(Rest, [E#ifStatement{then = expandInitExpression(Then, [], StructMap), else = expandInitExpression(Else, [], StructMap)} | NewAST], StructMap);
expandInitExpression([#whileStatement{statements = Expressions} = E | Rest], NewAST, StructMap) ->
    expandInitExpression(Rest, [E#whileStatement{statements = expandInitExpression(Expressions, [], StructMap)} | NewAST], StructMap);
expandInitExpression([#operatorExpression2{} = Op | Rest], NewAST, StructMap) ->
    expandInitExpression(Rest, replaceInitOps(Op, StructMap) ++ NewAST, StructMap);
expandInitExpression([Any | Rest], NewAST, StructMap) ->
    expandInitExpression(Rest, [Any | NewAST], StructMap);
expandInitExpression([], NewAST, _) ->
    lists:reverse(NewAST).

replaceInitOps(#operatorExpression2{operator = assign, operand1 = Operand1, operand2 = #structInitializeExpression{name = Name, line = Line, fieldValueMap = FieldValues}}, StructMap) ->
    case maps:find(Name, StructMap) of
        {ok, #struct{fieldNames = FieldNames, fieldTypeMap = FieldTypes, fieldDefaultValueMap = FieldDefaults}} ->
            FieldValueMap = maps:merge(FieldDefaults, FieldValues),
            structInitToOps(Operand1, FieldNames, FieldValueMap, FieldTypes, [], StructMap);
        error ->
            throw({Line, eUtil:fmt("struct ~s is not found", [Name])})
    end;
replaceInitOps(#operatorExpression2{operator = assign, operand1 = Operand1, operand2 = #arrayInitializeExpression{elements = Elements, line = Line}}, StructMap) ->
    arrayInitToOps(Operand1, Elements, 0, Line, [], StructMap);
replaceInitOps(Any, _) ->
    [Any].

structInitToOps(Target, [#variableReference{line = Line, name = FieldName} = Field | Rest], FieldInitMap, FieldTypes, NewCode, StructMap) ->
    Operand2 = case maps:find(FieldName, FieldInitMap) of
                   {ok, InitOp} ->
                       InitOp;
                   error ->
                       defaultInitValueOf(maps:get(FieldName, FieldTypes), Line)
               end,
    NewAssign = #operatorExpression2{operator = assign, operand2 = Operand2, line = Line, operand1 = #operatorExpression2{operator = '.', operand1 = Target, operand2 = Field, line = Line}},
    Ops = replaceInitOps(NewAssign, StructMap),
    structInitToOps(Target, Rest, FieldInitMap, FieldTypes, Ops ++ NewCode, StructMap);
structInitToOps(_, [], _, _, NewCode, _) ->
    NewCode.

defaultInitValueOf(#arrayType{elemtype = ElementType, length = Len}, Line) ->
    #arrayInitializeExpression{elements = lists:duplicate(Len, defaultInitValueOf(ElementType, Line)), line = Line};
defaultInitValueOf(#basicType{class = struct, tag = Tag, pdepth = 0}, Line) ->
    #structInitializeExpression{name = Tag, line = Line, fieldValueMap = #{}, fieldNames = []};
defaultInitValueOf(#basicType{class = integer, pdepth = 0}, Line) ->
    {integer, Line, 0};
defaultInitValueOf(#basicType{class = float, pdepth = 0}, Line) ->
    {float, Line, 0.0};
defaultInitValueOf(#basicType{pdepth = PointerDepth}, Line) when PointerDepth > 0 ->
    {integer, Line, 0}.

arrayInitToOps(Target, [E | Rest], Cnt, Line, NewCode, StructMap) ->
    A = #operatorExpression1{operator = '@', line = Line, operand = Target},
    B = #operatorExpression2{operator = '+', operand2 = {integer, Line, Cnt}, line = Line, operand1 = A},
    C = #operatorExpression1{operator = '^', line = Line, operand = B},
    NewAssign = #operatorExpression2{operator = assign, operand1 = C, operand2 = E, line = Line},
    Ops = replaceInitOps(NewAssign, StructMap),
    arrayInitToOps(Target, Rest, Cnt + 1, Line, Ops ++ NewCode, StructMap);
arrayInitToOps(_, [], _, _, NewCode, _) ->
    NewCode.
