-module(ecompilerExpandInitExpression).

-export([expandInitExpressionInFunctions/2, expandInitExpressions/2]).

-include("ecompilerFrameDef.hrl").

%% for now, array and struct init expression is only allowed in assignment
checkInitExpressionPosition(#op2{operator = assign, op2 = Operand2}) when is_record(Operand2, struct_init); is_record(Operand2, array_init) ->
    ok;
checkInitExpressionPosition(#struct_init{line = Line}) ->
    throw({Line, "struct init expression is only allowed in assignments"});
checkInitExpressionPosition(#array_init{line = Line}) ->
    throw({Line, "array init expression is only allowed in assignments"});
checkInitExpressionPosition(_) ->
    ok.

expandInitExpressionInFunctions([#function{exprs = Expressions} = F | Rest], StructMap) ->
    ecompilerUtil:expressionMap(fun checkInitExpressionPosition/1, Expressions),
    [F#function{exprs = expandInitExpressions(Expressions, StructMap)} | expandInitExpressionInFunctions(Rest, StructMap)];
expandInitExpressionInFunctions([Any | Rest], StructMap) ->
    [Any | expandInitExpressionInFunctions(Rest, StructMap)];
expandInitExpressionInFunctions([], _) ->
    [].

expandInitExpressions(Expressions, StructMap) ->
    expandInitExpression(Expressions, [], StructMap).

expandInitExpression([#if_expr{then = Then, else = Else} = E | Rest], NewAst, StructMap) ->
    expandInitExpression(Rest, [E#if_expr{then = expandInitExpression(Then, [], StructMap), else = expandInitExpression(Else, [], StructMap)} | NewAst], StructMap);
expandInitExpression([#while_expr{exprs = Expressions} = E | Rest], NewAst, StructMap) ->
    expandInitExpression(Rest, [E#while_expr{exprs = expandInitExpression(Expressions, [], StructMap)} | NewAst], StructMap);
expandInitExpression([#op2{} = Op | Rest], NewAst, StructMap) ->
    expandInitExpression(Rest, replaceInitOps(Op, StructMap) ++ NewAst, StructMap);
expandInitExpression([Any | Rest], NewAst, StructMap) ->
    expandInitExpression(Rest, [Any | NewAst], StructMap);
expandInitExpression([], NewAst, _) ->
    lists:reverse(NewAst).

replaceInitOps(#op2{operator = assign, op1 = Operand1, op2 = #struct_init{name = Name, line = Line, field_values = FieldValues}}, StructMap) ->
    case maps:find(Name, StructMap) of
        {ok, #struct{field_names = FieldNames, field_types = FieldTypes, field_defaults = FieldDefaults}} ->
            FieldValueMap = maps:merge(FieldDefaults, FieldValues),
            structInitToOps(Operand1, FieldNames, FieldValueMap, FieldTypes, [], StructMap);
        error ->
            throw({Line, ecompilerUtil:flatfmt("struct ~s is not found", [Name])})
    end;
replaceInitOps(#op2{operator = assign, op1 = Operand1, op2 = #array_init{elements = Elements, line = Line}}, StructMap) ->
    arrayInitToOps(Operand1, Elements, 0, Line, [], StructMap);
replaceInitOps(Any, _) ->
    [Any].

structInitToOps(Target, [#varref{line = Line, name = Fname} = Field | Rest], FieldInitMap, FieldTypes, Newcode, StructMap) ->
    Operand2 = case maps:find(Fname, FieldInitMap) of
                   {ok, InitOp} ->
                       InitOp;
                   error ->
                       defaultInitValueOf(maps:get(Fname, FieldTypes), Line)
               end,
    NewAssign = #op2{operator = assign, op2 = Operand2, line = Line, op1 = #op2{operator = '.', op1 = Target, op2 = Field, line = Line}},
    Ops = replaceInitOps(NewAssign, StructMap),
    structInitToOps(Target, Rest, FieldInitMap, FieldTypes, Ops ++ Newcode, StructMap);
structInitToOps(_, [], _, _, Newcode, _) ->
    Newcode.

defaultInitValueOf(#array_type{elemtype = Etype, len = Len}, Line) ->
    #array_init{elements = lists:duplicate(Len, defaultInitValueOf(Etype, Line)), line = Line};
defaultInitValueOf(#basic_type{class = struct, tag = Tag, pdepth = 0}, Line) ->
    #struct_init{name = Tag, line = Line, field_values = #{}, field_names = []};
defaultInitValueOf(#basic_type{class = integer, pdepth = 0}, Line) ->
    {integer, Line, 0};
defaultInitValueOf(#basic_type{class = float, pdepth = 0}, Line) ->
    {float, Line, 0.0};
defaultInitValueOf(#basic_type{pdepth = Pdepth}, Line) when Pdepth > 0 ->
    {integer, Line, 0}.

arrayInitToOps(Target, [E | Rest], Cnt, Line, Newcode, StructMap) ->
    A = #op1{operator = '@', line = Line, operand = Target},
    B = #op2{operator = '+', op2 = {integer, Line, Cnt}, line = Line, op1 = A},
    C = #op1{operator = '^', line = Line, operand = B},
    NewAssign = #op2{operator = assign, op1 = C, op2 = E, line = Line},
    Ops = replaceInitOps(NewAssign, StructMap),
    arrayInitToOps(Target, Rest, Cnt + 1, Line, Ops ++ Newcode, StructMap);
arrayInitToOps(_, [], _, _, Newcode, _) ->
    Newcode.
