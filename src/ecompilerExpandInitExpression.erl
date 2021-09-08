-module(ecompilerExpandInitExpression).

-export([expandInitExpressionInFunctions/2, expandInitExpressions/2]).

-include("ecompilerFrameDef.hrl").

%% for now, array and struct init expression is only allowed in assignment
prvCheckInitExpressionPosition(#op2{operator = assign, op2 = Operand2}) when is_record(Operand2, struct_init); is_record(Operand2, array_init) ->
    ok;
prvCheckInitExpressionPosition(#struct_init{line = Line}) ->
    throw({Line, "struct init expression is only allowed in assignments"});
prvCheckInitExpressionPosition(#array_init{line = Line}) ->
    throw({Line, "array init expression is only allowed in assignments"});
prvCheckInitExpressionPosition(_) ->
    ok.

expandInitExpressionInFunctions([#function{exprs = Expressions} = F | Rest], StructMap) ->
    ecompilerUtil:expressionMap( fun prvCheckInitExpressionPosition/1,  Expressions ),
    [F#function{exprs = expandInitExpressions(Expressions, StructMap)} | expandInitExpressionInFunctions(Rest, StructMap)];
expandInitExpressionInFunctions([Any | Rest], StructMap) ->
    [Any | expandInitExpressionInFunctions(Rest, StructMap)];
expandInitExpressionInFunctions([], _) ->
    [].

expandInitExpressions(Expressions, StructMap) -> prvExpandInitExpression(Expressions, [], StructMap).

prvExpandInitExpression([#if_expr{then = Then, else = Else} = E | Rest], NewAst, StructMap) ->
    prvExpandInitExpression(Rest, [E#if_expr{then = prvExpandInitExpression(Then, [], StructMap), else = prvExpandInitExpression(Else, [], StructMap)} | NewAst], StructMap);
prvExpandInitExpression([#while_expr{exprs = Expressions} = E | Rest], NewAst, StructMap) ->
    prvExpandInitExpression(Rest, [E#while_expr{exprs = prvExpandInitExpression(Expressions, [], StructMap)} | NewAst], StructMap);
prvExpandInitExpression([#op2{} = Op | Rest], NewAst, StructMap) ->
    prvExpandInitExpression(Rest, prvReplaceInitOps(Op, StructMap) ++ NewAst, StructMap);
prvExpandInitExpression([Any | Rest], NewAst, StructMap) ->
    prvExpandInitExpression(Rest, [Any | NewAst], StructMap);
prvExpandInitExpression([], NewAst, _) ->
    lists:reverse(NewAst).

prvReplaceInitOps(#op2{operator = assign, op1 = Operand1, op2 = #struct_init{name = Name, line = Line, field_values = FieldValues}}, StructMap) ->
    case maps:find(Name, StructMap) of
        {ok, #struct{field_names = FieldNames, field_types = FieldTypes, field_defaults = FieldDefaults}} ->
            FieldValueMap = maps:merge(FieldDefaults, FieldValues),
            prvStructInitToOps(Operand1, FieldNames, FieldValueMap, FieldTypes, [], StructMap);
        error ->
            throw({Line, ecompilerUtil:flatfmt("struct ~s is not found", [Name])})
    end;
prvReplaceInitOps(#op2{operator = assign, op1 = Operand1, op2 = #array_init{elements = Elements, line = Line}}, StructMap) ->
    prvArrayInitToOps(Operand1, Elements, 0, Line, [], StructMap);
prvReplaceInitOps(Any, _) ->
    [Any].

prvStructInitToOps(Target, [#varref{line = Line, name = Fname} = Field | Rest], FieldInitMap, FieldTypes, Newcode, StructMap) ->
    Operand2 =  case maps:find(Fname, FieldInitMap) of
                    {ok, InitOp}    -> InitOp;
                    error           -> prvDefaultInitValueOf( maps:get(Fname, FieldTypes),  Line )
                end,
    NewAssign = #op2{operator = assign, op2 = Operand2, line = Line, op1 = #op2{operator = '.', op1 = Target, op2 = Field, line = Line}},
    Ops = prvReplaceInitOps(NewAssign, StructMap),
    prvStructInitToOps(Target, Rest, FieldInitMap, FieldTypes, Ops ++ Newcode, StructMap);
prvStructInitToOps(_, [], _, _, Newcode, _) ->
    Newcode.

prvDefaultInitValueOf(#array_type{elemtype = Etype, len = Len}, Line) ->
    #array_init{elements = lists:duplicate( Len, prvDefaultInitValueOf(Etype, Line) ),  line = Line};
prvDefaultInitValueOf(#basic_type{class = struct, tag = Tag, pdepth = 0}, Line) ->
    #struct_init{name = Tag, line = Line, field_values = #{}, field_names = []};
prvDefaultInitValueOf(#basic_type{class = integer, pdepth = 0}, Line) ->
    {integer, Line, 0};
prvDefaultInitValueOf(#basic_type{class = float, pdepth = 0}, Line) ->
    {float, Line, 0.0};
prvDefaultInitValueOf(#basic_type{pdepth = Pdepth}, Line) when Pdepth > 0 ->
    {integer, Line, 0}.

prvArrayInitToOps(Target, [E | Rest], Cnt, Line, Newcode, StructMap) ->
    A = #op1{operator = '@', line = Line, operand = Target},
    B = #op2{operator = '+', op2 = {integer, Line, Cnt}, line = Line, op1 = A},
    C = #op1{operator = '^', line = Line, operand = B},
    NewAssign = #op2{operator = assign, op1 = C, op2 = E, line = Line},
    Ops = prvReplaceInitOps(NewAssign, StructMap),
    prvArrayInitToOps(Target, Rest, Cnt + 1, Line, Ops ++ Newcode, StructMap);
prvArrayInitToOps(_, [], _, _, Newcode, _) ->
    Newcode.
