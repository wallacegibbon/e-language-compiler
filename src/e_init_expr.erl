-module(e_init_expr).
-export([expand_in_function/2, expand_init_expr/2]).

-include("e_record_definition.hrl").

%% for now, array and struct init expression is only allowed in assignment
check_position(#op2_expr{operator = assign, operand2 = Operand2}) when is_record(Operand2, struct_init_expr); is_record(Operand2, array_init_expr) ->
    ok;
check_position(#struct_init_expr{line = Line}) ->
    throw({Line, "struct init expression is only allowed in assignments"});
check_position(#array_init_expr{line = Line}) ->
    throw({Line, "array init expression is only allowed in assignments"});
check_position(_) ->
    ok.

expand_in_function([#function{stmts = Expressions} = F | Rest], StructMap) ->
    e_util:expr_map(fun check_position/1, Expressions),
    [F#function{stmts = expand_init_expr(Expressions, StructMap)} | expand_in_function(Rest, StructMap)];
expand_in_function([Any | Rest], StructMap) ->
    [Any | expand_in_function(Rest, StructMap)];
expand_in_function([], _) ->
    [].

expand_init_expr(Expressions, StructMap) ->
    expand_init_expr(Expressions, [], StructMap).

expand_init_expr([#if_stmt{then = Then, else = Else} = E | Rest], NewAST, StructMap) ->
    expand_init_expr(Rest, [E#if_stmt{then = expand_init_expr(Then, [], StructMap), else = expand_init_expr(Else, [], StructMap)} | NewAST], StructMap);
expand_init_expr([#while_stmt{stmts = Expressions} = E | Rest], NewAST, StructMap) ->
    expand_init_expr(Rest, [E#while_stmt{stmts = expand_init_expr(Expressions, [], StructMap)} | NewAST], StructMap);
expand_init_expr([#op2_expr{} = Op | Rest], NewAST, StructMap) ->
    expand_init_expr(Rest, replace_init_ops(Op, StructMap) ++ NewAST, StructMap);
expand_init_expr([Any | Rest], NewAST, StructMap) ->
    expand_init_expr(Rest, [Any | NewAST], StructMap);
expand_init_expr([], NewAST, _) ->
    lists:reverse(NewAST).

replace_init_ops(#op2_expr{operator = assign, operand1 = Operand1, operand2 = #struct_init_expr{name = Name, line = Line, field_value_map = FieldValues}}, StructMap) ->
    case maps:find(Name, StructMap) of
        {ok, #struct{field_names = FieldNames, field_type_map = FieldTypes, field_default_value_map = FieldDefaults}} ->
            FieldValueMap = maps:merge(FieldDefaults, FieldValues),
            struct_init_to_ops(Operand1, FieldNames, FieldValueMap, FieldTypes, [], StructMap);
        error ->
            throw({Line, e_util:fmt("struct ~s is not found", [Name])})
    end;
replace_init_ops(#op2_expr{operator = assign, operand1 = Operand1, operand2 = #array_init_expr{elements = Elements, line = Line}}, StructMap) ->
    array_init_to_ops(Operand1, Elements, 0, Line, [], StructMap);
replace_init_ops(Any, _) ->
    [Any].

struct_init_to_ops(Target, [#variable_reference{line = Line, name = FieldName} = Field | Rest], FieldInitMap, FieldTypes, NewCode, StructMap) ->
    Operand2 = case maps:find(FieldName, FieldInitMap) of
                   {ok, InitOp} ->
                       InitOp;
                   error ->
                       default_value_of(maps:get(FieldName, FieldTypes), Line)
               end,
    NewAssign = #op2_expr{operator = assign, operand2 = Operand2, line = Line, operand1 = #op2_expr{operator = '.', operand1 = Target, operand2 = Field, line = Line}},
    Ops = replace_init_ops(NewAssign, StructMap),
    struct_init_to_ops(Target, Rest, FieldInitMap, FieldTypes, Ops ++ NewCode, StructMap);
struct_init_to_ops(_, [], _, _, NewCode, _) ->
    NewCode.

default_value_of(#array_type{elem_type = ElementType, length = Len}, Line) ->
    #array_init_expr{elements = lists:duplicate(Len, default_value_of(ElementType, Line)), line = Line};
default_value_of(#basic_type{class = struct, tag = Tag, p_depth = 0}, Line) ->
    #struct_init_expr{name = Tag, line = Line, field_value_map = #{}, field_names = []};
default_value_of(#basic_type{class = integer, p_depth = 0}, Line) ->
    {integer, Line, 0};
default_value_of(#basic_type{class = float, p_depth = 0}, Line) ->
    {float, Line, 0.0};
default_value_of(#basic_type{p_depth = PointerDepth}, Line) when PointerDepth > 0 ->
    {integer, Line, 0}.

array_init_to_ops(Target, [E | Rest], Cnt, Line, NewCode, StructMap) ->
    A = #op1_expr{operator = '@', line = Line, operand = Target},
    B = #op2_expr{operator = '+', operand2 = {integer, Line, Cnt}, line = Line, operand1 = A},
    C = #op1_expr{operator = '^', line = Line, operand = B},
    NewAssign = #op2_expr{operator = assign, operand1 = C, operand2 = E, line = Line},
    Ops = replace_init_ops(NewAssign, StructMap),
    array_init_to_ops(Target, Rest, Cnt + 1, Line, Ops ++ NewCode, StructMap);
array_init_to_ops(_, [], _, _, NewCode, _) ->
    NewCode.
