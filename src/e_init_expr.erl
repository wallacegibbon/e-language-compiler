-module(e_init_expr).
-export([expand_in_function/2, expand_init_expr/2]).

-include("e_record_definition.hrl").

%% for now, array and struct init expression is only allowed in assignment
check_position(#operator_expression2{operator = assign, operand2 = Operand2}) when is_record(Operand2, struct_init_expr); is_record(Operand2, array_init_expr) ->
    ok;
check_position(#struct_init_expr{line = Line}) ->
    throw({Line, "struct init expression is only allowed in assignments"});
check_position(#array_init_expr{line = Line}) ->
    throw({Line, "array init expression is only allowed in assignments"});
check_position(_) ->
    ok.

expand_in_function([#function{statements = Expressions} = F | Rest], StructMap) ->
    e_util:expr_map(fun check_position/1, Expressions),
    [F#function{statements = expand_init_expr(Expressions, StructMap)} | expand_in_function(Rest, StructMap)];
expand_in_function([Any | Rest], StructMap) ->
    [Any | expand_in_function(Rest, StructMap)];
expand_in_function([], _) ->
    [].

expand_init_expr(Expressions, StructMap) ->
    expand_init_expr(Expressions, [], StructMap).

expand_init_expr([#if_statement{then = Then, else = Else} = E | Rest], NewAST, StructMap) ->
    expand_init_expr(Rest, [E#if_statement{then = expand_init_expr(Then, [], StructMap), else = expand_init_expr(Else, [], StructMap)} | NewAST], StructMap);
expand_init_expr([#while_statement{statements = Expressions} = E | Rest], NewAST, StructMap) ->
    expand_init_expr(Rest, [E#while_statement{statements = expand_init_expr(Expressions, [], StructMap)} | NewAST], StructMap);
expand_init_expr([#operator_expression2{} = Op | Rest], NewAST, StructMap) ->
    expand_init_expr(Rest, replace_init_ops(Op, StructMap) ++ NewAST, StructMap);
expand_init_expr([Any | Rest], NewAST, StructMap) ->
    expand_init_expr(Rest, [Any | NewAST], StructMap);
expand_init_expr([], NewAST, _) ->
    lists:reverse(NewAST).

replace_init_ops(#operator_expression2{operator = assign, operand1 = Operand1, operand2 = #struct_init_expr{name = Name, line = Line, fieldValueMap = FieldValues}}, StructMap) ->
    case maps:find(Name, StructMap) of
        {ok, #struct{fieldNames = FieldNames, fieldTypeMap = FieldTypes, fieldDefaultValueMap = FieldDefaults}} ->
            FieldValueMap = maps:merge(FieldDefaults, FieldValues),
            struct_init_to_ops(Operand1, FieldNames, FieldValueMap, FieldTypes, [], StructMap);
        error ->
            throw({Line, e_util:fmt("struct ~s is not found", [Name])})
    end;
replace_init_ops(#operator_expression2{operator = assign, operand1 = Operand1, operand2 = #array_init_expr{elements = Elements, line = Line}}, StructMap) ->
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
    NewAssign = #operator_expression2{operator = assign, operand2 = Operand2, line = Line, operand1 = #operator_expression2{operator = '.', operand1 = Target, operand2 = Field, line = Line}},
    Ops = replace_init_ops(NewAssign, StructMap),
    struct_init_to_ops(Target, Rest, FieldInitMap, FieldTypes, Ops ++ NewCode, StructMap);
struct_init_to_ops(_, [], _, _, NewCode, _) ->
    NewCode.

default_value_of(#array_type{elemtype = ElementType, length = Len}, Line) ->
    #array_init_expr{elements = lists:duplicate(Len, default_value_of(ElementType, Line)), line = Line};
default_value_of(#basic_type{class = struct, tag = Tag, pdepth = 0}, Line) ->
    #struct_init_expr{name = Tag, line = Line, fieldValueMap = #{}, fieldNames = []};
default_value_of(#basic_type{class = integer, pdepth = 0}, Line) ->
    {integer, Line, 0};
default_value_of(#basic_type{class = float, pdepth = 0}, Line) ->
    {float, Line, 0.0};
default_value_of(#basic_type{pdepth = PointerDepth}, Line) when PointerDepth > 0 ->
    {integer, Line, 0}.

array_init_to_ops(Target, [E | Rest], Cnt, Line, NewCode, StructMap) ->
    A = #operator_expression1{operator = '@', line = Line, operand = Target},
    B = #operator_expression2{operator = '+', operand2 = {integer, Line, Cnt}, line = Line, operand1 = A},
    C = #operator_expression1{operator = '^', line = Line, operand = B},
    NewAssign = #operator_expression2{operator = assign, operand1 = C, operand2 = E, line = Line},
    Ops = replace_init_ops(NewAssign, StructMap),
    array_init_to_ops(Target, Rest, Cnt + 1, Line, Ops ++ NewCode, StructMap);
array_init_to_ops(_, [], _, _, NewCode, _) ->
    NewCode.
