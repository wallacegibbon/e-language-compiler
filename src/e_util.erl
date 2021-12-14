-module(e_util).
-export([expr_to_str/1, expr_map/2, filter_var_refs_in_map/2, fmt/2, get_values_by_keys/2, names_of_var_defs/1, names_of_var_refs/1, value_in_list/2]).
-export([primitive_size_of/1, void_type/1, cut_extra/2, fill_offset/2, fill_to_pointer_width/2, make_function_and_struct_map_from_ast/1, assert/2]).

-include_lib("eunit/include/eunit.hrl").
-include("e_record_definition.hrl").

%% when do simple conversions, this function can be used to avoid boilerplate
%% code for if, while, return, call..., so you can concentrate on operand1, operand2...
-spec expr_map(fun ((e_expr()) -> e_expr()), [e_expr()]) -> e_expr().
expr_map(Fn, [#if_statement{condition = Cond, then = Then, else = Else} = If | Rest]) ->
    [If#if_statement{condition = Fn(Cond), then = expr_map(Fn, Then), else = expr_map(Fn, Else)} | expr_map(Fn, Rest)];
expr_map(Fn, [#while_statement{condition = Cond, statements = Expressions} = While | Rest]) ->
    [While#while_statement{condition = Fn(Cond), statements = expr_map(Fn, Expressions)} | expr_map(Fn, Rest)];
expr_map(Fn, [#call_expr{fn = Callee, args = Arguments} = FunctionCall | Rest]) ->
    [FunctionCall#call_expr{fn = Fn(Callee), args = expr_map(Fn, Arguments)} | expr_map(Fn, Rest)];
expr_map(Fn, [#return_statement{expression = ReturnExpression} = Return | Rest]) ->
    [Return#return_statement{expression = Fn(ReturnExpression)} | expr_map(Fn, Rest)];
expr_map(Fn, [Any | Rest]) ->
    [Fn(Any) | expr_map(Fn, Rest)];
expr_map(_, []) ->
    [].

-spec expr_to_str(e_expr()) -> string().
expr_to_str(#if_statement{condition = Cond, then = Then, else = Else}) ->
    io_lib:format("if (~s) ~s else ~s end", [expr_to_str(Cond), expr_to_str(Then), expr_to_str(Else)]);
expr_to_str(#while_statement{condition = Cond, statements = Expressions}) ->
    io_lib:format("while (~s) ~s end", [expr_to_str(Cond), expr_to_str(Expressions)]);
expr_to_str(#call_expr{fn = Callee, args = Arguments}) ->
    io_lib:format("(~s)(~s)", [expr_to_str(Callee), expr_to_str(Arguments)]);
expr_to_str(#return_statement{expression = ReturnExpression}) ->
    io_lib:format("return (~s)", [expr_to_str(ReturnExpression)]);
expr_to_str(#variable_reference{name = Name}) ->
    io_lib:format("~s", [expr_to_str(Name)]);
expr_to_str(#operator_expression2{operator = Operator, operand1 = Operand1, operand2 = Operand2}) ->
    io_lib:format("~s ~s ~s", [expr_to_str(Operand1), Operator, expr_to_str(Operand2)]);
expr_to_str(#operator_expression1{operator = Operator, operand = Operand}) ->
    io_lib:format("~s ~s", [expr_to_str(Operand), Operator]);
expr_to_str({ImmediateValue, _, Val}) when ImmediateValue =:= integer; ImmediateValue =:= float ->
    io_lib:format("~w", [Val]);
expr_to_str({ImmediateValue, _, Val}) when ImmediateValue =:= string ->
    Val;
expr_to_str(Any) ->
    Any.

-spec fmt(string(), [any()]) -> string().
fmt(FmtStr, Arguments) ->
    lists:flatten(io_lib:format(FmtStr, Arguments)).

-spec get_values_by_keys([atom()], #{atom() => any()}) -> [any()].
get_values_by_keys(Fields, Map) when is_map(Map) ->
    get_values_by_keys(Fields, Map, []).

-spec get_values_by_keys([atom()], #{atom() => any()}, [any()]) -> [any()].
get_values_by_keys([Field | Rest], Map, Result) ->
    get_values_by_keys(Rest, Map, [maps:get(Field, Map) | Result]);
get_values_by_keys([], _, Result) ->
    lists:reverse(Result).

-ifdef(EUNIT).

get_values_by_keys_test() ->
    ?assertEqual([3, 2], get_values_by_keys([a, b], #{c => 1, b => 2, a => 3})).

-endif.

-spec make_function_and_struct_map_from_ast(any()) -> {fn_type_map(), struct_type_map()}.
make_function_and_struct_map_from_ast(AST) ->
    {Functions, Structs} = lists:partition(fun (A) -> element(1, A) =:= function end, AST),
    %% FunctionTypeMap stores function type only
    FunctionTypeMap = maps:from_list(lists:map(fun (#function{name = Name} = Fn) -> {Name, Fn#function.type} end, Functions)),
    StructMap = maps:from_list(lists:map(fun (#struct{name = Name} = S) -> {Name, S} end, Structs)),
    {FunctionTypeMap, StructMap}.

%% address calculations
-spec fill_to_pointer_width(integer(), non_neg_integer()) -> integer().
fill_to_pointer_width(Num, PointerWidth) ->
    (Num + PointerWidth - 1) div PointerWidth * PointerWidth.

-spec fill_offset(integer(), non_neg_integer()) -> integer().
fill_offset(Offset, PointerWidth) ->
    (Offset + PointerWidth) div PointerWidth * PointerWidth.

-spec cut_extra(integer(), non_neg_integer()) -> integer().
cut_extra(Offset, PointerWidth) ->
    Offset div PointerWidth * PointerWidth.

-spec primitive_size_of(atom()) -> pointerSize | 1 | 2 | 4 | 8.
primitive_size_of(usize) -> pointerSize;
primitive_size_of(isize) -> pointerSize;
primitive_size_of(u64) -> 8;
primitive_size_of(i64) -> 8;
primitive_size_of(u32) -> 4;
primitive_size_of(i32) -> 4;
primitive_size_of(u16) -> 2;
primitive_size_of(i16) -> 2;
primitive_size_of(u8) -> 1;
primitive_size_of(i8) -> 1;
primitive_size_of(f64) -> 8;
primitive_size_of(f32) -> 4;
primitive_size_of(T) -> throw(fmt("size of ~p is not defined", [T])).

void_type(Line) ->
    #basic_type{class = void, tag = void, pdepth = 0, line = Line}.

-spec names_of_var_refs([#variable_reference{}]) -> [atom()].
names_of_var_refs(VariableReferenceList) ->
    lists:map(fun (#variable_reference{name = Name}) -> Name end, VariableReferenceList).

-spec names_of_var_defs([#variable_definition{}]) -> [atom()].
names_of_var_defs(VariableDefinitionList) ->
    lists:map(fun (#variable_definition{name = Name}) -> Name end, VariableDefinitionList).

-spec assert(boolean(), any()) -> ok.
assert(true, _) ->
    ok;
assert(false, Info) ->
    throw(Info).

-spec value_in_list(any(), [any()]) -> boolean().
value_in_list(Value, List) ->
    lists:any(fun (V) -> V =:= Value end, List).

%% filter_var_refs_in_map([#variable_reference{name = a}, #variable_reference{name = b}], #{a => 1})
%% > [#variable_reference{name = a}].
-spec filter_var_refs_in_map([#variable_reference{}], #{atom() := any()}) -> [#variable_reference{}].
filter_var_refs_in_map(VariableReferenceList, TargetMap) ->
    lists:filter(fun (#variable_reference{name = Name}) -> exist_in_map(Name, TargetMap) end, VariableReferenceList).

-ifdef(EUNIT).

filter_var_refs_in_map_test() ->
    A = filter_var_refs_in_map([#variable_reference{name = a}, #variable_reference{name = b}], #{a => 1}),
    ?assertEqual(A, [#variable_reference{name = a}]).

-endif.

-spec exist_in_map(atom(), #{atom() := any()}) -> boolean().
exist_in_map(KeyName, Map) ->
    case maps:find(KeyName, Map) of
        {ok, _} ->
            true;
        _ ->
            false
    end.
