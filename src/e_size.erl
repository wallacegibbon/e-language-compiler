-module(e_size).
-export([expand_kw_in_ast/2, expand_kw_in_stmts/2, fill_offsets_in_ast/2, fill_offsets_in_vars/2]).
-export([size_of/2, align_of/2]).
-include("e_record_definition.hrl").
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec expand_kw_in_ast(e_ast(), e_compile_context:context()) -> e_ast().
expand_kw_in_ast([#e_function{stmts = Stmts} = Fn | Rest], Ctx) ->
    [Fn#e_function{stmts = expand_kw_in_stmts(Stmts, Ctx)} | expand_kw_in_ast(Rest, Ctx)];
expand_kw_in_ast([#e_struct{default_value_map = FieldDefaults} = S | Rest], Ctx) ->
    [S#e_struct{default_value_map = expand_kw_in_map(FieldDefaults, Ctx)} | expand_kw_in_ast(Rest, Ctx)];
expand_kw_in_ast([], _) ->
    [].

-spec expand_kw_in_stmts([e_stmt()], e_compile_context:context()) -> [e_stmt()].
expand_kw_in_stmts(Stmts, Ctx) ->
    e_util:expr_map(fun(E) -> expand_kw(E, Ctx) end, Stmts).

-spec expand_kw(e_expr(), e_compile_context:context()) -> e_expr().
expand_kw(#e_op{tag = {sizeof, T}, loc = Loc}, Ctx) ->
    ?I(size_of(T, Ctx), Loc);
expand_kw(#e_op{tag = {alignof, T}, loc = Loc}, Ctx) ->
    ?I(align_of(T, Ctx), Loc);
expand_kw(?CALL(Fn, Args) = E, Ctx) ->
    E?CALL(expand_kw(Fn, Ctx), [expand_kw(O, Ctx) || O <- Args]);
expand_kw(#e_op{data = Data} = E, Ctx) ->
    E#e_op{data = [expand_kw(O, Ctx) || O <- Data]};
expand_kw(#e_type_convert{expr = Expr} = C, Ctx) ->
    C#e_type_convert{expr = expand_kw(Expr, Ctx)};
expand_kw(#e_struct_init_expr{field_value_map = ExprMap} = S, Ctx) ->
    S#e_struct_init_expr{field_value_map = expand_kw_in_map(ExprMap, Ctx)};
expand_kw(#e_array_init_expr{elements = Elements} = A, Ctx) ->
    A#e_array_init_expr{elements = [expand_kw(E, Ctx) || E <- Elements]};
expand_kw(Any, _) ->
    Any.

expand_kw_in_map(Map, Ctx) ->
    #{K => expand_kw(V, Ctx) || K := V <- Map}.

-spec fill_offsets_in_ast(e_ast(), e_compile_context:context()) -> e_ast().
fill_offsets_in_ast([#e_function{vars = Old, param_names = ParamNames} = Fn | Rest], Ctx) ->
    Vars0 = fill_offsets_in_vars(Old, Ctx),
    Vars1 = shift_offset_params(Vars0, ParamNames),
    [Fn#e_function{vars = Vars1} | fill_offsets_in_ast(Rest, Ctx)];
fill_offsets_in_ast([#e_struct{name = Name, fields = Old} = S | Rest],
		    #{struct_map := StructMap} = Ctx) ->
    FilledS = S#e_struct{fields = fill_offsets_in_vars(Old, Ctx)},
    %% StructMap in Ctx got updated to avoid some duplicated calculations.
    [FilledS | fill_offsets_in_ast(Rest, Ctx#{struct_map := StructMap#{Name := FilledS}})];
fill_offsets_in_ast([Any | Rest], Ctx) ->
    [Any | fill_offsets_in_ast(Rest, Ctx)];
fill_offsets_in_ast([], _) ->
    [].

-spec fill_offsets_in_vars(#e_vars{}, e_compile_context:context()) -> #e_vars{}.
fill_offsets_in_vars(#e_vars{} = Vars, Ctx) ->
    #{size := Size, align := Align, offset_map := OffsetMap} = size_and_offsets_of_vars(Vars, Ctx),
    Vars#e_vars{offset_map = OffsetMap, size = Size, align = Align}.

-spec size_of_struct(#e_struct{}, e_compile_context:context()) -> non_neg_integer().
size_of_struct(#e_struct{fields = #e_vars{size = Size}}, _) when Size > 0 ->
    Size;
size_of_struct(#e_struct{fields = Fields}, Ctx) ->
    #{size := Size} = size_and_offsets_of_vars(Fields, Ctx),
    Size.

-spec align_of_struct(#e_struct{}, e_compile_context:context()) -> non_neg_integer().
align_of_struct(#e_struct{fields = #e_vars{align = Align}}, _) when Align > 0 ->
    Align;
align_of_struct(#e_struct{fields = #e_vars{type_map = TypeMap}}, Ctx) ->
    maps:fold(fun(_, Type, Align) -> max(align_of(Type, Ctx), Align) end, 0, TypeMap).


-type size_align_data() :: #{
			     size := non_neg_integer(),
			     align := non_neg_integer(),
			     offset_map := #{atom() := e_var_offset()}
			    }.

-spec size_and_offsets_of_vars(#e_vars{}, e_compile_context:context()) -> size_align_data().
size_and_offsets_of_vars(#e_vars{names = Names, type_map = TypeMap}, Ctx) ->
    TypeList = e_util:get_kvpair_by_keys(Names, TypeMap),
    size_and_offsets(TypeList, #{size => 0, align => 1, offset_map => #{}}, Ctx).

-spec size_and_offsets([{atom(), e_type()}], size_align_data(), e_compile_context:context()) ->
    size_align_data().
size_and_offsets([{Name, Type} | Rest],
		 #{size := CurrentOffset, align := MaxAlign, offset_map := OffsetMap},
		 Ctx) ->
    FieldAlign = align_of(Type, Ctx),
    Offset = e_util:align_to(CurrentOffset, FieldAlign),
    FieldSize = size_of(Type, Ctx),
    OffsetMapNew = OffsetMap#{Name => {Offset, FieldSize}},
    NextIn = #{size => Offset + FieldSize,
	       align => max(MaxAlign, FieldAlign),
	       offset_map => OffsetMapNew},
    size_and_offsets(Rest, NextIn, Ctx);
size_and_offsets([], #{size := CurrentOffset, align := MaxAlign} = Result, _) ->
    Result#{size := e_util:align_to(CurrentOffset, MaxAlign)}.

%% Usually, for 32-bit MCU, only 32-bit float is supported. For 64-bit CPU, 64-bit float is supported.
%% So we can assume that size of float is same as sizeof word.

-spec size_of(e_type(), e_compile_context:context()) -> non_neg_integer().
size_of(#e_array_type{elem_type = T, length = Len}, Ctx) ->
    size_of(T, Ctx) * Len;
size_of(#e_basic_type{p_depth = N}, #{wordsize := WordSize}) when N > 0 ->
    WordSize;
size_of(#e_basic_type{class = struct, p_depth = 0} = S, #{struct_map := StructMap} = Ctx) ->
    size_of_struct(e_util:get_struct_from_type(S, StructMap), Ctx);
size_of(#e_fn_type{}, #{wordsize := WordSize}) ->
    WordSize;
size_of(#e_basic_type{class = float}, #{wordsize := WordSize}) ->
    WordSize;
size_of(#e_basic_type{class = integer, tag = word}, #{wordsize := WordSize}) ->
    WordSize;
size_of(#e_basic_type{class = integer, tag = byte}, _) ->
    1;
size_of(Any, _) ->
    e_util:ethrow(element(2, Any), "invalid type \"~w\"", [Any]).

-spec align_of(e_type(), e_compile_context:context()) -> non_neg_integer().
align_of(#e_array_type{elem_type = T}, Ctx) ->
    align_of(T, Ctx);
align_of(#e_basic_type{class = struct, p_depth = 0} = S, #{struct_map := StructMap} = Ctx) ->
    align_of_struct(e_util:get_struct_from_type(S, StructMap), Ctx);
align_of(Type, Ctx) ->
    size_of(Type, Ctx).

local_var_offset(#e_vars{names = Names, offset_map = OffsetMap, size = Size}, ParamNames) ->
    case find_0th(ParamNames, Names) of
	{ok, N} ->
	    #{N := {Offset, _}} = OffsetMap,
	    Offset;
	_ ->
	    Size
    end.

-spec shift_offset_params(#e_vars{}, [atom()]) -> #e_vars{}.
shift_offset_params(Vars, ParamNames) ->
    e_var:shift_offset(Vars, local_var_offset(Vars, ParamNames)).

-ifdef(EUNIT).
local_var_offset_test() ->
    V0 = #e_vars{names = [a, b, c],
		 offset_map = #{a => {0, 4}, b => {4, 4}, c => {8, 1}},
		 size = 12,
		 shifted_size = 12},
    ?assertEqual(8, local_var_offset(V0, [a, b])).

shift_offset_params_test() ->
    V0 = #e_vars{names = [a, b, c],
		 offset_map = #{a => {0, 4}, b => {4, 4}, c => {8, 1}},
		 size = 12,
		 shifted_size = 12},
    V1 = shift_offset_params(V0, [a, b]),
    ?assertMatch(#e_vars{offset_map = #{a := {-8, 4}, b := {-4, 4}},
			 size = 12,
			 shifted_size = 4},
		 V1).

-endif.

find_0th([N | Rest], [N | Names]) ->
    find_0th(Rest, Names);
find_0th([], [N | _]) ->
    {ok, N};
find_0th([], []) ->
    ok;
find_0th(_, _) ->
    throw("invalid args and vars").
