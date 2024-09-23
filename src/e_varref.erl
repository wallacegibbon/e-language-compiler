-module(e_varref).
-export([varref_to_offset_in_ast/2, varref_to_offset_in_stmts/2]).
-include("./src/e_record_definition.hrl").

-type interface_context() :: {#e_vars{}, #{atom() => #e_fn_type{}}}.
-type context() :: {[#e_vars{}], #{atom() => #e_fn_type{}}}.

-spec varref_to_offset_in_ast(e_ast(), interface_context()) -> e_ast().
varref_to_offset_in_ast([#e_function{stmts = Stmts, vars = LocalVars} = Fn | Rest], {GlobalVars, FnTypeMap} = Ctx) ->
	Stmts1 = varref_to_offset_in_stmts_inner(Stmts, {[LocalVars, GlobalVars], FnTypeMap}),
	[Fn#e_function{stmts = Stmts1} | varref_to_offset_in_ast(Rest, Ctx)];
varref_to_offset_in_ast([Any | Rest], Ctx) ->
	[Any | varref_to_offset_in_ast(Rest, Ctx)];
varref_to_offset_in_ast([], _) ->
	[].

-spec varref_to_offset_in_stmts([e_stmt()], interface_context()) -> [e_stmt()].
varref_to_offset_in_stmts(Stmts, {GlobalVars, FnTypeMap}) ->
	varref_to_offset_in_stmts_inner(Stmts, {[GlobalVars], FnTypeMap}).

-spec varref_to_offset_in_stmts_inner([e_stmt()], context()) -> [e_stmt()].
varref_to_offset_in_stmts_inner(Stmts, Ctx) ->
	Stmts1 = e_util:expr_map(fun(E) -> varref_to_offset(E, Ctx) end, Stmts),
	e_util:eliminate_pointer(Stmts1).

-spec varref_to_offset(e_expr(), context()) -> e_expr().
varref_to_offset(#e_varref{} = Varref, Ctx) ->
	case find_name_in_vars_and_fn_map(Varref, Ctx) of
		{ok, {Tag, VarOffset}} ->
			varref_to_op(Varref, Tag, VarOffset);
		{ok, _} ->
			Varref
	end;
varref_to_offset(?CALL(Callee, Args) = Op, Ctx) ->
	Op?CALL(varref_to_offset(Callee, Ctx), lists:map(fun(E) -> varref_to_offset(E, Ctx) end, Args));
varref_to_offset(#e_op{data = Operands} = Op, Ctx) ->
	Op#e_op{data = lists:map(fun(E) -> varref_to_offset(E, Ctx) end, Operands)};
varref_to_offset(#e_type_convert{expr = Expr}, Ctx) ->
	varref_to_offset(Expr, Ctx);
varref_to_offset(Any, _) ->
	Any.

-spec varref_to_op(#e_varref{}, atom(), e_var_offset()) -> #e_op{}.
varref_to_op(#e_varref{loc = Loc} = Varref, Tag, {Offset, Size}) ->
	?OP2('^', ?OP2('+', Varref#e_varref{name = Tag}, ?I(Offset, Loc), Loc), ?I(Size, Loc), Loc).

-spec find_name_in_vars_and_fn_map(#e_varref{}, context()) -> {ok, {atom(), e_var_offset()}} | {ok, atom()}.
find_name_in_vars_and_fn_map(Varref, {VarsList, FnTypeMap}) ->
	case find_name_in_vars(Varref, VarsList) of
		{ok, _} = R ->
			R;
		notfound ->
			find_name_in_fn_map(Varref, FnTypeMap)
	end.

find_name_in_vars(#e_varref{name = Name} = Varref, [#e_vars{offset_map = Map, tag = Tag} | RestVars]) ->
	case maps:find(Name, Map) of
		{ok, OffsetAndSize} ->
			{ok, {tag_trans(Tag), OffsetAndSize}};
		error ->
			find_name_in_vars(Varref, RestVars)
	end;
find_name_in_vars(_, []) ->
	notfound.

find_name_in_fn_map(#e_varref{name = Name, loc = Loc}, FnTypeMap) ->
	case maps:find(Name, FnTypeMap) of
		{ok, _} ->
			{ok, Name};
		error ->
			e_util:ethrow(Loc, "\"~s\" is not defined", [Name])
	end.

tag_trans(global) -> 'gp';
tag_trans(local) -> 'fp'.

