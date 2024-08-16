-module(e_ref_trans).
-export([varref_to_offset_in_ast/2, varref_to_offset_in_stmts/2]).
-include("./src/e_record_definition.hrl").

-type interface_context() :: {#e_vars{}, #{atom() => #e_fn_type{}}}.
-type context() :: {[#e_vars{}], #{atom() => #e_fn_type{}}}.

-spec varref_to_offset_in_ast(e_ast(), interface_context()) -> e_ast().
varref_to_offset_in_ast([#e_function{stmts = Stmts, vars = LocalVars} = Fn | Rest], {GlobalVars, FnTypeMap} = Ctx) ->
	Stmts1 = varref_to_offset(Stmts, {[LocalVars, GlobalVars], FnTypeMap}),
	[Fn#e_function{stmts = Stmts1} | varref_to_offset_in_ast(Rest, Ctx)];
varref_to_offset_in_ast([Any | Rest], Ctx) ->
	[Any | varref_to_offset_in_ast(Rest, Ctx)];
varref_to_offset_in_ast([], _) ->
	[].

-spec varref_to_offset_in_stmts([e_stmt()], interface_context()) -> [e_stmt()].
varref_to_offset_in_stmts(Stmts, {GlobalVars, FnTypeMap}) ->
	varref_to_offset(Stmts, {[GlobalVars], FnTypeMap}).

-spec varref_to_offset([e_stmt()], context()) -> [e_stmt()].
varref_to_offset(Stmts, Ctx) ->
	Stmts1 = e_util:expr_map(fun(E) -> varref_to_offset_in_expr(E, Ctx) end, Stmts),
	e_util:eliminate_pointer(Stmts1).

-spec varref_to_offset_in_expr(e_expr(), context()) -> e_expr().
varref_to_offset_in_expr(#e_varref{loc = Loc} = Varref, Ctx) ->
	{ok, {Tag, {Offset, Size}}} = find_name_in_vars_and_fn_map(Varref, Ctx),
	A = #e_op{tag = '+', data = [Varref#e_varref{name = Tag}, #e_integer{value = Offset, loc = Loc}], loc = Loc},
	#e_op{tag = '^', data = [A, #e_integer{value = Size, loc = Loc}], loc = Loc};
varref_to_offset_in_expr(#e_op{tag = {call, Callee}, data = Args} = Op, Ctx) ->
	Op#e_op{tag = {call, varref_to_offset_in_expr(Callee, Ctx)}, data = lists:map(fun(E) -> varref_to_offset_in_expr(E, Ctx) end, Args)};
varref_to_offset_in_expr(#e_op{data = Args} = Op, Ctx) ->
	Op#e_op{data = lists:map(fun(E) -> varref_to_offset_in_expr(E, Ctx) end, Args)};
varref_to_offset_in_expr(#e_type_convert{expr = Expr}, Ctx) ->
	varref_to_offset_in_expr(Expr, Ctx);
varref_to_offset_in_expr(Any, _) ->
	Any.

-spec find_name_in_vars_and_fn_map(#e_varref{}, context()) -> {ok, {atom(), var_offset()}} | notfound.
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
			{ok, {Name, {0, 0}}};
		error ->
			e_util:ethrow(Loc, "\"~s\" is not defined", [Name])
	end.

tag_trans(local) -> '<fp>';
tag_trans(global) -> '<gp>';
tag_trans(_) -> '<none>'.

