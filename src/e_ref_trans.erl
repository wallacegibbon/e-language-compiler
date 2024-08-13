-module(e_ref_trans).
-export([varref_to_offset_in_ast/2, varref_to_offset_in_stmts/2]).
-include("./src/e_record_definition.hrl").

-spec varref_to_offset_in_ast(e_ast(), #e_vars{}) -> e_ast().
varref_to_offset_in_ast([#e_function{stmts = Stmts, vars = LocalVars} = Fn | Rest], GlobalVars) ->
	Stmts1 = varref_to_offset(Stmts, [LocalVars, GlobalVars]),
	[Fn#e_function{stmts = Stmts1} | varref_to_offset_in_ast(Rest, GlobalVars)];
varref_to_offset_in_ast([Any | Rest], Ctx) ->
	[Any | varref_to_offset_in_ast(Rest, Ctx)];
varref_to_offset_in_ast([], _) ->
	[].

-spec varref_to_offset_in_stmts([e_stmt()], #e_vars{}) -> [e_stmt()].
varref_to_offset_in_stmts(Stmts, GlobalVars) ->
	varref_to_offset(Stmts, [GlobalVars]).

-spec varref_to_offset([e_stmt()], [#e_vars{}]) -> [e_stmt()].
varref_to_offset(Stmts, VarsList) ->
	Stmts1 = e_util:expr_map(fun(E) -> varref_to_offset_in_expr(E, VarsList) end, Stmts),
	e_util:eliminate_pointer(Stmts1).

-spec varref_to_offset_in_expr(e_expr(), [#e_vars{}]) -> e_expr().
varref_to_offset_in_expr(#e_varref{line = Line} = Varref, VarsList) ->
	{Tag, Offset} = find_name_in_vars(Varref, VarsList),
	A = #e_op{tag = '+', data = [Varref#e_varref{name = Tag}, #e_integer{value = Offset, line = Line}], line = Line},
	#e_op{tag = '^', data = [A], line = Line};
varref_to_offset_in_expr(#e_op{data = Args} = Op, VarsList) ->
	Op#e_op{data = lists:map(fun(E) -> varref_to_offset_in_expr(E, VarsList) end, Args)};
varref_to_offset_in_expr(Any, _) ->
	Any.

find_name_in_vars(#e_varref{name = Name} = Varref, [#e_vars{offset_map = Map, tag = Tag} | RestVars]) ->
	case maps:find(Name, Map) of
		{ok, N} ->
			{tag_trans(Tag), N};
		error ->
			find_name_in_vars(Varref, RestVars)
	end;
find_name_in_vars(#e_varref{name = Name, line = Line}, []) ->
	e_util:ethrow(Line, "\"~s\" is not defined", [Name]).

tag_trans(local) -> fp;
tag_trans(global) -> gp;
tag_trans(_) -> none.

