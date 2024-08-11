-module(e_dump_c).
-export([generate_c_code/4]).
-include("e_record_definition.hrl").

-type context() :: {#{atom() := #e_fn_type{}}, #{atom() => #e_struct{}}, #{atom() => e_type()}}.

-spec generate_c_code(e_ast(), #{atom() => e_type()}, [e_stmt()], string()) -> ok.
generate_c_code(AST, GlobalVars, InitCode, OutputFile) ->
	{FnTypeMap, StructMap} = e_util:make_function_and_struct_map_from_ast(AST),
	Ctx = {FnTypeMap, StructMap, GlobalVars},
	AST2 = lists:map(fun(A) -> fix_function_for_c(A, Ctx) end, AST),
	InitCode2 = fix_exprs_for_c(InitCode, Ctx),
	% io:format(">>>~p~n", [AST2]),
	%% struct definition have to be before function declarations
	{StructAST, FnAST} = lists:partition(fun(A) -> element(1, A) =:= e_struct end, AST2),
	{StructStmts, []} = statements_to_str(StructAST, []),
	{FnStmts, FnDeclars} = statements_to_str(FnAST, InitCode2),
	VarStmts = var_map_to_str(GlobalVars),
	Code = lists:join("\n\n", [common_c_code(), StructStmts, VarStmts, FnDeclars, FnStmts]),
	ok = file:write_file(OutputFile, Code).

-spec fix_function_for_c(e_ast_elem(), context()) -> e_ast_elem().
fix_function_for_c(#e_function{} = Fn, {FnTypeMap, StructMap, GlobalVars}) ->
	#e_function{stmts = Stmts, vars = #e_vars{type_map = VarTypes}} = Fn,
	Fn#e_function{stmts = fix_exprs_for_c(Stmts, {FnTypeMap, StructMap, maps:merge(GlobalVars, VarTypes)})};
fix_function_for_c(Any, _) ->
	Any.

-spec fix_exprs_for_c([e_stmt()], context()) -> [e_stmt()].
fix_exprs_for_c(Stmts, Ctx) ->
	e_util:expr_map(fun(E) -> fix_expr_for_c(E, Ctx) end, Stmts).

-spec fix_expr_for_c(e_expr(), context()) -> e_expr().
fix_expr_for_c(#e_op{tag = '@', data = [Operand], line = L} = E, {FnTypeMap, StructMap, VarTypes} = Ctx) ->
	case e_type:type_of_node(Operand, {VarTypes, FnTypeMap, StructMap, #e_basic_type{}}) of
		#e_array_type{} ->
			#e_op{tag = '.', data = [fix_expr_for_c(Operand, Ctx), #e_varref{name = value, line = L}]};
		_ ->
			E
	end;
fix_expr_for_c(#e_op{data = Operands} = E, Ctx) ->
	E#e_op{data = lists:map(fun(D) -> fix_expr_for_c(D, Ctx) end, Operands)};
fix_expr_for_c(Any, _) ->
	Any.

-spec common_c_code() -> string().
common_c_code() ->
	"#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <stdint.h>\n"
	"typedef size_t usize;\ntypedef ssize_t isize;\n"
	"typedef uintptr_t uptr;\ntypedef intptr_t iptr;\n"
	"typedef unsigned char byte;\n"
	"typedef uint8_t u8;\ntypedef int8_t i8;\n"
	"typedef uint16_t u16;\ntypedef int16_t i16;\n"
	"typedef uint32_t u32;\ntypedef int32_t i32;\n"
	"typedef uint64_t u64;\ntypedef int64_t i64;\n"
	"typedef double f64;\ntypedef float f32;\n\n".

-spec statements_to_str(e_ast(), [e_stmt()]) -> {iolist(), iolist()}.
statements_to_str(Statements, InitCode) ->
	statements_to_str(Statements, InitCode, [], []).

statements_to_str([#e_function{name = Name} = Hd | Rest], InitCode, StmtStrs, FnDeclars) ->
	#e_function{param_names = ParamNames, type = FnType, vars = #e_vars{type_map = VarTypes}, stmts = Stmts} = Hd,
	PureParams = map_to_kv_list(ParamNames, maps:with(ParamNames, VarTypes)),
	PureVars = maps:without(ParamNames, VarTypes),
	Declars = function_to_str(Name, params_to_str(PureParams), FnType#e_fn_type.ret),
	Stmts2 =
		case Name =:= main of
			true -> InitCode ++ Stmts;
			false -> Stmts
		end,
	S = io_lib:format("~s~n{~n~s~n~n~s~n}~n~n", [Declars, var_map_to_str(PureVars), stmts_to_str(Stmts2)]),
	statements_to_str(Rest, InitCode, [S | StmtStrs], [Declars ++ ";\n" | FnDeclars]);
statements_to_str([#e_struct{} = Hd | Rest], InitCode, StmtStrs, FnDeclars) ->
	#e_struct{name = Name, fields = #e_vars{names = FieldNames, type_map = FieldTypes}} = Hd,
	FieldList = map_to_kv_list(FieldNames, FieldTypes),
	S = io_lib:format("struct ~s {~n~s~n};~n~n", [Name, var_list_to_str(FieldList)]),
	statements_to_str(Rest, InitCode, [S | StmtStrs], FnDeclars);
statements_to_str([], _, StmtStrs, FnDeclars) ->
	{lists:reverse(StmtStrs), lists:reverse(FnDeclars)}.

function_to_str(Name, ParamStr, #e_basic_type{p_depth = N} = RetType) when N > 0 ->
	return_type_to_str(RetType, io_lib:format("(*~s(~s))", [Name, ParamStr]));
function_to_str(Name, ParamStr, #e_fn_type{} = RetType) ->
	return_type_to_str(RetType, io_lib:format("(*~s(~s))", [Name, ParamStr]));
function_to_str(Name, ParamStr, RetType) ->
	type_to_c_str(RetType, io_lib:format("~s(~s)", [Name, ParamStr])).

params_to_str(NameTypePairs) ->
	L = lists:map(fun({N, T}) -> type_to_c_str(T, N) end, NameTypePairs),
	lists:join(",", L).

params_to_str_no_name(Types) ->
	L = lists:map(fun(T) -> type_to_c_str(T, "") end, Types),
	lists:join(",", L).

%% order is not necessary for vars
var_map_to_str(VarsMap) when is_map(VarsMap) ->
	L = lists:join(";\n", vars_to_str(maps:to_list(VarsMap), [])),
	lists:flatten(L, ";").

var_list_to_str(VarList) when is_list(VarList) ->
	lists:flatten(lists:join(";\n", vars_to_str(VarList, [])), ";").

vars_to_str([{Name, Type} | Rest], Strs) ->
	vars_to_str(Rest, [type_to_c_str(Type, Name) | Strs]);
vars_to_str([], Strs) ->
	lists:reverse(Strs).

map_to_kv_list(NameAtoms, ValueMap) ->
	lists:zip(NameAtoms, e_util:get_values_by_keys(NameAtoms, ValueMap)).

return_type_to_str(#e_fn_type{params = Params, ret = RetType}, NameParams) ->
	NewNameParams = io_lib:format("~s(~s)", [NameParams, params_to_str_no_name(Params)]),
	type_to_c_str(RetType, NewNameParams);
return_type_to_str(#e_basic_type{p_depth = N} = T, NameParams) when N > 0 ->
	type_to_c_str(T#e_basic_type{p_depth = N - 1}, NameParams).

%% convert type to C string
-spec type_to_c_str(e_type(), iolist()) -> iolist().
type_to_c_str(#e_array_type{length = Len, elem_type = Type}, VarName) ->
	io_lib:format("struct {~s value[~w];} ~s", [type_to_c_str(Type, ""), Len, VarName]);
type_to_c_str(#e_basic_type{class = Class, tag = Tag, p_depth = Depth}, VarName) when Depth > 0 ->
	io_lib:format("~s~s ~s", [type_tag_to_str(Class, Tag), lists:duplicate(Depth, "*"), VarName]);
type_to_c_str(#e_basic_type{class = Class, tag = Tag, p_depth = 0}, VarName) ->
	io_lib:format("~s ~s", [type_tag_to_str(Class, Tag), VarName]);
type_to_c_str(#e_fn_type{params = Params, ret = RetType}, VarName) ->
	NameParams = io_lib:format("(*~s)(~s)", [VarName, params_to_str_no_name(Params)]),
	type_to_c_str(RetType, NameParams).

type_tag_to_str(struct, Name) ->
	io_lib:format("struct ~s", [Name]);
type_tag_to_str(_, Name) ->
	atom_to_list(Name).

%% convert expression to C string
stmts_to_str(Stmts) ->
	[lists:join("\n", stmts_to_str(Stmts, []))].

stmts_to_str([Expr | Rest], ExprList) ->
	stmts_to_str(Rest, [stmt_to_str(Expr, $;) | ExprList]);
stmts_to_str([], ExprList) ->
	lists:reverse(ExprList).

-spec stmt_to_str(e_expr(), char()) -> iolist().
stmt_to_str(#e_if_stmt{condi = Condi, then = Then, else = Else}, _) ->
	io_lib:format("if (~s) {\n~s\n} else {\n~s}", [stmt_to_str(Condi, $\s), stmts_to_str(Then), stmts_to_str(Else)]);
stmt_to_str(#e_while_stmt{condi = Condi, stmts = Stmts}, _) ->
	io_lib:format("while (~s) {\n~s\n}\n", [stmt_to_str(Condi, $\s), stmts_to_str(Stmts)]);
stmt_to_str(#e_op{tag = {call, Fn}, data = Args}, EndChar) ->
	ArgStr = lists:join(",", lists:map(fun(E) -> stmt_to_str(E, $\s) end, Args)),
	io_lib:format("~s(~s)~c", [stmt_to_str(Fn, $\s), ArgStr, EndChar]);
stmt_to_str(#e_op{tag = Tag, data = [Op1, Op2]}, EndChar) ->
	io_lib:format("(~s ~s ~s)~c", [stmt_to_str(Op1, $\s), translate_op(Tag), stmt_to_str(Op2, $\s), EndChar]);
stmt_to_str(#e_op{tag = Tag, data = [Operand]}, EndChar) ->
	io_lib:format("(~s ~s)~c", [translate_op(Tag), stmt_to_str(Operand, $\s), EndChar]);
stmt_to_str(#e_return_stmt{expr = Expr}, EndChar) ->
	io_lib:format("return ~s~c", [stmt_to_str(Expr, $\s), EndChar]);
stmt_to_str(#e_goto_stmt{expr = Expr}, EndChar) ->
	io_lib:format("goto ~s~c", [stmt_to_str(Expr, $\s), EndChar]);
stmt_to_str(#e_goto_label{name = Name}, _) ->
	io_lib:format("~s:", [Name]);
stmt_to_str(#e_varref{name = Name}, EndChar) ->
	io_lib:format("~s~c", [Name, EndChar]);
stmt_to_str(#e_type_convert{expr = Expr, type = Type}, EndChar) ->
	io_lib:format("((~s) ~s)~c", [type_to_c_str(Type, ""), stmt_to_str(Expr, $\s), EndChar]);
stmt_to_str(#e_integer{value = Value}, EndChar) ->
	io_lib:format("~w~c", [Value, EndChar]);
stmt_to_str(#e_float{value = Value}, EndChar) ->
	io_lib:format("~w~c", [Value, EndChar]);
stmt_to_str(#e_string{value = S}, EndChar) ->
	io_lib:format("\"~s\"~c", [fix_special_chars(S), EndChar]).

-define(SPECIAL_CHARACTER_MAP, #{$\n => "\\n", $\r => "\\r", $\t => "\\t", $\f => "\\f", $\b => "\\b"}).

fix_special_chars(String) ->
	lists:map(fun(C) -> maps:get(C, ?SPECIAL_CHARACTER_MAP, C) end, String).

-spec translate_op(atom()) -> string() | atom().
translate_op(assign) -> "=";
translate_op('rem') -> "%";
translate_op('bxor') -> "^";
translate_op('bsr') -> ">>";
translate_op('bsl') -> "<<";
translate_op('band') -> "&";
translate_op('bor') -> "|";
translate_op('and') -> "&&";
translate_op('or') -> "||";
translate_op('@') -> "&";
translate_op('^') -> "*";
translate_op(Any) -> Any.

