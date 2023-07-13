-module(c_codegen).
-export([generate_c_code/4]).

-include("e_record_definition.hrl").

-type context() :: {fn_type_map(), struct_type_map(), var_type_map()}.

-spec generate_c_code(e_ast(), var_type_map(), [e_stmt()], string()) -> ok.
generate_c_code(AST, GlobalVars, InitCode, OutputFile) ->
	{FnTypeMap, StructMap} = e_util:make_function_and_struct_map_from_ast(AST),

	Ctx = {FnTypeMap, StructMap, GlobalVars},
	AST2 = lists:map(fun (A) -> fix_function_for_c(A, Ctx) end, AST),
	InitCode2 = fix_exprs_for_c(InitCode, Ctx),

	% io:format(">>>~p~n", [AST2]),
	%% struct definition have to be before function declarations
	{StructAST, FnAST} = lists:partition(fun (A) -> element(1, A) =:= struct end, AST2),
	{StructStmts, []} = statements_to_str(StructAST, []),
	{FnStmts, FnDeclars} = statements_to_str(FnAST, InitCode2),
	VarStmts = var_map_to_str(GlobalVars),
	Code = lists:join("\n\n", [common_c_code(), StructStmts, VarStmts, FnDeclars, FnStmts]),
	ok = file:write_file(OutputFile, Code).

-spec fix_function_for_c(e_ast_elem(), context()) -> e_ast_elem().
fix_function_for_c(#function{} = F, {FnTypeMap, StructMap, GlobalVars}) ->
	#function{stmts = Exprs, var_type_map = VarTypes} = F,
	F#function{stmts = fix_exprs_for_c(Exprs, {FnTypeMap, StructMap, maps:merge(GlobalVars, VarTypes)})};
fix_function_for_c(Any, _) ->
	Any.

-spec fix_exprs_for_c([e_stmt()], context()) -> [e_stmt()].
fix_exprs_for_c(Exprs, Ctx) ->
	e_util:expr_map(fun (E) -> fix_expr_for_c(E, Ctx) end, Exprs).

-spec fix_expr_for_c(e_expr(), context()) -> e_expr().
fix_expr_for_c(#e_expr{tag = '@', data = [Operand], line = Line} = E, {FnTypeMap, StructMap, VarTypes} = Ctx) ->
	case e_type:type_of_node(Operand, {VarTypes, FnTypeMap, StructMap, #basic_type{}}) of
	#array_type{} ->
		#e_expr{tag = '.', data = [fix_expr_for_c(Operand, Ctx), #var_ref{name = value, line = Line}]};
	_ ->
		E
	end;
fix_expr_for_c(#e_expr{data = Operands} = E, Ctx) ->
	E#e_expr{data = lists:map(fun(D) -> fix_expr_for_c(D, Ctx) end, Operands)};
fix_expr_for_c(Any, _) ->
	Any.

-spec common_c_code() -> string().
common_c_code() ->
	"#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
	"typedef unsigned int usize;\ntypedef int isize;\n"
	"typedef unsigned char u8;\ntypedef char i8;\n"
	"typedef unsigned short u16;\ntypedef short i16;\n"
	"typedef unsigned int u32;\ntypedef int i32;\n"
	"typedef unsigned long u64;\ntypedef long i64;\n"
	"typedef double f64;\ntypedef float f32;\n\n".

-spec statements_to_str(e_ast(), [e_stmt()]) -> {iolist(), iolist()}.
statements_to_str(Statements, InitCode) ->
	statements_to_str(Statements, InitCode, [], []).

statements_to_str([#function{} = Hd | Rest], InitCode, StmtStrs, FnDeclars) ->
	#function{name = Name, param_names = ParamNames, type = FnType, var_type_map = VarTypes, stmts = Exprs} = Hd,
	ParamNameAtoms = names_from_varrefs(ParamNames),
	PureParams = map_to_kv_list(ParamNameAtoms, maps:with(ParamNameAtoms, VarTypes)),
	PureVars = maps:without(ParamNameAtoms, VarTypes),
	Declars = function_declaration_to_str(Name, function_params_to_str(PureParams), FnType#fn_type.ret),
	Exprs2 =
		case Name =:= main of
			true -> InitCode ++ Exprs;
			false -> Exprs
		end,
	S = io_lib:format("~s~n{~n~s~n~n~s~n}~n~n", [Declars, var_map_to_str(PureVars), exprs_to_str(Exprs2)]),
	statements_to_str(Rest, InitCode, [S | StmtStrs], [Declars ++ ";\n" | FnDeclars]);
statements_to_str([#struct{} = Hd | Rest], InitCode, StmtStrs, FnDeclars) ->
	#struct{name = Name, field_type_map = FieldTypes, field_names = FieldNames} = Hd,
	FieldList = map_to_kv_list(names_from_varrefs(FieldNames), FieldTypes),
	S = io_lib:format("struct ~s {~n~s~n};~n~n", [Name, var_list_to_str(FieldList)]),
	statements_to_str(Rest, InitCode, [S | StmtStrs], FnDeclars);
statements_to_str([], _, StmtStrs, FnDeclars) ->
	{lists:reverse(StmtStrs), lists:reverse(FnDeclars)}.

function_declaration_to_str(Name, ParamStr, #basic_type{p_depth = N} = RetType) when N > 0 ->
	return_type_to_str(RetType, io_lib:format("(*~s(~s))", [Name, ParamStr]));
function_declaration_to_str(Name, ParamStr, #fn_type{} = RetType) ->
	return_type_to_str(RetType, io_lib:format("(*~s(~s))", [Name, ParamStr]));
function_declaration_to_str(Name, ParamStr, RetType) ->
	type_to_c_str(RetType, io_lib:format("~s(~s)", [Name, ParamStr])).

function_params_to_str(NameTypePairs) ->
	L = lists:map(fun ({N, T}) -> type_to_c_str(T, N) end, NameTypePairs),
	lists:join(",", L).

function_params_to_str_no_name(Types) ->
	L = lists:map(fun (T) -> type_to_c_str(T, "") end, Types),
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

names_from_varrefs(VarrefList) ->
	lists:map(fun (#var_ref{name = N}) -> N end, VarrefList).

map_to_kv_list(NameAtoms, ValueMap) ->
	lists:zip(NameAtoms, e_util:get_values_by_keys(NameAtoms, ValueMap)).

return_type_to_str(#fn_type{params = Params, ret = RetType}, NameParams) ->
	NewNameParams = io_lib:format("~s(~s)", [NameParams, function_params_to_str_no_name(Params)]),
	type_to_c_str(RetType, NewNameParams);
return_type_to_str(#basic_type{p_depth = N} = T, NameParams) when N > 0 ->
	type_to_c_str(T#basic_type{p_depth = N - 1}, NameParams).

%% convert type to C string
-spec type_to_c_str(e_type(), iolist()) -> iolist().
type_to_c_str(#array_type{length = Len, elem_type = Type}, VarName) ->
	io_lib:format("struct {~s value[~w];} ~s", [type_to_c_str(Type, ""), Len, VarName]);
type_to_c_str(#basic_type{class = Class, tag = Tag, p_depth = Depth}, VarName) when Depth > 0 ->
	io_lib:format("~s~s ~s", [type_tag_to_str(Class, Tag), lists:duplicate(Depth, "*"), VarName]);
type_to_c_str(#basic_type{class = Class, tag = Tag, p_depth = 0}, VarName) ->
	io_lib:format("~s ~s", [type_tag_to_str(Class, Tag), VarName]);
type_to_c_str(#fn_type{params = Params, ret = RetType}, VarName) ->
	NameParams = io_lib:format("(*~s)(~s)", [VarName, function_params_to_str_no_name(Params)]),
	type_to_c_str(RetType, NameParams).

type_tag_to_str(struct, Name) ->
	io_lib:format("struct ~s", [Name]);
type_tag_to_str(_, Name) ->
	atom_to_list(Name).

%% convert expression to C string
exprs_to_str(Exprs) ->
	[lists:join("\n", exprs_to_str(Exprs, []))].

exprs_to_str([Expr | Rest], ExprList) ->
	exprs_to_str(Rest, [expr_to_str(Expr, $;) | ExprList]);
exprs_to_str([], ExprList) ->
	lists:reverse(ExprList).

-spec expr_to_str(e_expr(), char()) -> iolist().
expr_to_str(#if_stmt{condi = Condi, then = Then, else = Else}, _) ->
	io_lib:format("if (~s) {\n~s\n} else {\n~s}", [expr_to_str(Condi, $\s), exprs_to_str(Then), exprs_to_str(Else)]);
expr_to_str(#while_stmt{condi = Condi, stmts = Exprs}, _) ->
	io_lib:format("while (~s) {\n~s\n}\n", [expr_to_str(Condi, $\s), exprs_to_str(Exprs)]);
expr_to_str(#e_expr{tag = {call, Fn}, data = Args}, EndChar) ->
	ArgStr = lists:join(lists:map(fun (E) -> expr_to_str(E, $\s) end, Args), ","),
	io_lib:format("~s(~s)~c", [expr_to_str(Fn, $\s), ArgStr, EndChar]);
expr_to_str(#e_expr{tag = Tag, data = [Op1, Op2]}, EndChar) ->
	io_lib:format("(~s ~s ~s)~c", [expr_to_str(Op1, $\s), translate_op(Tag), expr_to_str(Op2, $\s), EndChar]);
expr_to_str(#e_expr{tag = Tag, data = [Operand]}, EndChar) ->
	io_lib:format("(~s ~s)~c", [translate_op(Tag), expr_to_str(Operand, $\s), EndChar]);
expr_to_str(#return_stmt{expr = Expr}, EndChar) ->
	io_lib:format("return ~s~c", [expr_to_str(Expr, $\s), EndChar]);
expr_to_str(#goto_stmt{expr = Expr}, EndChar) ->
	io_lib:format("goto ~s~c", [expr_to_str(Expr, $\s), EndChar]);
expr_to_str(#goto_label{name = Name}, _) ->
	io_lib:format("~s:", [Name]);
expr_to_str(#var_ref{name = Name}, EndChar) ->
	io_lib:format("~s~c", [Name, EndChar]);
expr_to_str(#type_convert{expr = Expr, type = Type}, EndChar) ->
	io_lib:format("((~s) ~s)~c", [type_to_c_str(Type, ""), expr_to_str(Expr, $\s), EndChar]);
expr_to_str({T, _Line, Value}, EndChar) when T =:= integer; T =:= float ->
	io_lib:format("~w~c", [Value, EndChar]);
expr_to_str({string, _Line, S}, EndChar) ->
	io_lib:format("\"~s\"~c", [fix_special_chars(S), EndChar]).

-define(SPECIAL_CHARACTER_MAP, #{$\n => "\\n", $\r => "\\r", $\t => "\\t", $\f => "\\f", $\b => "\\b"}).

fix_special_chars(String) ->
	lists:map(fun (C) -> maps:get(C, ?SPECIAL_CHARACTER_MAP, C) end, String).

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

