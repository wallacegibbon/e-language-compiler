-module(ecompiler_genc).

-export([generate_ccode/4]).

-include("./ecompiler_frame.hrl").

generate_ccode(Ast, Vars, _InitCode, OutputFile) ->
    {Fns, Others} = lists:partition(fun(A) ->
					    element(1, A) =:= function_1
				    end, Ast),
    {FnStatements, FnDeclars} = statements_tostr(Fns),
    {OtherStatements, []} = statements_tostr(Others),
    VarStatements = vars_to_str(Vars),
    Code = [common_code(), OtherStatements, "\n\n", VarStatements, "\n\n",
	    FnDeclars, "\n\n", FnStatements],
    file:write_file(OutputFile, Code).

common_code() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
    "typedef unsigned char u8;\ntypedef char i8;\n"
    "typedef unsigned short u16;\ntypedef short i16;\n"
    "typedef unsigned int u32;\ntypedef int i32;\n"
    "typedef unsigned long u64;\ntypedef long i64;\n"
    "typedef double f64;\ntypedef float f32;\n\n".

statements_tostr(Statements) ->
    statements_tostr(Statements, [], []).

statements_tostr([#function_1{name=Name, params=Params, vars=Vars,
			      ret=Rettype, exprs=Exprs} | Rest],
		 StatementStrs, FnDeclars) ->
    Declar = fn_declar_str(Name, Params, Vars, Rettype),
    S = io_lib:format("~s~n{~n~s~n~n~s~n}~n~n",
		      [Declar, vars_to_str(maps:without(Params, Vars)),
		       exprs_tostr(Exprs)]),
    statements_tostr(Rest, [S | StatementStrs], [Declar ++ ";" | FnDeclars]);
statements_tostr([#struct_1{name=Name, fields=Fields} | Rest],
		 StatementStrs, FnDeclars) ->
    S = io_lib:format("typedef struct {~n~s~n} ~s;~n~n",
		      [vars_to_str(Fields), Name]),
    statements_tostr(Rest, [S | StatementStrs], FnDeclars);
statements_tostr([], StatementStrs, FnDeclars) ->
    {lists:reverse(StatementStrs), lists:reverse(FnDeclars)}.

fn_declar_str(Name, ParamNames, Vars, Rettype) ->
    io_lib:format("~s ~s(~s)", [type_tostr(Rettype), Name,
				params_to_str(ParamNames, Vars)]).

params_to_str(ParamNames, Vars) ->
    params_to_str(ParamNames, Vars, []).

params_to_str([ParamName | RestParamNames], Vars, FmtParams) ->
    Pstr = io_lib:format("~s ~s", [type_tostr(maps:get(ParamName, Vars)),
				   ParamName]),
    params_to_str(RestParamNames, Vars, [Pstr | FmtParams]);
params_to_str([], _, FmtParams) ->
    lists:join(",", lists:reverse(FmtParams)).

vars_to_str(VarsMap) ->
    lists:flatten(lists:join(";\n", vars_to_str(maps:to_list(VarsMap), [])),
		  ";").

vars_to_str([{Name, Type} | Rest], Strs) ->
    vars_to_str(Rest, [io_lib:format("~s ~s", [type_tostr(Type),
					       Name]) | Strs]);
vars_to_str([], Strs) ->
    lists:reverse(Strs).

%% convert type to C string
type_tostr(#box_type{size=Size, elemtype=ElementType}) ->
    io_lib:format("struct {~s val[~w];}", [type_tostr(ElementType), Size]);
type_tostr(#basic_type{type={Typeanno, Depth}}) ->
    io_lib:format("~s ~s", [Typeanno, lists:duplicate(Depth, "*")]);
type_tostr(#basic_type{type=Typeanno}) ->
    io_lib:format("~s", [Typeanno]).

%% convert expression to C string
exprs_tostr(Exprs) ->
    [lists:join(";\n", exprs_tostr(Exprs, [])), ";"].

exprs_tostr([Expr | Rest], ExprList) ->
    exprs_tostr(Rest, [expr_tostr(Expr) | ExprList]);
exprs_tostr([], ExprList) ->
    lists:reverse(ExprList).

expr_tostr(#if_expr{condition=Condition, then=Then, else=Else}) ->
    io_lib:format("if (~s) {\n~s\n} else {\n~s}",
		  [expr_tostr(Condition), exprs_tostr(Then),
		   exprs_tostr(Else)]);
expr_tostr(#while_expr{condition=Condition, exprs=Exprs}) ->
    io_lib:format("while (~s) {\n~s\n}\n",
		  [expr_tostr(Condition), exprs_tostr(Exprs)]);
expr_tostr(#op2{operator=Operator, op1=Op1, op2=Op2}) ->
    io_lib:format("(~s ~s ~s)", [expr_tostr(Op1), translate_operator(Operator),
				 expr_tostr(Op2)]);
expr_tostr(#op1{operator=Operator, operand=Operand}) ->
    io_lib:format("(~s ~s)", [translate_operator(Operator),
			      expr_tostr(Operand)]);
expr_tostr(#call{name=Name, args=Args}) ->
    io_lib:format("~s(~s)", [Name, lists:join(",", lists:map(fun expr_tostr/1,
							     Args))]);
expr_tostr(#return{expr=Expr}) ->
    io_lib:format("return ~s", [expr_tostr(Expr)]);
expr_tostr(#varref{name=Name}) ->
    io_lib:format("~s", [Name]);
expr_tostr({Any, _Line, Value}) when Any =:= integer; Any =:= float ->
    io_lib:format("~w", [Value]);
expr_tostr({Any, _Line, S}) when Any =:= string ->
    io_lib:format("\"~s\"", [S]).

translate_operator('assign') -> "=";
translate_operator('rem') -> "%";
translate_operator('bxor') -> "^";
translate_operator('bsr') -> ">>";
translate_operator('bsl') -> "<<";
translate_operator('band') -> "&";
translate_operator('bor') -> "|";
translate_operator('and') -> "&&";
translate_operator('or') -> "||";
translate_operator('@') -> "&";
translate_operator('^') -> "*";
translate_operator(Any) -> Any.

