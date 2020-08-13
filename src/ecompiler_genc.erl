-module(ecompiler_genc).

-export([generate_ccode/2]).

-include("./ecompiler_frame.hrl").


generate_ccode(Ast, OutputFile) ->
    {Fns, Others} = lists:partition(fun(A) -> element(1, A) =:= function end,
				    Ast),
    FnDeclars = get_function_declars(Fns),
    FnStatements = statements_tostr(Fns),
    OtherStatements = statements_tostr(Others),
    file:write_file(OutputFile, [common_code(), OtherStatements, FnDeclars
				 | FnStatements]).

common_code() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
    "typedef unsigned char u8;\ntypedef char i8;\n"
    "typedef unsigned short u16;\ntypedef short i16;\n"
    "typedef unsigned int u32;\ntypedef int i32;\n"
    "typedef unsigned long u64;\ntypedef long i64;\n"
    "typedef double f64;\ntypedef float f32;\n\n".

get_function_declars(FnAst) ->
    get_function_declars(FnAst, []).

get_function_declars([#function{name=Name, params=Params, ret=Rettype}
		      | Rest], Declars) ->
    Declar = fn_declar_str(Name, Params, Rettype),
    get_function_declars(Rest, [Declar | Declars]);
get_function_declars([], Declars) ->
    lists:flatten(lists:join(";\n", Declars), ";\n\n").

fn_declar_str(Name, Params, Rettype) ->
    io_lib:format("~s ~s(~s)", [type_tostr(Rettype), Name,
				defvar_tostr(Params, {",", false})]).

statements_tostr([#function{name=Name, params=Params,
			    ret=Rettype, exprs=Exprs} | Rest]) ->
    Declar = fn_declar_str(Name, Params, Rettype),
    [io_lib:format("~s~n{~n~s~n}~n~n", [Declar, exprs_tostr(Exprs)])
     | statements_tostr(Rest)];
statements_tostr([#struct{name=Name, fields=Fields} | Rest]) ->
    [io_lib:format("typedef struct {~n~s~n} ~s;~n~n",
		   [defvar_tostr(Fields), Name])
     | statements_tostr(Rest)];
statements_tostr([#const{name=Name, val=Expr} | Rest]) ->
    [io_lib:format("#define ~s (~s)~n~n",
		   [Name, expr_tostr(Expr)]) | statements_tostr(Rest)];
statements_tostr([]) ->
    [].


defvar_tostr(Vars, {SplitChar, true}) ->
    [defvar_tostr(Vars, SplitChar, []), SplitChar];
defvar_tostr(Vars, {SplitChar, false}) ->
    defvar_tostr(Vars, SplitChar, []).

defvar_tostr(Vars) ->
    [defvar_tostr(Vars, ";\n", []), ";\n"].

-define(VARDEF(Name, Type, Initval),
	#vardef{name=Name, type=Type, initval=Initval}).

defvar_tostr([?VARDEF(Name, Type, Initval) | Rest], SplitChar, Defs)
  when Initval =/= none ->
    S = io_lib:format("~s = ~s", [type_tostr_vardef(Type, Name),
				  expr_tostr(Initval)]),
    defvar_tostr(Rest, SplitChar, [S | Defs]);
defvar_tostr([?VARDEF(Name, Type, _Initval) | Rest], SplitChar, Defs) ->
    defvar_tostr(Rest, SplitChar,
		 [type_tostr_vardef(Type, Name) | Defs]);
defvar_tostr([], SplitChar, Defs) ->
    lists:join(SplitChar, lists:reverse(Defs)).


%% int[3] a; is not valid in C, you need int a[3];
type_tostr_vardef({box_type, _, Size, Typeanno}, Name) ->
    io_lib:format("~s ~s[~w]", [type_anno_tostr(Typeanno), Name, Size]);
type_tostr_vardef({basic_type, _, Typeanno}, Name) ->
    io_lib:format("~s ~s", [type_anno_tostr(Typeanno), Name]).

type_tostr({box_type, _, Size, Typeanno}) ->
    io_lib:format("~s[~w]", [type_anno_tostr(Typeanno), Size]);
type_tostr({basic_type, _, Typeanno}) ->
    type_anno_tostr(Typeanno).

type_anno_tostr({T, Depth}) ->
    io_lib:format("~s~s", [T, lists:duplicate(Depth, "*")]);
type_anno_tostr(T) ->
    io_lib:format("~s", [T]).

expr_tostr(#if_expr{condition=Condition, then=Then, else=Else}) ->
    io_lib:format("if (~s) {\n~s\n} else {\n~s}",
		  [expr_tostr(Condition), exprs_tostr(Then),
		   exprs_tostr(Else)]);
expr_tostr(#while_expr{condition=Condition, exprs=Exprs}) ->
    io_lib:format("while (~s) {\n~s\n}\n",
		  [expr_tostr(Condition), exprs_tostr(Exprs)]);
expr_tostr({op, _Line, Operator, Operand1, Operand2}) ->
    io_lib:format("(~s ~s ~s)", [expr_tostr(Operand1),
			       translate_operator(Operator),
			       expr_tostr(Operand2)]);
expr_tostr({op, _Line, Operator, Operand}) ->
    io_lib:format("(~s~s)", [translate_operator(Operator),
			   expr_tostr(Operand)]);
expr_tostr({call, _Line, Name, Args}) ->
    io_lib:format("~s(~s)", [Name, lists:join(",", lists:map(fun expr_tostr/1,
							     Args))]);
expr_tostr({return, _Line, Expr}) ->
    io_lib:format("return ~s", [expr_tostr(Expr)]);
expr_tostr({vardef, _Line, Name, Type, none}) ->
    type_tostr_vardef(Type, Name);
expr_tostr({vardef, _Line, Name, Type, Initval}) ->
    io_lib:format("~s = ~s", [type_tostr_vardef(Type, Name),
			      expr_tostr(Initval)]);
expr_tostr({varref, _Line, Name}) ->
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

exprs_tostr(Exprs) ->
    [lists:join(";\n", exprs_tostr(Exprs, [])), ";"].

exprs_tostr([Expr | Rest], ExprList) ->
    exprs_tostr(Rest, [expr_tostr(Expr) | ExprList]);
exprs_tostr([], ExprList) ->
    lists:reverse(ExprList).

