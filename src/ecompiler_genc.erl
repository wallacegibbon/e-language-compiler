-module(ecompiler_genc).

-export([generate_ccode/2]).

-include("./ecompiler_frame.hrl").

generate_ccode(Ast, OutputFile) ->
    AstWithoutConst = ecompiler_fillconst:parse_and_remove_const(Ast),
    {Fns, Others} = lists:partition(fun(A) ->
					    element(1, A) =:= function
				    end, AstWithoutConst),
    FnDeclars = get_function_declars(Fns),
    FnStatements = statements_tostr(Fns),
    OtherStatements = statements_tostr(Others),
    Code = [common_code(), OtherStatements, FnDeclars | FnStatements],
    file:write_file(OutputFile, Code).

common_code() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
    "typedef unsigned char u8;\ntypedef char i8;\n"
    "typedef unsigned short u16;\ntypedef short i16;\n"
    "typedef unsigned int u32;\ntypedef int i32;\n"
    "typedef unsigned long u64;\ntypedef long i64;\n"
    "typedef double f64;\ntypedef float f32;\n\n".

%% convert statements(function, struct and const definitions) to C string
fn_declar_str(Name, Params, Rettype) ->
    io_lib:format("~s ~s(~s)", [type_tostr(Rettype), Name,
				defvar_tostr(Params, {",", false})]).

get_function_declars(FnAst) ->
    get_function_declars(FnAst, []).

get_function_declars([#function{name=Name, params=Params, ret=Rettype}
		      | Rest], Declars) ->
    Declar = fn_declar_str(Name, Params, Rettype),
    get_function_declars(Rest, [Declar | Declars]);
get_function_declars([], Declars) ->
    lists:flatten(lists:join(";\n", Declars), ";\n\n").

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

%% fill ";," of definitions (for function arguments and struct fields)
defvar_tostr(Vars, {SplitChar, true}) ->
    [defvar_tostr(Vars, SplitChar, []), SplitChar];
defvar_tostr(Vars, {SplitChar, false}) ->
    defvar_tostr(Vars, SplitChar, []).

defvar_tostr(Vars) ->
    [defvar_tostr(Vars, ";\n", []), ";\n"].

defvar_tostr([Vardef | Rest], SplitChar, Defs)
  when is_record(Vardef, vardef) ->
    defvar_tostr(Rest, SplitChar, [expr_tostr(Vardef) | Defs]);
defvar_tostr([], SplitChar, Defs) ->
    lists:join(SplitChar, lists:reverse(Defs)).

%% convert type to C string
type_tostr(#box_type{size=Size, elemtype=ElementType}) ->
    io_lib:format("struct {~s val[~w];}", [type_tostr(ElementType), Size]);
type_tostr(#basic_type{type={Typeanno, Depth}}) ->
    io_lib:format("~s ~s", [Typeanno, lists:duplicate(Depth, "*")]);
type_tostr(#basic_type{type=Typeanno}) ->
    io_lib:format("~s", [Typeanno]).

%% convert expression to C string
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
expr_tostr(#vardef{name=Name, type=Type, initval=none}) ->
    io_lib:format("~s ~s", [type_tostr(Type), Name]);
expr_tostr(#vardef{name=Name, type=Type, initval=Initval}) ->
    io_lib:format("~s ~s = ~s", [type_tostr(Type), Name, expr_tostr(Initval)]);
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

exprs_tostr(Exprs) ->
    [lists:join(";\n", exprs_tostr(Exprs, [])), ";"].

exprs_tostr([Expr | Rest], ExprList) ->
    exprs_tostr(Rest, [expr_tostr(Expr) | ExprList]);
exprs_tostr([], ExprList) ->
    lists:reverse(ExprList).

