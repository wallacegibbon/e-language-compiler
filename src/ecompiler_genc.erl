-module(ecompiler_genc).

-export([generate_ccode/2]).

-include("./ecompiler_frame.hrl").

generate_ccode(Ast, OutputFile) ->
    {FnsRaw, OthersRaw} = lists:partition(fun(A) ->
						  element(1, A) =:= function
					  end, Ast),
    FnDeclars = get_function_declars(FnsRaw),
    {Constants, NewOthersRaw} = fetch_constants(OthersRaw),
    Fns = replace_constants(FnsRaw, Constants),
    Others = replace_constants(NewOthersRaw, Constants),
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

%% fetch constants
fetch_constants(Ast) -> fetch_constants(Ast, [], #{}).

fetch_constants([#const{name=Name, val=Expr} | Rest], Statements, Constants) ->
    fetch_constants(Rest, Statements,
		    Constants#{Name => eval_constexpr(Expr, Constants)});
fetch_constants([Any | Rest], Statements, Constants) ->
    fetch_constants(Rest, [Any | Statements], Constants);
fetch_constants([], Statements, Constants) ->
    {Constants, lists:reverse(Statements)}.

eval_constexpr(#op2{operator='+', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) + eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='-', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) - eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='*', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) * eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='/', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) / eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='rem', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) rem eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='band', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) band eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='bor', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) bor eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='bxor', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) bxor eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='bsr', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) bsr eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='bsl', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) bsl eval_constexpr(Op2, Constants);
eval_constexpr(#varref{name=Name, line=Line}, Constants) ->
    try maps:get(Name, Constants)
    catch
	error:{badkey, _} ->
	    throw({Line, flat_format("undefined constant ~s", [Name])})
    end;
eval_constexpr({ImmiType, _, Val}, _) when ImmiType =:= integer;
					   ImmiType =:= float ->
    Val;
eval_constexpr(Num, _) when is_integer(Num); is_float(Num) ->
    Num;
eval_constexpr({Any, Line, Val}, _) ->
    E = flat_format("invalid const expression: ~s, ~p", [Any, Val]),
    throw({Line, E}).

%% replace constants in AST
replace_constants([#function{params=Params, exprs=Exprs} = Fn | Rest],
		  Constants) ->
    [Fn#function{params=replace_constants_inexprs(Params, Constants),
		 exprs=replace_constants_inexprs(Exprs, Constants)}
     | replace_constants(Rest, Constants)];
replace_constants([#struct{fields=Fields} = S | Rest], Constants) ->
    [S#struct{fields=replace_constants_inexprs(Fields, Constants)}
     | replace_constants(Rest, Constants)];
replace_constants([], _) ->
    [].

replace_constants_inexprs([Expr | Rest], Constants) ->
    [replace_constants_inexpr(Expr, Constants) |
     replace_constants_inexprs(Rest, Constants)];
replace_constants_inexprs([], _) ->
    [].

replace_constants_inexpr(#if_expr{condition=Condition, then=Then,
				  else=Else} = Expr,
			 Constants) ->
    Expr#if_expr{condition=replace_constants_inexpr(Condition, Constants),
		 then=replace_constants_inexprs(Then, Constants),
		 else=replace_constants_inexprs(Else, Constants)};
replace_constants_inexpr(#while_expr{condition=Condition, exprs=Exprs} = Expr,
			 Constants) ->
    Expr#while_expr{condition=replace_constants_inexpr(Condition, Constants),
		    exprs=replace_constants_inexprs(Exprs, Constants)};
replace_constants_inexpr(#op2{op1=Op1, op2=Op2} = Expr, Constants) ->
    Expr#op2{op1=replace_constants_inexpr(Op1, Constants),
	     op2=replace_constants_inexpr(Op2, Constants)};
replace_constants_inexpr(#op1{operand=Operand} = Expr, Constants) ->
    Expr#op1{operand=replace_constants_inexpr(Operand, Constants)};
replace_constants_inexpr(#call{args=Args} = Expr, Constants) ->
    Expr#call{args=replace_constants_inexprs(Args, Constants)};
replace_constants_inexpr(#return{expr=RetExpr} = Expr, Constants) ->
    Expr#return{expr=replace_constants_inexpr(RetExpr, Constants)};
replace_constants_inexpr(#vardef{name=Name, initval=Initval, type=Type,
				 line=Line} = Expr, Constants) ->
    case maps:find(Name, Constants) of
	{ok, _} ->
	    throw({Line, flat_format("name ~s is conflict with const",
				     [Name])});
	error ->
	    Expr#vardef{initval=replace_constants_inexpr(Initval, Constants),
			type=replace_constants_intype(Type, Constants)}
    end;
replace_constants_inexpr(#varref{name=Name, line=Line} = Expr, Constants) ->
    case maps:find(Name, Constants) of
	{ok, Val} ->
	    constnum_to_token(Val, Line);
	error ->
	    Expr
    end;
replace_constants_inexpr({constref, Line, Name}, Constants) ->
    case maps:find(Name, Constants) of
	{ok, Val} ->
	    constnum_to_token(Val, Line);
	error ->
	    throw({Line, flat_format("const ~s is not found", [Name])})
    end;
replace_constants_inexpr(Any, _) ->
    Any.

constnum_to_token(Num, Line) when is_float(Num) ->
    #float{val=Num, line=Line};
constnum_to_token(Num, Line) when is_integer(Num) ->
    #integer{val=Num, line=Line}.

replace_constants_intype(#box_type{elemtype=ElementType, size=Size} = T,
			 Constants) ->
    T#box_type{elemtype=replace_constants_intype(ElementType, Constants),
	       size=eval_constexpr(replace_constants_inexpr(Size, Constants),
				   Constants)};
replace_constants_intype(Any, _) ->
    Any.


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

flat_format(FmtStr, Args) ->
    lists:flatten(io_lib:format(FmtStr, Args)).

