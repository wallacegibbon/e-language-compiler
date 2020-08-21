-module(ecompiler_genc).

-export([generate_ccode/4]).

-import(ecompiler_utils, [exprsmap/2, is_primitive_type/1,
			  getvalues_bykeys/2]).

-include("./ecompiler_frame.hrl").

generate_ccode({{FnMap, StructMap}, {_, StructList}}, GlobalVars,
		_InitCode, OutputFile) ->
    FixedFnMap = make_functionmap_for_c(FnMap, StructMap, GlobalVars),
    {FnStatements, FnDeclars} = statements_tostr(maps:values(FixedFnMap)),
    %% use StructList instead of StructMap, because the order of struct matters
    %% in C language
    {StructStatements, []} = statements_tostr(StructList),
    VarStatements = mapvars_to_str(GlobalVars),
    Code = [common_code(), StructStatements, "\n\n", VarStatements, "\n\n",
	    FnDeclars, "\n\n", FnStatements],
    file:write_file(OutputFile, Code).

make_functionmap_for_c(FnMap, StructMap, GlobalVars) ->
    maps:map(fun(_, #function{exprs=Exprs, var_types=VarTypes} = F) ->
		     CurrentVars = maps:merge(GlobalVars, VarTypes),
		     Ctx = {CurrentVars, FnMap, StructMap},
		     F#function{exprs=fixexprs_for_c(Exprs, Ctx)}
	     end, FnMap).

fixexprs_for_c(Exprs, Ctx) ->
    exprsmap(fun(E) -> fixexpr_for_c(E, Ctx) end, Exprs).

fixexpr_for_c(#op1{operator='@', operand=Operand, line=Line} = E,
	      {VarTypes, Functions, Structs}) ->
    case ecompiler_type:typeof_expr(Operand, {VarTypes, Functions, Structs,
					      none}) of
	#array_type{elemtype=_} ->
	    #op2{operator='.', op1=Operand, op2=#varref{name=val, line=Line}};
	_ ->
	    E
    end;
fixexpr_for_c(#op1{operand=Operand} = E, Ctx) ->
    E#op1{operand=fixexpr_for_c(Operand, Ctx)};
fixexpr_for_c(#op2{op1=Op1, op2=Op2} = E, Ctx) ->
    E#op2{op1=fixexpr_for_c(Op1, Ctx), op2=fixexpr_for_c(Op2, Ctx)};
fixexpr_for_c(Any, _Env) ->
    Any.

common_code() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
    "typedef unsigned char u8;\ntypedef char i8;\n"
    "typedef unsigned short u16;\ntypedef short i16;\n"
    "typedef unsigned int u32;\ntypedef int i32;\n"
    "typedef unsigned long u64;\ntypedef long i64;\n"
    "typedef double f64;\ntypedef float f32;\n\n".

statements_tostr(Statements) ->
    statements_tostr(Statements, [], []).

statements_tostr([#function{name=Name, param_names=ParamNames, type=Fntype,
			    var_types=VarTypes, exprs=Exprs} | Rest],
		 StatementStrs, FnDeclars) ->
    Declar = fn_declar_str(Name, ParamNames, VarTypes, Fntype#fun_type.ret),
    S = io_lib:format("~s~n{~n~s~n~n~s~n}~n~n",
		      [Declar, mapvars_to_str(maps:without(ParamNames, VarTypes)),
		       exprs_tostr(Exprs)]),
    statements_tostr(Rest, [S | StatementStrs], [Declar ++ ";\n" | FnDeclars]);
statements_tostr([#struct{name=Name, field_types=FieldTypes,
			  field_names=FieldNames} | Rest],
		 StatementStrs, FnDeclars) ->
    FieldList = lists:zip(FieldNames, getvalues_bykeys(FieldNames, FieldTypes)),
    S = io_lib:format("struct ~s {~n~s~n};~n~n",
		      [Name, listvars_to_str(FieldList)]),
    statements_tostr(Rest, [S | StatementStrs], FnDeclars);
statements_tostr([], StatementStrs, FnDeclars) ->
    {lists:reverse(StatementStrs), lists:reverse(FnDeclars)}.

fn_declar_str(Name, ParamNames, VarTypes, Rettype) ->
    io_lib:format("~s(~s)", [type_tostr(Rettype, Name),
			     params_to_str(ParamNames, VarTypes)]).

params_to_str(ParamNames, VarTypes) ->
    params_to_str(ParamNames, VarTypes, []).

params_to_str([ParamName | RestParamNames], VarTypes, FmtParams) ->
    Pstr = type_tostr(maps:get(ParamName, VarTypes), ParamName),
    params_to_str(RestParamNames, VarTypes, [Pstr | FmtParams]);
params_to_str([], _, FmtParams) ->
    lists:join(",", lists:reverse(FmtParams)).

mapvars_to_str(VarsMap) when is_map(VarsMap) ->
    lists:flatten(lists:join(";\n", vars_to_str(maps:to_list(VarsMap), [])),
		  ";").

listvars_to_str(VarList) when is_list(VarList) ->
    lists:flatten(lists:join(";\n", vars_to_str(VarList, [])), ";").

vars_to_str([{Name, Type} | Rest], Strs) ->
    vars_to_str(Rest, [type_tostr(Type, Name) | Strs]);
vars_to_str([], Strs) ->
    lists:reverse(Strs).

%% convert type to C string
type_tostr(#fun_type{params=Params, ret=Rettype}, Varname) ->
    Paramstr = lists:join(",", lists:map(fun(T) ->
						 type_tostr(T, "")
					 end, Params)),
    io_lib:format("~s (*~s)(~s)", [type_tostr(Rettype, ""), Varname,
				  Paramstr]);
type_tostr(#array_type{size=Size, elemtype=ElementType}, Varname) ->
    io_lib:format("struct {~s val[~w];} ~s", [type_tostr(ElementType, ""),
					      Size, Varname]);
type_tostr(#basic_type{type={Typeanno, Depth}}, Varname) when Depth > 0 ->
    io_lib:format("~s~s ~s", [typeanno_tostr(Typeanno),
			      lists:duplicate(Depth, "*"), Varname]);
type_tostr(#basic_type{type={Typeanno, 0}}, Varname) ->
    io_lib:format("~s ~s", [typeanno_tostr(Typeanno), Varname]).

typeanno_tostr(Name) when is_atom(Name) ->
    case is_primitive_type(Name) of
	false ->
	    io_lib:format("struct ~s", [Name]);
	true ->
	    atom_to_list(Name)
    end.

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
expr_tostr(#call{fn=Fn, args=Args}) ->
    io_lib:format("~s(~s)", [expr_tostr(Fn),
			     lists:join(",", lists:map(fun expr_tostr/1,
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

