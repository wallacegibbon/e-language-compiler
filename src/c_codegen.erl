-module(c_codegen).
-export([generate_c_code/4]).

-include("e_record_definition.hrl").

-type context() :: {fn_type_map(), struct_type_map(), var_type_map()}.

-spec generate_c_code(e_ast(), var_type_map(), e_ast(), string()) -> ok.
generate_c_code(AST, GlobalVars, InitCode, OutputFile) ->
    {FunctionTypeMap, StructMap} = e_util:make_function_and_struct_map_from_ast(AST),
    Context = {FunctionTypeMap, StructMap, GlobalVars},
    AST2 = lists:map(fun (A) -> fix_function_for_c(A, Context) end, AST),
    InitCode2 = fix_exprs_for_c(InitCode, Context),
    %io:format(">>>~p~n", [AST2]),
    %% struct definition have to be before function declarations
    CheckStruct = fun (A) -> element(1, A) =:= struct end,
    {StructAST, FunctionAST} = lists:partition(CheckStruct, AST2),
    {StructStatements, []} = statements_to_str(StructAST, []),
    {FnStatements, FunctionDeclarations} = statements_to_str(FunctionAST, InitCode2),
    VarStatements = var_map_to_str(GlobalVars),
    Code = lists:join("\n\n", [common_c_code(), StructStatements, VarStatements, FunctionDeclarations, FnStatements]),
    ok = file:write_file(OutputFile, Code).

-spec fix_function_for_c(e_expr(), context()) -> e_expr().
fix_function_for_c(#function{stmts = Expressions, var_type_map = VarTypes} = F, {FunctionTypeMap, StructMap, GlobalVars}) ->
    F#function{stmts = fix_exprs_for_c(Expressions, {FunctionTypeMap, StructMap, maps:merge(GlobalVars, VarTypes)})};
fix_function_for_c(Any, _) ->
    Any.

-spec fix_exprs_for_c(e_ast(), context()) -> e_ast().
fix_exprs_for_c(Expressions, Context) ->
    e_util:expr_map(fun (E) -> fix_expr_for_c(E, Context) end, Expressions).

-spec fix_expr_for_c(e_expr(), context()) -> e_expr().
fix_expr_for_c(#op1_expr{operator = '@', operand = Operand, line = Line} = E, {FunctionTypeMap, StructMap, VarTypes} = Context) ->
    case e_type:type_of_ast_node(Operand, {VarTypes, FunctionTypeMap, StructMap, #{}}) of
        #array_type{} ->
            #op2_expr{operator = '.', operand1 = fix_expr_for_c(Operand, Context), operand2 = #variable_reference{name = value, line = Line}};
        _ ->
            E
    end;
fix_expr_for_c(#op1_expr{operand = Operand} = E, Context) ->
    E#op1_expr{operand = fix_expr_for_c(Operand, Context)};
fix_expr_for_c(#op2_expr{operand1 = Operand1, operand2 = Operand2} = E, Context) ->
    E#op2_expr{operand1 = fix_expr_for_c(Operand1, Context), operand2 = fix_expr_for_c(Operand2, Context)};
fix_expr_for_c(Any, _) ->
    Any.

-spec common_c_code() -> string().
common_c_code() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
    "typedef unsigned int usize;\ntypedef int isize;\ntypedef unsigned char u8;\ntypedef char i8;\n"
    "typedef unsigned short u16;\ntypedef short i16;\ntypedef unsigned int u32;\ntypedef int i32;\n"
    "typedef unsigned long u64;\ntypedef long i64;\ntypedef double f64;\ntypedef float f32;\n\n".

statements_to_str(Statements, InitCode) ->
    statements_to_str(Statements, InitCode, [], []).

statements_to_str([#function{name = Name, param_names = ParamNames, type = FunctionType, var_type_map = VarTypes, stmts = Expressions} | Rest], InitCode, StatementStringList, FnDeclarationList) ->
    ParamNameAtoms = names_from_varrefs(ParamNames),
    PureParams = map_to_kv_list(ParamNameAtoms, maps:with(ParamNameAtoms, VarTypes)),
    PureVars = maps:without(ParamNameAtoms, VarTypes),
    Declarations = function_declaration_to_str(Name, function_params_to_str(PureParams), FunctionType#function_type.ret),
    Expressions2 = case Name =:= main of
                       true ->
                           InitCode ++ Expressions;
                       false ->
                           Expressions
                   end,
    S = io_lib:format("~s~n{~n~s~n~n~s~n}~n~n", [Declarations, var_map_to_str(PureVars), exprs_to_str(Expressions2)]),
    statements_to_str(Rest, InitCode, [S | StatementStringList], [Declarations ++ ";\n" | FnDeclarationList]);
statements_to_str([#struct{name = Name, field_type_map = FieldTypes, field_names = FieldNames} | Rest], InitCode, StatementStringList, FnDeclarationList) ->
    FieldList = map_to_kv_list(names_from_varrefs(FieldNames), FieldTypes),
    S = io_lib:format("struct ~s {~n~s~n};~n~n", [Name, var_list_to_str(FieldList)]),
    statements_to_str(Rest, InitCode, [S | StatementStringList], FnDeclarationList);
statements_to_str([], _, StatementStringList, FnDeclarationList) ->
    {lists:reverse(StatementStringList), lists:reverse(FnDeclarationList)}.

function_declaration_to_str(Name, ParamStr, #basic_type{p_depth = N} = ReturnType) when N > 0 ->
    return_type_to_str(ReturnType, io_lib:format("(*~s(~s))", [Name, ParamStr]));
function_declaration_to_str(Name, ParamStr, #function_type{} = ReturnType) ->
    return_type_to_str(ReturnType, io_lib:format("(*~s(~s))", [Name, ParamStr]));
function_declaration_to_str(Name, ParamStr, ReturnType) ->
    type_to_c_str(ReturnType, io_lib:format("~s(~s)", [Name, ParamStr])).

function_params_to_str(NameTypePairs) ->
    lists:join(",", lists:map(fun ({N, T}) -> type_to_c_str(T, N) end, NameTypePairs)).

function_params_to_str_no_name(Types) ->
    lists:join(",", lists:map(fun (T) -> type_to_c_str(T, "") end, Types)).

%% order is not necessary for vars
var_map_to_str(VarsMap) when is_map(VarsMap) ->
    lists:flatten(lists:join(";\n", vars_to_str(maps:to_list(VarsMap), [])), ";").

var_list_to_str(VarList) when is_list(VarList) ->
    lists:flatten(lists:join(";\n", vars_to_str(VarList, [])), ";").

vars_to_str([{Name, Type} | Rest], Strs) ->
    vars_to_str(Rest, [type_to_c_str(Type, Name) | Strs]);
vars_to_str([], Strs) ->
    lists:reverse(Strs).

names_from_varrefs(VarrefList) ->
    lists:map(fun (#variable_reference{name = N}) -> N end, VarrefList).

map_to_kv_list(NameAtoms, ValueMap) ->
    lists:zip(NameAtoms, e_util:get_values_by_keys(NameAtoms, ValueMap)).

return_type_to_str(#function_type{parameters = Params, ret = ReturnType}, NameParams) ->
    ParametersString = function_params_to_str_no_name(Params),
    NewNameParams = io_lib:format("~s(~s)", [NameParams, ParametersString]),
    type_to_c_str(ReturnType, NewNameParams);
return_type_to_str(#basic_type{p_depth = N} = T, NameParams) when N > 0 ->
    type_to_c_str(T#basic_type{p_depth = N - 1}, NameParams).

%% convert type to C string
-spec type_to_c_str(e_expr(), iolist()) -> iolist().
type_to_c_str(#array_type{length = Len, elem_type = ElementType}, VariableName) ->
    io_lib:format("struct {~s value[~w];} ~s", [type_to_c_str(ElementType, ""), Len, VariableName]);
type_to_c_str(#basic_type{class = Class, tag = Tag, p_depth = Depth}, VariableName) when Depth > 0 ->
    io_lib:format("~s~s ~s", [type_tag_to_str(Class, Tag), lists:duplicate(Depth, "*"), VariableName]);
type_to_c_str(#basic_type{class = Class, tag = Tag, p_depth = 0}, VariableName) ->
    io_lib:format("~s ~s", [type_tag_to_str(Class, Tag), VariableName]);
type_to_c_str(#function_type{parameters = Params, ret = ReturnType}, VariableName) ->
    ParameterString = function_params_to_str_no_name(Params),
    NameParams = io_lib:format("(*~s)(~s)", [VariableName, ParameterString]),
    type_to_c_str(ReturnType, NameParams).

type_tag_to_str(struct, Name) ->
    io_lib:format("struct ~s", [Name]);
type_tag_to_str(_, Name) ->
    atom_to_list(Name).

%% convert expression to C string
exprs_to_str(Expressions) ->
    [lists:join("\n", exprs_to_str(Expressions, []))].

exprs_to_str([Expression | Rest], ExprList) ->
    exprs_to_str(Rest, [expr_to_str(Expression, $;) | ExprList]);
exprs_to_str([], ExprList) ->
    lists:reverse(ExprList).

-spec expr_to_str(e_expr(), char()) -> iolist().
expr_to_str(#if_stmt{condi = Condition, then = Then, else = Else}, _) ->
    io_lib:format("if (~s) {\n~s\n} else {\n~s}", [expr_to_str(Condition, $\s), exprs_to_str(Then), exprs_to_str(Else)]);
expr_to_str(#while_stmt{condi = Condition, stmts = Expressions}, _) ->
    io_lib:format("while (~s) {\n~s\n}\n", [expr_to_str(Condition, $\s), exprs_to_str(Expressions)]);
expr_to_str(#op2_expr{operator = '::', operand1 = #variable_reference{name = c}, operand2 = Operand2}, EndChar) ->
    expr_to_str(Operand2, EndChar);
expr_to_str(#op2_expr{operator = Operator, operand1 = Operand1, operand2 = Operand2}, EndChar) ->
    io_lib:format("(~s ~s ~s)~c", [expr_to_str(Operand1, $\s), translate_op(Operator), expr_to_str(Operand2, $\s), EndChar]);
expr_to_str(#op1_expr{operator = Operator, operand = Operand}, EndChar) ->
    io_lib:format("(~s ~s)~c", [translate_op(Operator), expr_to_str(Operand, $\s), EndChar]);
expr_to_str(#call_expr{fn = Fn, args = Arguments}, EndChar) ->
    ArgumentString = lists:join(",", lists:map(fun (E) -> expr_to_str(E, $\s) end, Arguments)),
    io_lib:format("~s(~s)~c", [expr_to_str(Fn, $\s), ArgumentString, EndChar]);
expr_to_str(#return_stmt{expr = Expression}, EndChar) ->
    io_lib:format("return ~s~c", [expr_to_str(Expression, $\s), EndChar]);
expr_to_str(#goto_stmt{expr = Expression}, EndChar) ->
    io_lib:format("goto ~s~c", [expr_to_str(Expression, $\s), EndChar]);
expr_to_str(#goto_label{name = Name}, _) ->
    io_lib:format("~s:", [Name]);
expr_to_str(#variable_reference{name = Name}, EndChar) ->
    io_lib:format("~s~c", [Name, EndChar]);
expr_to_str(#type_convert{expr = Expression, type = TargetType}, EndChar) ->
    io_lib:format("((~s) ~s)~c", [type_to_c_str(TargetType, ""), expr_to_str(Expression, $\s), EndChar]);
expr_to_str({Any, _Line, Value}, EndChar) when Any =:= integer; Any =:= float ->
    io_lib:format("~w~c", [Value, EndChar]);
expr_to_str({Any, _Line, S}, EndChar) when Any =:= string ->
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
