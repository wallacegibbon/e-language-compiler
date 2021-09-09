-module(ecompilerGenerateCCode).

-export([generateCCode/4]).

-include("ecompilerFrameDef.hrl").

-type genCContext() :: {functionTypeMap(), structTypeMap(), variableTypeMap()}.

-spec generateCCode(eAST(), variableTypeMap(), eAST(), string()) -> ok.
generateCCode(AST, GlobalVars, InitCode, OutputFile) ->
    {FunctionTypeMap, StructMap} = ecompilerUtil:makeFunctionAndStructMapFromAST(AST),
    Ctx = {FunctionTypeMap, StructMap, GlobalVars},
    Ast2 = lists:map(fun (A) -> prvFixFunctionForC(A, Ctx) end, AST),
    InitCode2 = prvFixExpressionsForC(InitCode, Ctx),
    %io:format(">>>~p~n", [Ast2]),
    %% struct definition have to be before function declarations
    CheckStruct = fun (A) -> element(1, A) =:= struct end,
    {StructAST, FunctionAST} = lists:partition(CheckStruct, Ast2),
    {StructStatements, []} = prvStatementsToString(StructAST, []),
    {FnStatements, FnDeclars} = prvStatementsToString(FunctionAST, InitCode2),
    VarStatements = prvVariableMapToString(GlobalVars),
    Code = lists:join("\n\n", [prvCommonCCodes(), StructStatements, VarStatements, FnDeclars, FnStatements]),
    ok = file:write_file(OutputFile, Code).

-spec prvFixFunctionForC(eExpression(), genCContext()) -> eExpression().
prvFixFunctionForC(#function{exprs = Expressions, var_types = VarTypes} = F, {FunctionTypeMap, StructMap, GlobalVars}) ->
    F#function{exprs = prvFixExpressionsForC(Expressions, {FunctionTypeMap, StructMap, maps:merge(GlobalVars, VarTypes)})};
prvFixFunctionForC(Any, _) ->
    Any.

-spec prvFixExpressionsForC(eAST(), genCContext()) -> eAST().
prvFixExpressionsForC(Expressions, Ctx) ->
    ecompilerUtil:expressionMap(fun (E) -> prvFixExpressionForC(E, Ctx) end, Expressions).

-spec prvFixExpressionForC(eExpression(), genCContext()) -> eExpression().
prvFixExpressionForC(#op1{operator = '@', operand = Operand, line = Line} = E, {FunctionTypeMap, StructMap, VarTypes} = Ctx) ->
    case ecompilerType:typeOfExpression(Operand, {VarTypes, FunctionTypeMap, StructMap, #{}}) of
        #array_type{} ->
            #op2{operator = '.', op1 = prvFixExpressionForC(Operand, Ctx), op2 = #varref{name = val, line = Line}};
        _ ->
            E
    end;
prvFixExpressionForC(#op1{operand = Operand} = E, Ctx) ->
    E#op1{operand = prvFixExpressionForC(Operand, Ctx)};
prvFixExpressionForC(#op2{op1 = Operand1, op2 = Operand2} = E, Ctx) ->
    E#op2{op1 = prvFixExpressionForC(Operand1, Ctx), op2 = prvFixExpressionForC(Operand2, Ctx)};
prvFixExpressionForC(Any, _) ->
    Any.

-spec prvCommonCCodes() -> string().
prvCommonCCodes() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
    "typedef unsigned int usize;\ntypedef int isize;\ntypedef unsigned char u8;\ntypedef char i8;\n"
    "typedef unsigned short u16;\ntypedef short i16;\ntypedef unsigned int u32;\ntypedef int i32;\n"
    "typedef unsigned long u64;\ntypedef long i64;\ntypedef double f64;\ntypedef float f32;\n\n".

prvStatementsToString(Statements, InitCode) ->
    prvStatementsToString(Statements, InitCode, [], []).

prvStatementsToString([#function{name = Name, param_names = ParamNames, type = Fntype, var_types = VarTypes, exprs = Expressions} | Rest], InitCode, StatementStrs, FnDeclars) ->
    ParamNameAtoms = prvFetchNamesFromVariableReferences(ParamNames),
    PureParams = prvMapToKVList(ParamNameAtoms, maps:with(ParamNameAtoms, VarTypes)),
    PureVars = maps:without(ParamNameAtoms, VarTypes),
    Declar = prvFunctioinDeclarationToString(Name, prvFunctionParametersToString(PureParams), Fntype#fun_type.ret),
    Exprs2 = case Name =:= main of
                true ->
                    InitCode ++ Expressions;
                false ->
                    Expressions
             end,
    S = io_lib:format("~s~n{~n~s~n~n~s~n}~n~n", [Declar, prvVariableMapToString(PureVars), prvExpressionsToString(Exprs2)]),
    prvStatementsToString(Rest, InitCode, [S | StatementStrs], [Declar ++ ";\n" | FnDeclars]);
prvStatementsToString([#struct{name = Name, field_types = FieldTypes, field_names = FieldNames} | Rest], InitCode, StatementStrs, FnDeclars) ->
    FieldList = prvMapToKVList(prvFetchNamesFromVariableReferences(FieldNames), FieldTypes),
    S = io_lib:format("struct ~s {~n~s~n};~n~n", [Name, prvVariableListToString(FieldList)]),
    prvStatementsToString(Rest, InitCode, [S | StatementStrs], FnDeclars);
prvStatementsToString([], _, StatementStrs, FnDeclars) ->
    {lists:reverse(StatementStrs), lists:reverse(FnDeclars)}.

prvFunctioinDeclarationToString(Name, ParamStr, #basic_type{pdepth = N} = Rettype) when N > 0 ->
    prvFunctionReturnTypeToString(Rettype, io_lib:format("(*~s(~s))", [Name, ParamStr]));
prvFunctioinDeclarationToString(Name, ParamStr, #fun_type{} = Rettype) ->
    prvFunctionReturnTypeToString(Rettype, io_lib:format("(*~s(~s))", [Name, ParamStr]));
prvFunctioinDeclarationToString(Name, ParamStr, Rettype) ->
    prvTypeToCString(Rettype, io_lib:format("~s(~s)", [Name, ParamStr])).

prvFunctionParametersToString(NameTypePairs) ->
    lists:join(",", lists:map(fun ({N, T}) -> prvTypeToCString(T, N) end, NameTypePairs)).

prvFunctionParamsToStringNoFunctionNames(Types) ->
    lists:join(",", lists:map(fun (T) -> prvTypeToCString(T, "") end, Types)).

%% order is not necessary for vars
prvVariableMapToString(VarsMap) when is_map(VarsMap) ->
    lists:flatten(lists:join(";\n", prvVariablesToString(maps:to_list(VarsMap), [])), ";").

prvVariableListToString(VarList) when is_list(VarList) ->
    lists:flatten(lists:join(";\n", prvVariablesToString(VarList, [])), ";").

prvVariablesToString([{Name, Type} | Rest], Strs) ->
    prvVariablesToString(Rest, [prvTypeToCString(Type, Name) | Strs]);
prvVariablesToString([], Strs) ->
    lists:reverse(Strs).

prvFetchNamesFromVariableReferences(VarrefList) ->
    lists:map(fun (#varref{name = N}) -> N end, VarrefList).

prvMapToKVList(NameAtoms, ValueMap) ->
    lists:zip(NameAtoms, ecompilerUtil:getValuesByKeys(NameAtoms, ValueMap)).

prvFunctionReturnTypeToString(#fun_type{params = Params, ret = Rettype}, NameParams) ->
    Paramstr = prvFunctionParamsToStringNoFunctionNames(Params),
    NewNameParams = io_lib:format("~s(~s)", [NameParams, Paramstr]),
    prvTypeToCString(Rettype, NewNameParams);
prvFunctionReturnTypeToString(#basic_type{pdepth = N} = T, NameParams) when N > 0 ->
    prvTypeToCString(T#basic_type{pdepth = N - 1}, NameParams).

%% convert type to C string
-spec prvTypeToCString(eExpression(), iolist()) -> iolist().
prvTypeToCString(#array_type{len = Len, elemtype = ElementType}, Varname) ->
    io_lib:format("struct {~s val[~w];} ~s", [prvTypeToCString(ElementType, ""), Len, Varname]);
prvTypeToCString(#basic_type{class = Class, tag = Tag, pdepth = Depth}, Varname) when Depth > 0 ->
    io_lib:format("~s~s ~s", [prvTypeTagToString(Class, Tag), lists:duplicate(Depth, "*"), Varname]);
prvTypeToCString(#basic_type{class = Class, tag = Tag, pdepth = 0}, Varname) ->
    io_lib:format("~s ~s", [prvTypeTagToString(Class, Tag), Varname]);
prvTypeToCString(#fun_type{params = Params, ret = Rettype}, Varname) ->
    Paramstr = prvFunctionParamsToStringNoFunctionNames(Params),
    NameParams = io_lib:format("(*~s)(~s)", [Varname, Paramstr]),
    prvTypeToCString(Rettype, NameParams).

prvTypeTagToString(struct, Name) ->
    io_lib:format("struct ~s", [Name]);
prvTypeTagToString(_, Name) ->
    atom_to_list(Name).

%% convert expression to C string
prvExpressionsToString(Expressions) ->
    [lists:join("\n", prvExpressionsToString(Expressions, []))].

prvExpressionsToString([Expression | Rest], ExprList) ->
    prvExpressionsToString(Rest, [prvExpressionToString(Expression, $;) | ExprList]);
prvExpressionsToString([], ExprList) ->
    lists:reverse(ExprList).

-spec prvExpressionToString(eExpression(), char()) -> iolist().
prvExpressionToString(#if_expr{condition = Condition, then = Then, else = Else}, _) ->
    io_lib:format("if (~s) {\n~s\n} else {\n~s}", [prvExpressionToString(Condition, $\s), prvExpressionsToString(Then), prvExpressionsToString(Else)]);
prvExpressionToString(#while_expr{condition = Condition, exprs = Expressions}, _) ->
    io_lib:format("while (~s) {\n~s\n}\n", [prvExpressionToString(Condition, $\s), prvExpressionsToString(Expressions)]);
prvExpressionToString(#op2{operator = '::', op1 = #varref{name = c}, op2 = Operand2}, Endchar) ->
    prvExpressionToString(Operand2, Endchar);
prvExpressionToString(#op2{operator = Operator, op1 = Operand1, op2 = Operand2}, Endchar) ->
    io_lib:format("(~s ~s ~s)~c", [prvExpressionToString(Operand1, $\s), prvTranslateOperator(Operator), prvExpressionToString(Operand2, $\s), Endchar]);
prvExpressionToString(#op1{operator = Operator, operand = Operand}, Endchar) ->
    io_lib:format("(~s ~s)~c", [prvTranslateOperator(Operator), prvExpressionToString(Operand, $\s), Endchar]);
prvExpressionToString(#call{fn = Fn, args = Arguments}, Endchar) ->
    ArgumentString = lists:join(",", lists:map(fun (E) -> prvExpressionToString(E, $\s) end, Arguments)),
    io_lib:format("~s(~s)~c", [prvExpressionToString(Fn, $\s), ArgumentString, Endchar]);
prvExpressionToString(#return{expr = Expression}, Endchar) ->
    io_lib:format("return ~s~c", [prvExpressionToString(Expression, $\s), Endchar]);
prvExpressionToString(#goto{expr = Expression}, Endchar) ->
    io_lib:format("goto ~s~c", [prvExpressionToString(Expression, $\s), Endchar]);
prvExpressionToString(#label{name = Name}, _) ->
    io_lib:format("~s:", [Name]);
prvExpressionToString(#varref{name = Name}, Endchar) ->
    io_lib:format("~s~c", [Name, Endchar]);
prvExpressionToString({Any, _Line, Value}, Endchar) when Any =:= integer; Any =:= float ->
    io_lib:format("~w~c", [Value, Endchar]);
prvExpressionToString({Any, _Line, S}, Endchar) when Any =:= string ->
    io_lib:format("\"~s\"~c", [prvHandleSpecialCharactersInString(S), Endchar]).

-define(SPECIAL_CHARMAP, #{$\n => "\\n", $\r => "\\r", $\t => "\\t", $\f => "\\f", $\b => "\\b"}).

prvHandleSpecialCharactersInString(String) ->
    lists:map(fun (C) -> maps:get(C, ?SPECIAL_CHARMAP, C) end, String).

-spec prvTranslateOperator(atom()) -> string() | atom().
prvTranslateOperator(assign) ->
    "=";
prvTranslateOperator('rem') ->
    "%";
prvTranslateOperator('bxor') ->
    "^";
prvTranslateOperator('bsr') ->
    ">>";
prvTranslateOperator('bsl') ->
    "<<";
prvTranslateOperator('band') ->
    "&";
prvTranslateOperator('bor') ->
    "|";
prvTranslateOperator('and') ->
    "&&";
prvTranslateOperator('or') ->
    "||";
prvTranslateOperator('@') ->
    "&";
prvTranslateOperator('^') ->
    "*";
prvTranslateOperator(Any) ->
    Any.
