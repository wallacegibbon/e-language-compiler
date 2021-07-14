-module(ecompilerGenerateCCode).

-export([generateCCode/4]).

-include("./ecompilerFrameDef.hrl").

generateCCode(AST, GlobalVars, InitCode, OutputFile) ->
    {FnMap, StructMap} = ecompilerUtil:makeFunctionAndStructMapFromAST(AST),
    Ctx = {FnMap, StructMap, GlobalVars},
    Ast2 = lists:map(fun (A) -> prvFixFunctionForC(A, Ctx) end, AST),
    InitCode2 = prvFixExpressionsForC(InitCode, Ctx),
    %io:format(">>>~p~n", [Ast2]),
    %% struct definition have to be before function declarations
    CheckStruct = fun (A) -> element(1, A) =:= struct end,
    {StructAst, FnAst} = lists:partition(CheckStruct, Ast2),
    {StructStatements, []} = prvStatementsToString(StructAst, []),
    {FnStatements, FnDeclars} = prvStatementsToString(FnAst, InitCode2),
    VarStatements = prvVariableMapToString(GlobalVars),
    Code = [prvCommonCCodes(), "\n\n", StructStatements, "\n\n", VarStatements, "\n\n", FnDeclars, "\n\n", FnStatements],
    file:write_file(OutputFile, Code).

prvFixFunctionForC(#function{exprs = Exprs, var_types = VarTypes} = F, {FnMap, StructMap, GlobalVars}) ->
    Ctx1 = {FnMap, StructMap, maps:merge(GlobalVars, VarTypes)},
    F#function{exprs = prvFixExpressionsForC(Exprs, Ctx1)};
prvFixFunctionForC(Any, _) ->
    Any.

prvFixExpressionsForC(Exprs, Ctx) ->
    ecompilerUtil:expressionMap(fun (E) -> prvFixExpressionForC(E, Ctx) end, Exprs).

prvFixExpressionForC(#op1{operator = '@', operand = Operand, line = Line} = E, {FnMap, StructMap, VarTypes} = Ctx) ->
    case ecompilerType:typeOfExpression(Operand, {VarTypes, FnMap, StructMap, none}) of
        #array_type{} ->
            #op2{operator = '.', op1 = prvFixExpressionForC(Operand, Ctx), op2 = #varref{name = val, line = Line}};
        _ ->
            E
    end;
prvFixExpressionForC(#op1{operand = Operand} = E, Ctx) ->
    E#op1{operand = prvFixExpressionForC(Operand, Ctx)};
prvFixExpressionForC(#op2{op1 = Op1, op2 = Op2} = E, Ctx) ->
    E#op2{op1 = prvFixExpressionForC(Op1, Ctx), op2 = prvFixExpressionForC(Op2, Ctx)};
prvFixExpressionForC(Any, _) ->
    Any.

prvCommonCCodes() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
    "typedef unsigned int usize;\ntypedef int isize;\n"
    "typedef unsigned char u8;\ntypedef char i8;\n"
    "typedef unsigned short u16;\ntypedef short i16;\n"
    "typedef unsigned int u32;\ntypedef int i32;\n"
    "typedef unsigned long u64;\ntypedef long i64;\n"
    "typedef double f64;\ntypedef float f32;\n\n".

prvStatementsToString(Statements, InitCode) -> prvStatementsToString(Statements, InitCode, [], []).

prvStatementsToString([#function{name = Name, param_names = ParamNames, type = Fntype, var_types = VarTypes, exprs = Exprs} | Rest], InitCode, StatementStrs, FnDeclars) ->
    ParamNameAtoms = prvFetchNamesFromVariableReferences(ParamNames),
    PureParams = prvMapToKVList(ParamNameAtoms, maps:with(ParamNameAtoms, VarTypes)),
    PureVars = maps:without(ParamNameAtoms, VarTypes),
    Declar = prvFunctioinDeclarationToString(Name, prvFunctionParametersToString(PureParams), Fntype#fun_type.ret),
    Exprs2 =    case Name =:= main of
                    true -> InitCode ++ Exprs;
                    _ -> Exprs
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

prvVariablesToString([{Name, Type} | Rest], Strs) -> prvVariablesToString(Rest, [prvTypeToCString(Type, Name) | Strs]);
prvVariablesToString([], Strs) -> lists:reverse(Strs).

prvFetchNamesFromVariableReferences(VarrefList) -> lists:map(fun (#varref{name = N}) -> N end, VarrefList).

prvMapToKVList(NameAtoms, ValueMap) -> lists:zip(NameAtoms, ecompilerUtil:getValuesByKeys(NameAtoms, ValueMap)).

prvFunctionReturnTypeToString(#fun_type{params = Params, ret = Rettype}, NameParams) ->
    Paramstr = prvFunctionParamsToStringNoFunctionNames(Params),
    NewNameParams = io_lib:format("~s(~s)", [NameParams, Paramstr]),
    prvTypeToCString(Rettype, NewNameParams);
prvFunctionReturnTypeToString(#basic_type{pdepth = N} = T, NameParams) when N > 0 ->
    prvTypeToCString(T#basic_type{pdepth = N - 1}, NameParams).

%% convert type to C string
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

prvTypeTagToString(struct, Name) -> io_lib:format("struct ~s", [Name]);
prvTypeTagToString(_, Name) -> atom_to_list(Name).

%% convert expression to C string
prvExpressionsToString(Exprs) -> [lists:join("\n", prvExpressionsToString(Exprs, []))].

prvExpressionsToString([Expr | Rest], ExprList) -> prvExpressionsToString(Rest, [prvExpressionToString(Expr, $;) | ExprList]);
prvExpressionsToString([], ExprList) -> lists:reverse(ExprList).

prvExpressionToString(#if_expr{condition = Condition, then = Then, else = Else}, _) ->
    io_lib:format("if (~s) {\n~s\n} else {\n~s}", [prvExpressionToString(Condition, $\s), prvExpressionsToString(Then), prvExpressionsToString(Else)]);
prvExpressionToString(#while_expr{condition = Condition, exprs = Exprs}, _) ->
    io_lib:format("while (~s) {\n~s\n}\n", [prvExpressionToString(Condition, $\s), prvExpressionsToString(Exprs)]);
prvExpressionToString(#op2{operator = '::', op1 = #varref{name = c}, op2 = Op2}, Endchar) ->
    prvExpressionToString(Op2, Endchar);
prvExpressionToString(#op2{operator = Operator, op1 = Op1, op2 = Op2}, Endchar) ->
    io_lib:format("(~s ~s ~s)~c", [prvExpressionToString(Op1, $\s), prvTranslateOperator(Operator), prvExpressionToString(Op2, $\s), Endchar]);
prvExpressionToString(#op1{operator = Operator, operand = Operand}, Endchar) ->
    io_lib:format("(~s ~s)~c", [prvTranslateOperator(Operator), prvExpressionToString(Operand, $\s), Endchar]);
prvExpressionToString(#call{fn = Fn, args = Args}, Endchar) ->
    ArgStr = lists:join(",", lists:map(fun (E) -> prvExpressionToString(E, $\s) end, Args)),
    io_lib:format("~s(~s)~c", [prvExpressionToString(Fn, $\s), ArgStr, Endchar]);
prvExpressionToString(#return{expr = Expr}, Endchar) ->
    io_lib:format("return ~s~c", [prvExpressionToString(Expr, $\s), Endchar]);
prvExpressionToString(#goto{expr = Expr}, Endchar) ->
    io_lib:format("goto ~s~c", [prvExpressionToString(Expr, $\s), Endchar]);
prvExpressionToString(#label{name = Name}, _) ->
    io_lib:format("~s:", [Name]);
prvExpressionToString(#varref{name = Name}, Endchar) ->
    io_lib:format("~s~c", [Name, Endchar]);
prvExpressionToString({Any, _Line, Value}, Endchar) when Any =:= integer; Any =:= float ->
    io_lib:format("~w~c", [Value, Endchar]);
prvExpressionToString({Any, _Line, S}, Endchar) when Any =:= string ->
    io_lib:format("\"~s\"~c", [prvHandleSpecialCharactersInString(S), Endchar]).

-define(SPECIAL_CHARMAP, #{$\n => "\\n", $\r => "\\r", $\t => "\\t", $\f => "\\f", $\b => "\\b"}).

prvHandleSpecialCharactersInString(Str) ->
    lists:map(fun (C) -> maps:get(C, ?SPECIAL_CHARMAP, C) end, Str).

prvTranslateOperator(assign) -> "=";
prvTranslateOperator('rem') -> "%";
prvTranslateOperator('bxor') -> "^";
prvTranslateOperator('bsr') -> ">>";
prvTranslateOperator('bsl') -> "<<";
prvTranslateOperator('band') -> "&";
prvTranslateOperator('bor') -> "|";
prvTranslateOperator('and') -> "&&";
prvTranslateOperator('or') -> "||";
prvTranslateOperator('@') -> "&";
prvTranslateOperator('^') -> "*";
prvTranslateOperator(Any) -> Any.

