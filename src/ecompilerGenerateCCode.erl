-module(ecompilerGenerateCCode).

-export([generateCCode/4]).

-include("ecompilerFrameDef.hrl").

-type genCContext() :: {functionTypeMap(), structTypeMap(), variableTypeMap()}.

-spec generateCCode(eAST(), variableTypeMap(), eAST(), string()) -> ok.
generateCCode(AST, GlobalVars, InitCode, OutputFile) ->
    {FunctionTypeMap, StructMap} = ecompilerUtil:makeFunctionAndStructMapFromAST(AST),
    Context = {FunctionTypeMap, StructMap, GlobalVars},
    AST2 = lists:map(fun (A) -> fixFunctionForC(A, Context) end, AST),
    InitCode2 = fixExpressionsForC(InitCode, Context),
    %io:format(">>>~p~n", [AST2]),
    %% struct definition have to be before function declarations
    CheckStruct = fun (A) -> element(1, A) =:= struct end,
    {StructAST, FunctionAST} = lists:partition(CheckStruct, AST2),
    {StructStatements, []} = statementsToString(StructAST, []),
    {FnStatements, FunctionDeclarations} = statementsToString(FunctionAST, InitCode2),
    VarStatements = variableMapToString(GlobalVars),
    Code = lists:join("\n\n", [commonCCodes(), StructStatements, VarStatements, FunctionDeclarations, FnStatements]),
    ok = file:write_file(OutputFile, Code).

-spec fixFunctionForC(eExpression(), genCContext()) -> eExpression().
fixFunctionForC(#function{statements = Expressions, variableTypeMap = VarTypes} = F, {FunctionTypeMap, StructMap, GlobalVars}) ->
    F#function{statements = fixExpressionsForC(Expressions, {FunctionTypeMap, StructMap, maps:merge(GlobalVars, VarTypes)})};
fixFunctionForC(Any, _) ->
    Any.

-spec fixExpressionsForC(eAST(), genCContext()) -> eAST().
fixExpressionsForC(Expressions, Context) ->
    ecompilerUtil:expressionMap(fun (E) -> fixExpressionForC(E, Context) end, Expressions).

-spec fixExpressionForC(eExpression(), genCContext()) -> eExpression().
fixExpressionForC(#operatorExpression1{operator = '@', operand = Operand, line = Line} = E, {FunctionTypeMap, StructMap, VarTypes} = Context) ->
    case ecompilerType:typeOfASTNode(Operand, {VarTypes, FunctionTypeMap, StructMap, #{}}) of
        #arrayType{} ->
            #operatorExpression2{operator = '.', operand1 = fixExpressionForC(Operand, Context), operand2 = #variableReference{name = value, line = Line}};
        _ ->
            E
    end;
fixExpressionForC(#operatorExpression1{operand = Operand} = E, Context) ->
    E#operatorExpression1{operand = fixExpressionForC(Operand, Context)};
fixExpressionForC(#operatorExpression2{operand1 = Operand1, operand2 = Operand2} = E, Context) ->
    E#operatorExpression2{operand1 = fixExpressionForC(Operand1, Context), operand2 = fixExpressionForC(Operand2, Context)};
fixExpressionForC(Any, _) ->
    Any.

-spec commonCCodes() -> string().
commonCCodes() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n"
    "typedef unsigned int usize;\ntypedef int isize;\ntypedef unsigned char u8;\ntypedef char i8;\n"
    "typedef unsigned short u16;\ntypedef short i16;\ntypedef unsigned int u32;\ntypedef int i32;\n"
    "typedef unsigned long u64;\ntypedef long i64;\ntypedef double f64;\ntypedef float f32;\n\n".

statementsToString(Statements, InitCode) ->
    statementsToString(Statements, InitCode, [], []).

statementsToString([#function{name = Name, parameterNames = ParamNames, type = FunctionType, variableTypeMap = VarTypes, statements = Expressions} | Rest], InitCode, StatementStringList, FnDeclarationList) ->
    ParamNameAtoms = fetchNamesFromVariableReferences(ParamNames),
    PureParams = mapToKVList(ParamNameAtoms, maps:with(ParamNameAtoms, VarTypes)),
    PureVars = maps:without(ParamNameAtoms, VarTypes),
    Declarations = functionDeclarationToString(Name, functionParametersToString(PureParams), FunctionType#functionType.ret),
    Expressions2 = case Name =:= main of
                       true ->
                           InitCode ++ Expressions;
                       false ->
                           Expressions
                   end,
    S = io_lib:format("~s~n{~n~s~n~n~s~n}~n~n", [Declarations, variableMapToString(PureVars), expressionsToString(Expressions2)]),
    statementsToString(Rest, InitCode, [S | StatementStringList], [Declarations ++ ";\n" | FnDeclarationList]);
statementsToString([#struct{name = Name, fieldTypeMap = FieldTypes, fieldNames = FieldNames} | Rest], InitCode, StatementStringList, FnDeclarationList) ->
    FieldList = mapToKVList(fetchNamesFromVariableReferences(FieldNames), FieldTypes),
    S = io_lib:format("struct ~s {~n~s~n};~n~n", [Name, variableListToString(FieldList)]),
    statementsToString(Rest, InitCode, [S | StatementStringList], FnDeclarationList);
statementsToString([], _, StatementStringList, FnDeclarationList) ->
    {lists:reverse(StatementStringList), lists:reverse(FnDeclarationList)}.

functionDeclarationToString(Name, ParamStr, #basicType{pdepth = N} = ReturnType) when N > 0 ->
    functionReturnTypeToString(ReturnType, io_lib:format("(*~s(~s))", [Name, ParamStr]));
functionDeclarationToString(Name, ParamStr, #functionType{} = ReturnType) ->
    functionReturnTypeToString(ReturnType, io_lib:format("(*~s(~s))", [Name, ParamStr]));
functionDeclarationToString(Name, ParamStr, ReturnType) ->
    typeToCString(ReturnType, io_lib:format("~s(~s)", [Name, ParamStr])).

functionParametersToString(NameTypePairs) ->
    lists:join(",", lists:map(fun ({N, T}) -> typeToCString(T, N) end, NameTypePairs)).

functionParamsToStringNoFunctionNames(Types) ->
    lists:join(",", lists:map(fun (T) -> typeToCString(T, "") end, Types)).

%% order is not necessary for vars
variableMapToString(VarsMap) when is_map(VarsMap) ->
    lists:flatten(lists:join(";\n", variablesToString(maps:to_list(VarsMap), [])), ";").

variableListToString(VarList) when is_list(VarList) ->
    lists:flatten(lists:join(";\n", variablesToString(VarList, [])), ";").

variablesToString([{Name, Type} | Rest], Strs) ->
    variablesToString(Rest, [typeToCString(Type, Name) | Strs]);
variablesToString([], Strs) ->
    lists:reverse(Strs).

fetchNamesFromVariableReferences(VarrefList) ->
    lists:map(fun (#variableReference{name = N}) -> N end, VarrefList).

mapToKVList(NameAtoms, ValueMap) ->
    lists:zip(NameAtoms, ecompilerUtil:getValuesByKeys(NameAtoms, ValueMap)).

functionReturnTypeToString(#functionType{parameters = Params, ret = ReturnType}, NameParams) ->
    ParametersString = functionParamsToStringNoFunctionNames(Params),
    NewNameParams = io_lib:format("~s(~s)", [NameParams, ParametersString]),
    typeToCString(ReturnType, NewNameParams);
functionReturnTypeToString(#basicType{pdepth = N} = T, NameParams) when N > 0 ->
    typeToCString(T#basicType{pdepth = N - 1}, NameParams).

%% convert type to C string
-spec typeToCString(eExpression(), iolist()) -> iolist().
typeToCString(#arrayType{length = Len, elemtype = ElementType}, VariableName) ->
    io_lib:format("struct {~s value[~w];} ~s", [typeToCString(ElementType, ""), Len, VariableName]);
typeToCString(#basicType{class = Class, tag = Tag, pdepth = Depth}, VariableName) when Depth > 0 ->
    io_lib:format("~s~s ~s", [typeTagToString(Class, Tag), lists:duplicate(Depth, "*"), VariableName]);
typeToCString(#basicType{class = Class, tag = Tag, pdepth = 0}, VariableName) ->
    io_lib:format("~s ~s", [typeTagToString(Class, Tag), VariableName]);
typeToCString(#functionType{parameters = Params, ret = ReturnType}, VariableName) ->
    ParameterString = functionParamsToStringNoFunctionNames(Params),
    NameParams = io_lib:format("(*~s)(~s)", [VariableName, ParameterString]),
    typeToCString(ReturnType, NameParams).

typeTagToString(struct, Name) ->
    io_lib:format("struct ~s", [Name]);
typeTagToString(_, Name) ->
    atom_to_list(Name).

%% convert expression to C string
expressionsToString(Expressions) ->
    [lists:join("\n", expressionsToString(Expressions, []))].

expressionsToString([Expression | Rest], ExprList) ->
    expressionsToString(Rest, [expressionToString(Expression, $;) | ExprList]);
expressionsToString([], ExprList) ->
    lists:reverse(ExprList).

-spec expressionToString(eExpression(), char()) -> iolist().
expressionToString(#ifStatement{condition = Condition, then = Then, else = Else}, _) ->
    io_lib:format("if (~s) {\n~s\n} else {\n~s}", [expressionToString(Condition, $\s), expressionsToString(Then), expressionsToString(Else)]);
expressionToString(#whileStatement{condition = Condition, statements = Expressions}, _) ->
    io_lib:format("while (~s) {\n~s\n}\n", [expressionToString(Condition, $\s), expressionsToString(Expressions)]);
expressionToString(#operatorExpression2{operator = '::', operand1 = #variableReference{name = c}, operand2 = Operand2}, EndChar) ->
    expressionToString(Operand2, EndChar);
expressionToString(#operatorExpression2{operator = Operator, operand1 = Operand1, operand2 = Operand2}, EndChar) ->
    io_lib:format("(~s ~s ~s)~c", [expressionToString(Operand1, $\s), translateOperator(Operator), expressionToString(Operand2, $\s), EndChar]);
expressionToString(#operatorExpression1{operator = Operator, operand = Operand}, EndChar) ->
    io_lib:format("(~s ~s)~c", [translateOperator(Operator), expressionToString(Operand, $\s), EndChar]);
expressionToString(#callExpression{fn = Fn, args = Arguments}, EndChar) ->
    ArgumentString = lists:join(",", lists:map(fun (E) -> expressionToString(E, $\s) end, Arguments)),
    io_lib:format("~s(~s)~c", [expressionToString(Fn, $\s), ArgumentString, EndChar]);
expressionToString(#returnStatement{expression = Expression}, EndChar) ->
    io_lib:format("return ~s~c", [expressionToString(Expression, $\s), EndChar]);
expressionToString(#gotoStatement{expression = Expression}, EndChar) ->
    io_lib:format("goto ~s~c", [expressionToString(Expression, $\s), EndChar]);
expressionToString(#gotoLabel{name = Name}, _) ->
    io_lib:format("~s:", [Name]);
expressionToString(#variableReference{name = Name}, EndChar) ->
    io_lib:format("~s~c", [Name, EndChar]);
expressionToString(#typeConvert{expression = Expression, type = TargetType}, EndChar) ->
    io_lib:format("((~s) ~s)~c", [typeToCString(TargetType, ""), expressionToString(Expression, $\s), EndChar]);
expressionToString({Any, _Line, Value}, EndChar) when Any =:= integer; Any =:= float ->
    io_lib:format("~w~c", [Value, EndChar]);
expressionToString({Any, _Line, S}, EndChar) when Any =:= string ->
    io_lib:format("\"~s\"~c", [handleSpecialCharactersInString(S), EndChar]).

-define(SPECIAL_CHARACTER_MAP, #{$\n => "\\n", $\r => "\\r", $\t => "\\t", $\f => "\\f", $\b => "\\b"}).

handleSpecialCharactersInString(String) ->
    lists:map(fun (C) -> maps:get(C, ?SPECIAL_CHARACTER_MAP, C) end, String).

-spec translateOperator(atom()) -> string() | atom().
translateOperator(assign) ->
    "=";
translateOperator('rem') ->
    "%";
translateOperator('bxor') ->
    "^";
translateOperator('bsr') ->
    ">>";
translateOperator('bsl') ->
    "<<";
translateOperator('band') ->
    "&";
translateOperator('bor') ->
    "|";
translateOperator('and') ->
    "&&";
translateOperator('or') ->
    "||";
translateOperator('@') ->
    "&";
translateOperator('^') ->
    "*";
translateOperator(Any) ->
    Any.
