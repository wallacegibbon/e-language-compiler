-module(ecompilerCompile).

-export([compileFromRawAST/2]).

-include("ecompilerFrameDef.hrl").

-type compileOptions() :: map().

-spec compileFromRawAST(eAST(), compileOptions()) -> {eAST(), variableTypeMap(), eAST(), functionTypeMap()}.
compileFromRawAST(AST, CustomCompileOptions) ->
    CompileOptions = maps:merge(defaultCompileOptions(), CustomCompileOptions),

    AST1 = ecompilerFillConstant:parseAndRemoveConstants(AST),
    %io:format(">>> ~p~n", [Ast1]),
    {AST2, VariableTypeMap, InitCode0} = ecompilerCollectVariable:fetchVariables(AST1),
    %io:format(">>> ~p~n", [Ast2]),

    {FunctionTypeMap, StructMap0} = ecompilerUtil:makeFunctionAndStructMapFromAST(AST2),

    %% struct recursion is not allowed.
    checkStructRecursive(StructMap0),
    #{pointer_width := PointerWidth} = CompileOptions,
    Ctx0 = {StructMap0, PointerWidth},
    %% calculate struct size, filed offsets
    AST3 = ecompilerFillSize:fillStructInformation(AST2, Ctx0),

    %% struct size is updated, so StructMap needs to be updated, too
    {_, StructMap1} = ecompilerUtil:makeFunctionAndStructMapFromAST(AST3),
    %% expand sizeof expression
    Ctx1 = {StructMap1, PointerWidth},
    AST4 = ecompilerFillSize:expandSizeOf(AST3, Ctx1),

    %% initcode is not in main ast, do not forget it
    InitCode1 = ecompilerFillSize:expandSizeofInExpressions(InitCode0, Ctx1),
    %% sizeof expressions are expanded, so StructMap needs to be updated
    {_, StructMap2} = ecompilerUtil:makeFunctionAndStructMapFromAST(AST4),
    %% type checking
    Maps = {FunctionTypeMap, StructMap2},
    ecompilerType:checkTypesInAST(AST4, VariableTypeMap, Maps),
    ecompilerType:checkTypesInExpressions(InitCode1, VariableTypeMap, Maps),
    %% expand init exprs like A{a=1} and {1,2,3}
    AST5 = ecompilerExpandInitExpression:expandInitExpressionInFunctions(AST4, StructMap2),

    InitCode2 = ecompilerExpandInitExpression:expandInitExpressions(InitCode1, StructMap2),
    {AST5, VariableTypeMap, InitCode2, FunctionTypeMap}.

-spec defaultCompileOptions() -> compileOptions().
defaultCompileOptions() ->
    #{pointer_width => 8}.

-spec checkStructRecursive(structTypeMap()) -> ok.
checkStructRecursive(StructTypeMap) ->
    maps:foreach(fun (_, S) -> checkStructObject(S, StructTypeMap, []) end, StructTypeMap).

-spec checkStructObject(#struct{}, structTypeMap(), [atom()]) -> ok.
checkStructObject(#struct{name = Name, field_types = FieldTypes, line = Line}, StructMap, UsedStructs) ->
    try
        checkStructField(maps:to_list(FieldTypes), StructMap, [Name | UsedStructs])
    catch
        {recur, Chain} ->
            throw({Line, ecompilerUtil:flatfmt("recursive struct ~s -> ~w", [Name, Chain])})
    end;
checkStructObject(_, _, _) ->
    ok.

-spec checkStructField([{atom(), eType()}], structTypeMap(), [atom()]) -> ok.
checkStructField([{_, FieldType} | RestFields], StructMap, UsedStructs) ->
    case containStruct(FieldType) of
        {yes, StructName} ->
            case ecompilerUtil:valueInList(StructName, UsedStructs) of
                true ->
                    throw({recur, lists:reverse(UsedStructs)});
                false ->
                    checkStructObject(maps:get(StructName, StructMap), StructMap, UsedStructs)
            end;
        no ->
            ok
    end,
    checkStructField(RestFields, StructMap, UsedStructs);
checkStructField([], _, _) ->
    ok.

-spec containStruct(eType()) -> {yes, atom()} | no.
containStruct(#basic_type{class = struct, pdepth = 0, tag = Name}) ->
    {yes, Name};
containStruct(#array_type{elemtype = BaseType}) ->
    containStruct(BaseType);
containStruct(_) ->
    no.
