-module(eCompile).

-export([compileFromRawAST/2]).

-include("eRecordDefinition.hrl").

-type compileOptions() :: map().

-spec compileFromRawAST(eAST(), compileOptions()) -> {eAST(), variableTypeMap(), eAST()}.
compileFromRawAST(AST, CustomCompileOptions) ->
    CompileOptions = maps:merge(defaultCompileOptions(), CustomCompileOptions),
    {AST2, VariableTypeMap, InitCode0} = eVariable:fetchVariables(AST),
    %io:format(">>> ~p~n", [AST2]),

    {FunctionTypeMap, StructMap0} = eUtil:makeFunctionAndStructMapFromAST(AST2),

    %% struct recursion is not allowed.
    ensureNoRecursiveStruct(StructMap0),
    #{pointer_width := PointerWidth} = CompileOptions,
    %% calculate struct size, filed offsets
    AST3 = eSize:fillStructInformation(AST2, {StructMap0, PointerWidth}),

    %% struct size is updated, so StructMap needs to be updated, too
    {_, StructMap1} = eUtil:makeFunctionAndStructMapFromAST(AST3),
    %% expand sizeof expression
    Ctx1 = {StructMap1, PointerWidth},
    AST4 = eSize:expandSizeOf(AST3, Ctx1),

    %% Initializing code for global variables are not in main ast, do not forget it
    InitCode1 = eSize:expandSizeofInExpressions(InitCode0, Ctx1),
    %% sizeof expressions are expanded, so StructMap needs to be updated
    {_, StructMap2} = eUtil:makeFunctionAndStructMapFromAST(AST4),
    %% type checking
    Maps = {FunctionTypeMap, StructMap2},
    eType:checkTypesInAST(AST4, VariableTypeMap, Maps),
    eType:checkTypesInASTNodeList(InitCode1, VariableTypeMap, Maps),
    %% expand init exprs like A{a=1} and {1,2,3}
    AST5 = eInitExpression:expandInFunctions(AST4, StructMap2),

    InitCode2 = eInitExpression:expandInitExpressions(InitCode1, StructMap2),
    {AST5, VariableTypeMap, InitCode2}.

-spec defaultCompileOptions() -> compileOptions().
defaultCompileOptions() ->
    #{pointer_width => 8}.

-spec ensureNoRecursiveStruct(structTypeMap()) -> ok.
ensureNoRecursiveStruct(StructTypeMap) ->
    maps:foreach(fun (_, S) -> checkStructRecursive(S, StructTypeMap) end, StructTypeMap).

-spec checkStructRecursive(#struct{}, structTypeMap()) -> ok.
checkStructRecursive(#struct{name = Name, line = Line} = Struct, StructTypeMap) ->
    case checkStructObject(Struct, StructTypeMap, []) of
        ok ->
            ok;
        {recur, Chain} ->
            throw({Line, eUtil:fmt("recursive struct ~s -> ~w", [Name, Chain])})
    end.

-spec checkStructObject(#struct{}, structTypeMap(), [atom()]) -> ok | {recur, [any()]}.
checkStructObject(#struct{name = Name, fieldTypeMap = FieldTypes}, StructMap, UsedStructs) ->
    checkStructFields(maps:to_list(FieldTypes), StructMap, [Name | UsedStructs]).

-spec checkStructFields([{atom(), eType()}], structTypeMap(), [atom()]) -> ok | {recur, [any()]}.
checkStructFields([{_, FieldType} | RestFields], StructMap, UsedStructs) ->
    case containStruct(FieldType) of
        {yes, StructName} ->
            case eUtil:valueInList(StructName, UsedStructs) of
                true ->
                    {recur, lists:reverse([StructName | UsedStructs])};
                false ->
                    case checkStructObject(maps:get(StructName, StructMap), StructMap, UsedStructs) of
                        ok ->
                            checkStructFields(RestFields, StructMap, UsedStructs);
                        {recur, _} = Any ->
                            Any
                    end
            end;
        no ->
            checkStructFields(RestFields, StructMap, UsedStructs)
    end;
checkStructFields([], _, _) ->
    ok.

-spec containStruct(eType()) -> {yes, atom()} | no.
containStruct(#basicType{class = struct, pdepth = 0, tag = Name}) ->
    {yes, Name};
containStruct(#arrayType{elemtype = BaseType}) ->
    containStruct(BaseType);
containStruct(_) ->
    no.
