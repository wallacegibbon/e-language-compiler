-module(ecompilerCompile).

-export([compileFromRawAST/2]).

-include("./ecompilerFrameDef.hrl").

compileFromRawAST(Ast, CustomCompileOptions) ->
    CompileOptions = maps:merge(prvDefaultCompileOptions(), CustomCompileOptions),

    Ast1 = ecompilerFillConstant:parseAndRemoveConstants(Ast),
    %io:format(">>> ~p~n", [Ast1]),
    {Ast2, Vars, InitCode0} = ecompilerCollectVariable:fetchVariables(Ast1),

    %io:format(">>> ~p~n", [Ast2]),
    {FnMap, StructMap0} = ecompilerUtil:makeFunctionAndStructMapFromAST(Ast2),

    %% struct recursion is not allowed.
    prvCheckStructRecursive(StructMap0),
    #{pointer_width := PointerWidth} = CompileOptions,
    Ctx0 = {StructMap0, PointerWidth},
    %% calculate struct size, filed offsets
    Ast3 = ecompilerFillSize:fillStructInformation(Ast2, Ctx0),

    %% struct size is updated, so StructMap needs to be updated, too
    {_, StructMap1} = ecompilerUtil:makeFunctionAndStructMapFromAST(Ast3),
    %% expand sizeof expression
    Ctx1 = {StructMap1, PointerWidth},
    Ast4 = ecompilerFillSize:expandSizeOf(Ast3, Ctx1),

    %% initcode is not in main ast, do not forget it
    InitCode1 = ecompilerFillSize:expandSizeofInExpressions(InitCode0, Ctx1),
    %% sizeof expressions are expanded, so StructMap needs to be updated
    {_, StructMap2} = ecompilerUtil:makeFunctionAndStructMapFromAST(Ast4),
    %% type checking
    Maps = {FnMap, StructMap2},
    ecompilerType:checkTypesInAST(Ast4, Vars, Maps),
    ecompilerType:checkTypesInExpressions(InitCode1, Vars, Maps),
    %% expand init exprs like A{a=1} and {1,2,3}
    Ast5 = ecompilerExpandInitExpression:expandInitExpressionInFunctions(Ast4, StructMap2),

    InitCode2 = ecompilerExpandInitExpression:expandInitExpressions(InitCode1, StructMap2),
    {Ast5, Vars, InitCode2, FnMap}.

prvDefaultCompileOptions() -> #{pointer_width => 8}.

prvCheckStructRecursive(StructMap) -> maps:map(fun (_, S) -> prvCheckStructObject(S, StructMap, []) end, StructMap).

prvCheckStructObject(#struct{name = Name, field_types = FieldTypes, line = Line}, StructMap, UsedStructs) ->
    try
        prvCheckStructField(maps:to_list(FieldTypes), StructMap, [Name | UsedStructs])
    catch
        {recur, Chain} ->
            throw({Line, ecompilerUtil:flatfmt("recursive struct ~s -> ~w", [Name, Chain])})
    end;
prvCheckStructObject(_, _, _) ->
    ok.

prvCheckStructField([{_, FieldType} | Rest], StructMap, UsedStructs) ->
    case prvContainStruct(FieldType) of
        {yes, StructName} ->
            case ecompilerUtil:valueInList(StructName, UsedStructs) of
                false ->
                    prvCheckStructObject(maps:get(StructName, StructMap), StructMap, UsedStructs);
                true ->
                    throw({recur, lists:reverse(UsedStructs)})
            end;
        no ->
            ok
    end,
    prvCheckStructField(Rest, StructMap, UsedStructs);
prvCheckStructField([], _, _) ->
    ok.

prvContainStruct(#basic_type{class = struct, pdepth = 0, tag = Name}) -> {yes, Name};
prvContainStruct(#array_type{elemtype = BaseT}) -> prvContainStruct(BaseT);
prvContainStruct(_) -> no.