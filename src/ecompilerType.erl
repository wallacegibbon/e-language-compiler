-module(ecompilerType).

-export([checkTypesInAST/3, checkTypesInExpressions/3, typeOfExpression/2]).

-include("./ecompilerFrameDef.hrl").

-spec checkTypesInAST(eAST(), variableTypeMap(), {functionTypeMap(), structTypeMap()}) -> ok.
checkTypesInAST([#function{var_types = VarTypes, exprs = Expressions, type = Fntype} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    prvCheckTypes(maps:values(VarTypes), StructMap),
    prvCheckType(Fntype#fun_type.ret, StructMap),
    CurrentVars = maps:merge(GlobalVarTypes, VarTypes),
    Ctx = {CurrentVars, FunctionTypeMap, StructMap, Fntype#fun_type.ret},
    typeOfExpressions(Expressions, Ctx),
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([#struct{name = Name, field_types = FieldTypes, field_names = FieldNames, field_defaults = FieldDefaults} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    prvCheckTypes(maps:values(FieldTypes), StructMap),
    Ctx = {GlobalVarTypes, FunctionTypeMap, StructMap, none},
    %% check the default values for fields
    InitFieldNames = ecompilerUtil:filterVariableReferenceInMap(FieldNames, FieldDefaults),
    prvCheckTypesInStructFields(InitFieldNames, FieldTypes, FieldDefaults, Name, Ctx),
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([_ | Rest], GlobalVarTypes, Maps) ->
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([], _, _) ->
    ok.

-type typeOfContext() :: {variableTypeMap(), functionTypeMap(), structTypeMap(), functionReturnTypeMap()}.


-spec checkTypesInExpressions([eExpression()], variableTypeMap(), {functionTypeMap(), structTypeMap()}) -> ok.
checkTypesInExpressions(Expressions, GlobalVarTypes, {FunctionTypeMap, StructMap}) ->
    typeOfExpressions(Expressions, {GlobalVarTypes, FunctionTypeMap, StructMap, none}),
    ok.

-spec typeOfExpressions([eExpression()], typeOfContext()) -> [eType()].
typeOfExpressions(Expressions, Ctx) -> lists:map(fun (Expression) -> typeOfExpression(Expression, Ctx) end, Expressions).

-spec typeOfExpression(eExpression(), typeOfContext()) -> eType().
typeOfExpression(#op2{operator = assign, op1 = Operand1, op2 = Operand2, line = Line}, {_, _, StructMap, _} = Ctx) ->
    TypeofOp1 = case Operand1 of
                    #op2{operator = '.', op1 = SubOp1, op2 = SubOp2} ->     prvTypeOfStructField(typeOfExpression(SubOp1, Ctx), SubOp2, StructMap, Line);
                    #op1{operator = '^', operand = SubOp} ->                prvDecreasePointerDepth(typeOfExpression(SubOp, Ctx), Line);
                    #varref{} ->                                            typeOfExpression(Operand1, Ctx);
                    Any ->                                                  throw({Line, ecompilerUtil:flatfmt("invalid left value (~s)", [ecompilerUtil:expressionToString(Any)])})
                end,
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case prvCompareType(TypeofOp1, TypeofOp2) of
        false ->    throw({Line, ecompilerUtil:flatfmt("type mismatch in \"~s = ~s\"", [prvTypeToString(TypeofOp1), prvTypeToString(TypeofOp2)])});
        _ ->        TypeofOp1
    end;
typeOfExpression(#op2{operator = '.', op1 = Operand1, op2 = Operand2, line = Line}, {_, _, StructMap, _} = Ctx) ->
    prvTypeOfStructField(typeOfExpression(Operand1, Ctx), Operand2, StructMap, Line);
typeOfExpression(#op2{operator = '::', op1 = #varref{name = self}, op2 = Operand2}, Ctx) ->
    typeOfExpression(Operand2, Ctx);
typeOfExpression(#op2{operator = '::', op1 = Operand1, op2 = Operand2, line = Line}, _) ->
    ecompilerUtil:assert(is_record(Operand1, varref), {Line, "invalid usage on ::"}),
    ecompilerUtil:assert(is_record(Operand2, varref), {Line, "invalid usage on ::"}),
    #varref{name = ModName} = Operand1,
    #varref{name = FunName} = Operand2,
    try ecompiler:prvQueryFunctionInModule(ModName, FunName) of
        {error, moduleNotFound, _} ->   throw({Line, ecompilerUtil:flatfmt("module ~s is not found", [ModName])});
        {error, functionNotFound} ->    throw({Line, ecompilerUtil:flatfmt("~s:~s is not found", [ModName, FunName])});
        {ok, Type} ->                   Type
    catch
        E ->
            throw({Line, E})
    end;
typeOfExpression(#op2{operator = '+', op1 = Operand1, op2 = Operand2, line = Line}, Ctx) ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case prvAreBothNumberSameType(TypeofOp1, TypeofOp2) of
        false ->
            case prvIsPointerAndInt(TypeofOp1, TypeofOp2) of
                false ->            throw({Line, prvTypeErrorOfOp2('+', TypeofOp1, TypeofOp2)});
                {true, Ptype} ->    Ptype
            end;
        {true, T} ->
            T
    end;
%% integer + pointer is valid, but integer - pointer is invalid
typeOfExpression(#op2{operator = '-', op1 = Operand1, op2 = Operand2, line = Line}, Ctx) ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case prvAreBothNumberSameType(TypeofOp1, TypeofOp2) of
        false ->
            case prvIsPointerAndIntOrdered(TypeofOp1, TypeofOp2) of
                false ->            throw({Line, prvTypeErrorOfOp2('-', TypeofOp1, TypeofOp2)});
                {true, Ptype} ->    Ptype
            end;
        {true, T} ->
            T
    end;
typeOfExpression(#op2{operator = Operator, op1 = Operand1, op2 = Operand2, line = Line}, Ctx) when Operator =:= '*'; Operator =:= '/' ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case prvAreBothNumberSameType(TypeofOp1, TypeofOp2) of
        false ->        throw({Line, prvTypeErrorOfOp2(Operator, TypeofOp1, TypeofOp2)});
        {true, T} ->    T
    end;
%% the left operators are: and, or, band, bor, bxor, bsl, bsr, >, <, ...
typeOfExpression(#op2{operator = Operator, op1 = Operand1, op2 = Operand2, line = Line}, Ctx) ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case prvAreBothIntegers(TypeofOp1, TypeofOp2) of
        false ->        throw({Line, prvTypeErrorOfOp2(Operator, TypeofOp1, TypeofOp2)});
        true ->         TypeofOp1
    end;
typeOfExpression(#op1{operator = '^', operand = Operand, line = Line}, Ctx) ->
    case typeOfExpression(Operand, Ctx) of
        #basic_type{} = T ->    prvDecreasePointerDepth(T, Line);
        _ ->                    throw({Line, ecompilerUtil:flatfmt("invalid \"^\" on operand ~s", [ecompilerUtil:expressionToString(Operand)])})
    end;
typeOfExpression(#op1{operator = '@', operand = Operand, line = Line}, {_, _, StructMap, _} = Ctx) ->
    case Operand of
        #op2{operator = '.', op1 = Operand1, op2 = Operand2} ->
            T = prvTypeOfStructField(typeOfExpression(Operand1, Ctx), Operand2, StructMap, Line),
            prvIncreasePointerDepth(T, Line);
        #varref{} ->
            prvIncreasePointerDepth(typeOfExpression(Operand, Ctx), Line);
        _ ->
            throw({Line, ecompilerUtil:flatfmt("invalid \"@\" on operand ~s", [ecompilerUtil:expressionToString(Operand)])})
    end;
typeOfExpression(#op1{operand = Operand}, Ctx) ->
    typeOfExpression(Operand, Ctx);
typeOfExpression(#call{fn = FunExpr, args = Arguments, line = Line}, Ctx) ->
    ArgsTypes = typeOfExpressions(Arguments, Ctx),
    case typeOfExpression(FunExpr, Ctx) of
        #fun_type{params = FnParamTypes, ret = FnRetType} ->
            case prvCompareTypes(ArgsTypes, FnParamTypes) of
                false ->    throw({Line, prvArgumentsErrorInformation(FnParamTypes, ArgsTypes)});
                true ->     FnRetType
            end;
        T ->
            throw({Line, ecompilerUtil:flatfmt("invalid function expr: ~s", [prvTypeToString(T)])})
    end;
typeOfExpression(#if_expr{condition = Condition, then = Then, else = Else, line = Line}, Ctx) ->
    typeOfExpression(Condition, Ctx),
    typeOfExpressions(Then, Ctx),
    typeOfExpressions(Else, Ctx),
    ecompilerUtil:voidType(Line);
typeOfExpression(#while_expr{condition = Condition, exprs = Expressions, line = Line}, Ctx) ->
    typeOfExpression(Condition, Ctx),
    typeOfExpressions(Expressions, Ctx),
    ecompilerUtil:voidType(Line);
typeOfExpression(#return{expr = Expression, line = Line}, {_, _, _, FnRetType} = Ctx) ->
    RealRet = typeOfExpression(Expression, Ctx),
    case prvCompareType(RealRet, FnRetType) of
        false ->    throw({Line, ecompilerUtil:flatfmt("ret type should be (~s), not (~s)", [prvTypeToString(FnRetType), prvTypeToString(RealRet)])});
        true ->     RealRet
    end;
typeOfExpression(#varref{name = Name, line = Line}, {VarTypes, FunctionTypeMap, StructMap, _}) ->
    Type =  case maps:find(Name, VarTypes) of
                error ->
                    case maps:find(Name, FunctionTypeMap) of
                        error ->        throw({Line, ecompilerUtil:flatfmt("variable ~s is undefined", [Name])});
                        {ok, T} ->      T
                    end;
                {ok, T} ->
                    T
            end,
    prvCheckType(Type, StructMap),
    Type;
typeOfExpression(#array_init{elements = Elements, line = Line}, Ctx) ->
    ElementTypes = typeOfExpressions(Elements, Ctx),
    case areSameType(ElementTypes) of
        true ->     #array_type{elemtype = hd(ElementTypes), len = length(ElementTypes), line = Line};
        _ ->        throw({Line, ecompilerUtil:flatfmt("array init type conflict: {~s}", [prvJoinTypesToString(ElementTypes)])})
    end;
typeOfExpression(#struct_init{name = StructName, field_names = InitFieldNames, field_values = InitFieldValues, line = Line}, {_, _, StructMap, _} = Ctx) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{field_types = FieldTypes}} ->
            prvCheckTypesInStructFields(InitFieldNames, FieldTypes, InitFieldValues, StructName, Ctx),
            #basic_type{class = struct, tag = StructName, pdepth = 0, line = Line};
        _ ->
            throw({Line, ecompilerUtil:flatfmt("struct ~s is not found", [StructName])})
    end;
typeOfExpression(#sizeof{line = Line}, _) ->
    #basic_type{class = integer, pdepth = 0, tag = i64, line = Line};
typeOfExpression(#goto{line = Line}, _) ->
    ecompilerUtil:voidType(Line);
typeOfExpression(#label{line = Line}, _) ->
    ecompilerUtil:voidType(Line);
typeOfExpression({float, Line, _}, _) ->
    #basic_type{class = float, pdepth = 0, tag = f64, line = Line};
typeOfExpression({integer, Line, _}, _) ->
    #basic_type{class = integer, pdepth = 0, tag = i64, line = Line};
typeOfExpression({string, Line, _}, _) ->
    #basic_type{class = integer, pdepth = 1, tag = i8, line = Line}.

-spec prvArgumentsErrorInformation(variableTypeMap(), variableTypeMap()) -> string().
prvArgumentsErrorInformation(FnParamTypes, ArgsTypes) ->
    ecompilerUtil:flatfmt("args should be (~s), not (~s)", [prvJoinTypesToString(FnParamTypes), prvJoinTypesToString(ArgsTypes)]).

-spec prvIncreasePointerDepth(eType(), integer()) -> eType().
prvIncreasePointerDepth(#basic_type{pdepth = Pdepth} = T, _) ->
    T#basic_type{pdepth = Pdepth + 1};
prvIncreasePointerDepth(#array_type{elemtype = #basic_type{} = T}, OpLine) ->
    prvIncreasePointerDepth(T, OpLine);
prvIncreasePointerDepth(T, OpLine) ->
    throw({OpLine, ecompilerUtil:flatfmt("'@' on type ~s is invalid", [prvTypeToString(T)])}).

-spec prvDecreasePointerDepth(eType(), integer()) -> eType().
prvDecreasePointerDepth(#basic_type{pdepth = Pdepth} = T, _) when Pdepth > 0 ->
    T#basic_type{pdepth = Pdepth - 1};
prvDecreasePointerDepth(T, OpLine) ->
    throw({OpLine, ecompilerUtil:flatfmt("'^' on type ~s is invalid", [prvTypeToString(T)])}).

-spec prvCheckTypesInStructFields([#varref{}], variableTypeMap(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
prvCheckTypesInStructFields(FieldNames, FieldTypes, ValMap, StructName, Ctx) ->
    lists:foreach(fun (V) -> prvCheckStructField(V, FieldTypes, ValMap, StructName, Ctx) end, FieldNames).

-spec prvCheckStructField(#varref{}, variableTypeMap(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
prvCheckStructField(#varref{name = FieldName, line = Line}, FieldTypes, ValMap, StructName, {_, _, StructMap, _} = Ctx) ->
    {ok, Val} = maps:find(FieldName, ValMap),
    ExpectedType = prvGetFieldType(FieldName, FieldTypes, StructName, Line),
    prvCheckType(ExpectedType, StructMap),
    GivenType = typeOfExpression(Val, Ctx),
    case prvCompareType(ExpectedType, GivenType) of
        false ->    throw({Line, ecompilerUtil:flatfmt("~s.~s type error: ~s = ~s", [StructName, FieldName, prvTypeToString(ExpectedType), prvTypeToString(GivenType)])});
        _ ->        ok
    end.

-spec areSameType([eType()]) -> boolean().
areSameType([TargetType, TargetType | Rest]) ->     areSameType([TargetType | Rest]);
areSameType([_]) ->                                 true;
areSameType(_) ->                                   false.

-spec prvTypeOfStructField(eType(), #varref{}, structTypeMap(), integer()) -> eType().
prvTypeOfStructField(#basic_type{class = struct, tag = StructName, pdepth = 0}, #varref{name = FieldName}, StructMap, Line) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{field_types = FieldTypes}} ->  prvGetFieldType(FieldName, FieldTypes, StructName, Line);
        error ->                                    throw({Line, ecompilerUtil:flatfmt("struct ~s is not found", [StructName])})
    end;
prvTypeOfStructField(T, _, _, Line) ->
    throw({Line, ecompilerUtil:flatfmt("op1 for \".\" is not struct ~s", [prvTypeToString(T)])}).

-spec prvGetFieldType(atom(), #{atom() => eType()}, atom(), integer()) -> eType().
prvGetFieldType(FieldName, FieldTypes, StructName, Line) ->
    case maps:find(FieldName, FieldTypes) of
        error ->        throw({Line, ecompilerUtil:flatfmt("~s.~s does not exist", [StructName, FieldName])});
        {ok, Type} ->   Type
    end.

-spec prvCompareTypes([eType()], [eType()]) -> boolean().
prvCompareTypes([T1 | Types1], [T2 | Types2]) ->
    case prvCompareType(T1, T2) of
        true ->     prvCompareTypes(Types1, Types2);
        _ ->        false
    end;
prvCompareTypes([], []) ->
    true;
prvCompareTypes(_, _) ->
    false.

-spec prvCompareType(eType(), eType()) -> boolean().
prvCompareType(#fun_type{params = P1, ret = R1}, #fun_type{params = P2, ret = R2}) ->
    prvCompareTypes(P1, P2) and prvCompareType(R1, R2);
prvCompareType(#array_type{elemtype = E1, len = L1}, #array_type{elemtype = E2, len = L2}) ->
    prvCompareType(E1, E2) and (L1 =:= L2);
prvCompareType(#basic_type{class = integer, pdepth = 0}, #basic_type{class = integer, pdepth = 0}) ->
    true;
prvCompareType(#basic_type{class = C, tag = T, pdepth = P}, #basic_type{class = C, tag = T, pdepth = P}) ->
    true;
prvCompareType(_, _) ->
    false.

-spec prvIsPointerAndIntOrdered(eType(), eType()) -> {true, eType()} | false.
prvIsPointerAndIntOrdered(#basic_type{pdepth = PointerDepth} = Type, #basic_type{class = integer, pdepth = 0}) when PointerDepth > 0 ->
    {true, Type};
prvIsPointerAndIntOrdered(_, _) ->
    false.

-spec prvIsPointerAndInt(eType(), eType()) -> {true, eType()} | false.
prvIsPointerAndInt(#basic_type{pdepth = PointerDepth} = Type, #basic_type{class = integer, pdepth = 0}) when PointerDepth > 0 ->
    {true, Type};
prvIsPointerAndInt(#basic_type{class = integer, pdepth = 0}, #basic_type{pdepth = PointerDepth} = Type) when PointerDepth > 0 ->
    {true, Type};
prvIsPointerAndInt(_, _) ->
    false.

-spec prvAreBothNumberSameType(eType(), eType()) -> {true, eType()} | false.
prvAreBothNumberSameType(T1, T2) ->
    case prvAreBothIntegers(T1, T2) or prvAreBothFloats(T1, T2) of
        true ->     {true, T1};
        false ->    false
    end.

-spec prvAreBothIntegers(eType(), eType()) -> boolean().
prvAreBothIntegers(#basic_type{pdepth = 0, class = integer}, #basic_type{pdepth = 0, class = integer}) ->   true;
prvAreBothIntegers(_, _) ->                                                                                 false.

-spec prvAreBothFloats(eType(), eType()) -> boolean().
prvAreBothFloats(#basic_type{pdepth = 0, class = float}, #basic_type{pdepth = 0, class = float}) ->     true;
prvAreBothFloats(_, _) ->                                                                               false.

-spec prvTypeErrorOfOp2(atom(), eType(), eType()) -> string().
prvTypeErrorOfOp2(Operator, TypeofOp1, TypeofOp2) ->
    ecompilerUtil:flatfmt("type error in \"~s ~s ~s\"", [prvTypeToString(TypeofOp1), Operator, prvTypeToString(TypeofOp2)]).

-spec prvCheckTypes([eType()], structTypeMap()) -> ok.
prvCheckTypes(TypeList, StructMap) -> lists:foreach(fun (T) -> prvCheckType(T, StructMap) end, TypeList).

%% check type, ensure that all struct used by type exists.
-spec prvCheckType(eType(), structTypeMap()) -> ok.
prvCheckType(#basic_type{class = struct, tag = Tag, line = Line}, StructMap) ->
    case maps:find(Tag, StructMap) of
        error ->        throw({Line, ecompilerUtil:flatfmt("struct ~s is not found", [Tag])});
        {ok, _} ->      ok
    end;
prvCheckType(#basic_type{}, _) ->
    ok;
prvCheckType(#array_type{elemtype = Elemtype}, StructMap) ->
    case Elemtype of
        #array_type{line = Line} ->     throw({Line, "nested array is not supported"});
        _ ->                            prvCheckType(Elemtype, StructMap)
    end;
prvCheckType(#fun_type{params = Params, ret = Rettype}, StructMap) ->
    prvCheckTypes(Params, StructMap),
    prvCheckType(Rettype, StructMap).

-spec prvJoinTypesToString([eType()]) -> string().
prvJoinTypesToString(Types) -> lists:join(",", lists:map(fun prvTypeToString/1, Types)).

-spec prvTypeToString(eType()) -> string().
prvTypeToString(#fun_type{params = Params, ret = Rettype}) ->
    io_lib:format("fun(~s): ~s", [prvJoinTypesToString(Params), prvTypeToString(Rettype)]);
prvTypeToString(#array_type{elemtype = Type, len = N}) ->
    io_lib:format("{~s, ~w}", [prvTypeToString(Type), N]);
prvTypeToString(#basic_type{tag = Tag, pdepth = Pdepth}) when Pdepth > 0 ->
    io_lib:format("(~s~s)", [Tag, lists:duplicate(Pdepth, "^")]);
prvTypeToString(#basic_type{tag = Tag, pdepth = 0}) ->
    atom_to_list(Tag).