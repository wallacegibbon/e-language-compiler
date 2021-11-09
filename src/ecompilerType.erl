-module(ecompilerType).

-export([checkTypesInAST/3, checkTypesInExpressions/3, typeOfExpression/2]).

-include("ecompilerFrameDef.hrl").

-spec checkTypesInAST(eAST(), variableTypeMap(), {functionTypeMap(), structTypeMap()}) -> ok.
checkTypesInAST([#function{variableTypeMap = VarTypes, statements = Expressions, type = Fntype} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    checkTypes(maps:values(VarTypes), StructMap),
    checkType(Fntype#functionType.ret, StructMap),
    CurrentVars = maps:merge(GlobalVarTypes, VarTypes),
    typeOfExpressions(Expressions, {CurrentVars, FunctionTypeMap, StructMap, Fntype#functionType.ret}),
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([#struct{name = Name, fieldTypeMap = FieldTypes, fieldNames = FieldNames, fieldDefaultValueMap = FieldDefaults} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    checkTypes(maps:values(FieldTypes), StructMap),
    %% check the default values for fields
    InitFieldNames = ecompilerUtil:filterVariableReferenceInMap(FieldNames, FieldDefaults),
    checkTypesInStructFields(InitFieldNames, FieldTypes, FieldDefaults, Name, {GlobalVarTypes, FunctionTypeMap, StructMap, #{}}),
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([_ | Rest], GlobalVarTypes, Maps) ->
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([], _, _) ->
    ok.

-type typeOfContext() :: {variableTypeMap(), functionTypeMap(), structTypeMap(), functionReturnTypeMap()}.

-spec checkTypesInExpressions([eExpression()], variableTypeMap(), {functionTypeMap(), structTypeMap()}) -> ok.
checkTypesInExpressions(Expressions, GlobalVarTypes, {FunctionTypeMap, StructMap}) ->
    typeOfExpressions(Expressions, {GlobalVarTypes, FunctionTypeMap, StructMap, #{}}),
    ok.

-spec typeOfExpressions([eExpression()], typeOfContext()) -> [eType()].
typeOfExpressions(Expressions, Ctx) ->
    lists:map(fun (Expression) -> typeOfExpression(Expression, Ctx) end, Expressions).

-spec typeOfExpression(eExpression(), typeOfContext()) -> eType().
typeOfExpression(#operatorExpression2{operator = assign, operand1 = Operand1, operand2 = Operand2, line = Line}, {_, _, StructMap, _} = Ctx) ->
    TypeofOp1 = case Operand1 of
                    #operatorExpression2{operator = '.', operand1 = SubOp1, operand2 = SubOp2} ->
                        typeOfStructField(typeOfExpression(SubOp1, Ctx), SubOp2, StructMap, Line);
                    #operatorExpression1{operator = '^', operand = SubOp} ->
                        decreasePointerDepth(typeOfExpression(SubOp, Ctx), Line);
                    #variableReference{} ->
                        typeOfExpression(Operand1, Ctx);
                    Any ->
                        throw({Line, ecompilerUtil:fmt("invalid left value (~s)", [ecompilerUtil:expressionToString(Any)])})
                end,
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case compareType(TypeofOp1, TypeofOp2) of
        true ->
            TypeofOp1;
        false ->
            throw({Line, ecompilerUtil:fmt("type mismatch in \"~s = ~s\"", [typeToString(TypeofOp1), typeToString(TypeofOp2)])})
    end;
typeOfExpression(#operatorExpression2{operator = '.', operand1 = Operand1, operand2 = Operand2, line = Line}, {_, _, StructMap, _} = Ctx) ->
    typeOfStructField(typeOfExpression(Operand1, Ctx), Operand2, StructMap, Line);
typeOfExpression(#operatorExpression2{operator = '+', operand1 = Operand1, operand2 = Operand2, line = Line}, Ctx) ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case areBothNumberSameType(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            case isPointerAndInt(TypeofOp1, TypeofOp2) of
                {true, Ptype} ->
                    Ptype;
                false ->
                    throw({Line, typeErrorOfOp2('+', TypeofOp1, TypeofOp2)})
            end
    end;
%% integer + pointer is valid, but integer - pointer is invalid
typeOfExpression(#operatorExpression2{operator = '-', operand1 = Operand1, operand2 = Operand2, line = Line}, Ctx) ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case areBothNumberSameType(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            case isPointerAndIntOrdered(TypeofOp1, TypeofOp2) of
                {true, Ptype} ->
                    Ptype;
                false ->
                    throw({Line, typeErrorOfOp2('-', TypeofOp1, TypeofOp2)})
            end
    end;
typeOfExpression(#operatorExpression2{operator = Operator, operand1 = Operand1, operand2 = Operand2, line = Line}, Ctx) when Operator =:= '*'; Operator =:= '/' ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case areBothNumberSameType(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            throw({Line, typeErrorOfOp2(Operator, TypeofOp1, TypeofOp2)})
    end;
%% the left operators are: and, or, band, bor, bxor, bsl, bsr, >, <, ...
typeOfExpression(#operatorExpression2{operator = Operator, operand1 = Operand1, operand2 = Operand2, line = Line}, Ctx) ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case areBothIntegers(TypeofOp1, TypeofOp2) of
        true ->
            TypeofOp1;
        false ->
            throw({Line, typeErrorOfOp2(Operator, TypeofOp1, TypeofOp2)})
    end;
typeOfExpression(#operatorExpression1{operator = '^', operand = Operand, line = Line}, Ctx) ->
    case typeOfExpression(Operand, Ctx) of
        #basicType{} = T ->
            decreasePointerDepth(T, Line);
        _ ->
            throw({Line, ecompilerUtil:fmt("invalid \"^\" on operand ~s", [ecompilerUtil:expressionToString(Operand)])})
    end;
typeOfExpression(#operatorExpression1{operator = '@', operand = Operand, line = Line}, {_, _, StructMap, _} = Ctx) ->
    case Operand of
        #operatorExpression2{operator = '.', operand1 = Operand1, operand2 = Operand2} ->
            T = typeOfStructField(typeOfExpression(Operand1, Ctx), Operand2, StructMap, Line),
            increasePointerDepth(T, Line);
        #variableReference{} ->
            increasePointerDepth(typeOfExpression(Operand, Ctx), Line);
        _ ->
            throw({Line, ecompilerUtil:fmt("invalid \"@\" on operand ~s", [ecompilerUtil:expressionToString(Operand)])})
    end;
typeOfExpression(#operatorExpression1{operand = Operand}, Ctx) ->
    typeOfExpression(Operand, Ctx);
typeOfExpression(#callExpression{fn = FunExpr, args = Arguments, line = Line}, Ctx) ->
    ArgsTypes = typeOfExpressions(Arguments, Ctx),
    case typeOfExpression(FunExpr, Ctx) of
        #functionType{parameters = FnParamTypes, ret = FnRetType} ->
            case compareTypes(ArgsTypes, FnParamTypes) of
                true ->
                    FnRetType;
                false ->
                    throw({Line, argumentsErrorInformation(FnParamTypes, ArgsTypes)})
            end;
        T ->
            throw({Line, ecompilerUtil:fmt("invalid function expr: ~s", [typeToString(T)])})
    end;
typeOfExpression(#ifStatement{condition = Condition, then = Then, else = Else, line = Line}, Ctx) ->
    typeOfExpression(Condition, Ctx),
    typeOfExpressions(Then, Ctx),
    typeOfExpressions(Else, Ctx),
    ecompilerUtil:voidType(Line);
typeOfExpression(#whileStatement{condition = Condition, statements = Expressions, line = Line}, Ctx) ->
    typeOfExpression(Condition, Ctx),
    typeOfExpressions(Expressions, Ctx),
    ecompilerUtil:voidType(Line);
typeOfExpression(#returnStatement{expression = Expression, line = Line}, {_, _, _, FnRetType} = Ctx) ->
    RealRet = typeOfExpression(Expression, Ctx),
    case compareType(RealRet, FnRetType) of
        true ->
            RealRet;
        false ->
            throw({Line, ecompilerUtil:fmt("ret type should be (~s), not (~s)", [typeToString(FnRetType), typeToString(RealRet)])})
    end;
typeOfExpression(#variableReference{name = Name, line = Line}, {VarTypes, FunctionTypeMap, StructMap, _}) ->
    Type = case maps:find(Name, VarTypes) of
               error ->
                   case maps:find(Name, FunctionTypeMap) of
                       {ok, T} ->
                           T;
                       error ->
                           throw({Line, ecompilerUtil:fmt("variable ~s is undefined", [Name])})
                   end;
               {ok, T} ->
                   T
           end,
    checkType(Type, StructMap),
    Type;
typeOfExpression(#arrayInitializeExpression{elements = Elements, line = Line}, Ctx) ->
    ElementTypes = typeOfExpressions(Elements, Ctx),
    case areSameType(ElementTypes) of
        true ->
            #arrayType{elemtype = hd(ElementTypes), length = length(ElementTypes), line = Line};
        false ->
            throw({Line, ecompilerUtil:fmt("array init type conflict: {~s}", [joinTypesToString(ElementTypes)])})
    end;
typeOfExpression(#structInitializeExpression{name = StructName, fieldNames = InitFieldNames, fieldValueMap = InitFieldValues, line = Line}, {_, _, StructMap, _} = Ctx) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{fieldTypeMap = FieldTypes}} ->
            checkTypesInStructFields(InitFieldNames, FieldTypes, InitFieldValues, StructName, Ctx),
            #basicType{class = struct, tag = StructName, pdepth = 0, line = Line};
        _ ->
            throw({Line, ecompilerUtil:fmt("struct ~s is not found", [StructName])})
    end;
typeOfExpression(#sizeofExpression{line = Line}, _) ->
    #basicType{class = integer, pdepth = 0, tag = i64, line = Line};
typeOfExpression(#gotoStatement{line = Line}, _) ->
    ecompilerUtil:voidType(Line);
typeOfExpression(#gotoLabel{line = Line}, _) ->
    ecompilerUtil:voidType(Line);
typeOfExpression({float, Line, _}, _) ->
    #basicType{class = float, pdepth = 0, tag = f64, line = Line};
typeOfExpression({integer, Line, _}, _) ->
    #basicType{class = integer, pdepth = 0, tag = i64, line = Line};
typeOfExpression({string, Line, _}, _) ->
    #basicType{class = integer, pdepth = 1, tag = i8, line = Line}.

-spec argumentsErrorInformation([eType()], [eType()]) -> string().
argumentsErrorInformation(FnParamTypes, ArgsTypes) ->
    ecompilerUtil:fmt("args should be (~s), not (~s)", [joinTypesToString(FnParamTypes), joinTypesToString(ArgsTypes)]).

-spec increasePointerDepth(eType(), integer()) -> eType().
increasePointerDepth(#basicType{pdepth = Pdepth} = T, _) ->
    T#basicType{pdepth = Pdepth + 1};
increasePointerDepth(#arrayType{elemtype = #basicType{} = T}, OpLine) ->
    increasePointerDepth(T, OpLine);
increasePointerDepth(T, OpLine) ->
    throw({OpLine, ecompilerUtil:fmt("'@' on type ~s is invalid", [typeToString(T)])}).

-spec decreasePointerDepth(eType(), integer()) -> eType().
decreasePointerDepth(#basicType{pdepth = Pdepth} = T, _) when Pdepth > 0 ->
    T#basicType{pdepth = Pdepth - 1};
decreasePointerDepth(T, OpLine) ->
    throw({OpLine, ecompilerUtil:fmt("'^' on type ~s is invalid", [typeToString(T)])}).

-spec checkTypesInStructFields([#variableReference{}], variableTypeMap(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
checkTypesInStructFields(FieldNames, FieldTypes, ValMap, StructName, Ctx) ->
    lists:foreach(fun (V) -> checkStructField(V, FieldTypes, ValMap, StructName, Ctx) end, FieldNames).

-spec checkStructField(#variableReference{}, variableTypeMap(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
checkStructField(#variableReference{name = FieldName, line = Line}, FieldTypes, ValMap, StructName, {_, _, StructMap, _} = Ctx) ->
    {ok, Val} = maps:find(FieldName, ValMap),
    ExpectedType = getFieldType(FieldName, FieldTypes, StructName, Line),
    checkType(ExpectedType, StructMap),
    GivenType = typeOfExpression(Val, Ctx),
    case compareType(ExpectedType, GivenType) of
        true ->
            ok;
        false ->
            throw({Line, ecompilerUtil:fmt("~s.~s type error: ~s = ~s", [StructName, FieldName, typeToString(ExpectedType), typeToString(GivenType)])})
    end.

-spec areSameType([eType()]) -> boolean().
areSameType([TargetType, TargetType | Rest]) ->
    areSameType([TargetType | Rest]);
areSameType([_]) ->
    true;
areSameType(_) ->
    false.

-spec typeOfStructField(eType(), #variableReference{}, structTypeMap(), integer()) -> eType().
typeOfStructField(#basicType{class = struct, tag = StructName, pdepth = 0}, #variableReference{name = FieldName}, StructMap, Line) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{fieldTypeMap = FieldTypes}} ->
            getFieldType(FieldName, FieldTypes, StructName, Line);
        error ->
            throw({Line, ecompilerUtil:fmt("struct ~s is not found", [StructName])})
    end;
typeOfStructField(T, _, _, Line) ->
    throw({Line, ecompilerUtil:fmt("operand1 for \".\" is not struct ~s", [typeToString(T)])}).

-spec getFieldType(atom(), #{atom() => eType()}, atom(), integer()) -> eType().
getFieldType(FieldName, FieldTypes, StructName, Line) ->
    case maps:find(FieldName, FieldTypes) of
        {ok, Type} ->
            Type;
        error ->
            throw({Line, ecompilerUtil:fmt("~s.~s does not exist", [StructName, FieldName])})
    end.

-spec compareTypes([eType()], [eType()]) -> boolean().
compareTypes([T1 | Types1], [T2 | Types2]) ->
    case compareType(T1, T2) of
        true ->
            compareTypes(Types1, Types2);
        false ->
            false
    end;
compareTypes([], []) ->
    true;
compareTypes(_, _) ->
    false.

-spec compareType(eType(), eType()) -> boolean().
compareType(#functionType{parameters = P1, ret = R1}, #functionType{parameters = P2, ret = R2}) ->
    compareTypes(P1, P2) and compareType(R1, R2);
compareType(#arrayType{elemtype = E1, length = L1}, #arrayType{elemtype = E2, length = L2}) ->
    compareType(E1, E2) and (L1 =:= L2);
compareType(#basicType{class = integer, pdepth = 0}, #basicType{class = integer, pdepth = 0}) ->
    true;
compareType(#basicType{class = C, tag = T, pdepth = P}, #basicType{class = C, tag = T, pdepth = P}) ->
    true;
compareType(_, _) ->
    false.

-spec isPointerAndIntOrdered(eType(), eType()) -> {true, eType()} | false.
isPointerAndIntOrdered(#basicType{pdepth = PointerDepth} = Type, #basicType{class = integer, pdepth = 0}) when PointerDepth > 0 ->
    {true, Type};
isPointerAndIntOrdered(_, _) ->
    false.

-spec isPointerAndInt(eType(), eType()) -> {true, eType()} | false.
isPointerAndInt(#basicType{pdepth = PointerDepth} = Type, #basicType{class = integer, pdepth = 0}) when PointerDepth > 0 ->
    {true, Type};
isPointerAndInt(#basicType{class = integer, pdepth = 0}, #basicType{pdepth = PointerDepth} = Type) when PointerDepth > 0 ->
    {true, Type};
isPointerAndInt(_, _) ->
    false.

-spec areBothNumberSameType(eType(), eType()) -> {true, eType()} | false.
areBothNumberSameType(T1, T2) ->
    case areBothIntegers(T1, T2) or areBothFloats(T1, T2) of
        true ->
            {true, T1};
        false ->
            false
    end.

-spec areBothIntegers(eType(), eType()) -> boolean().
areBothIntegers(#basicType{pdepth = 0, class = integer}, #basicType{pdepth = 0, class = integer}) ->
    true;
areBothIntegers(_, _) ->
    false.

-spec areBothFloats(eType(), eType()) -> boolean().
areBothFloats(#basicType{pdepth = 0, class = float}, #basicType{pdepth = 0, class = float}) ->
    true;
areBothFloats(_, _) ->
    false.

-spec typeErrorOfOp2(atom(), eType(), eType()) -> string().
typeErrorOfOp2(Operator, TypeofOp1, TypeofOp2) ->
    ecompilerUtil:fmt("type error in \"~s ~s ~s\"", [typeToString(TypeofOp1), Operator, typeToString(TypeofOp2)]).

-spec checkTypes([eType()], structTypeMap()) -> ok.
checkTypes(TypeList, StructMap) ->
    lists:foreach(fun (T) -> checkType(T, StructMap) end, TypeList).

%% check type, ensure that all struct used by type exists.
-spec checkType(eType(), structTypeMap()) -> ok.
checkType(#basicType{class = struct, tag = Tag, line = Line}, StructMap) ->
    case maps:find(Tag, StructMap) of
        {ok, _} ->
            ok;
        error ->
            throw({Line, ecompilerUtil:fmt("struct ~s is not found", [Tag])})
    end;
checkType(#basicType{}, _) ->
    ok;
checkType(#arrayType{elemtype = Elemtype}, StructMap) ->
    case Elemtype of
        #arrayType{line = Line} ->
            throw({Line, "nested array is not supported"});
        _ ->
            checkType(Elemtype, StructMap)
    end;
checkType(#functionType{parameters = Params, ret = Rettype}, StructMap) ->
    checkTypes(Params, StructMap),
    checkType(Rettype, StructMap).

-spec joinTypesToString([eType()]) -> string().
joinTypesToString(Types) ->
    lists:join(",", lists:map(fun typeToString/1, Types)).

-spec typeToString(eType()) -> string().
typeToString(#functionType{parameters = Params, ret = Rettype}) ->
    io_lib:format("fun(~s): ~s", [joinTypesToString(Params), typeToString(Rettype)]);
typeToString(#arrayType{elemtype = Type, length = N}) ->
    io_lib:format("{~s, ~w}", [typeToString(Type), N]);
typeToString(#basicType{tag = Tag, pdepth = Pdepth}) when Pdepth > 0 ->
    io_lib:format("(~s~s)", [Tag, lists:duplicate(Pdepth, "^")]);
typeToString(#basicType{tag = Tag, pdepth = 0}) ->
    atom_to_list(Tag).
