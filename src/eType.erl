-module(eType).

-export([checkTypesInAST/3, checkTypesInASTNodeList/3, typeOfASTNode/2]).

-include("eRecordDefinition.hrl").

-spec checkTypesInAST(eAST(), variableTypeMap(), {functionTypeMap(), structTypeMap()}) -> ok.
checkTypesInAST([#function{variableTypeMap = VarTypes, statements = Expressions, type = FunctionType} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    checkTypes(maps:values(VarTypes), StructMap),
    checkType(FunctionType#functionType.ret, StructMap),
    CurrentVars = maps:merge(GlobalVarTypes, VarTypes),
    typeOfASTNodeList(Expressions, {CurrentVars, FunctionTypeMap, StructMap, FunctionType#functionType.ret}),
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([#struct{name = Name, fieldTypeMap = FieldTypes, fieldNames = FieldNames, fieldDefaultValueMap = FieldDefaults} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    checkTypes(maps:values(FieldTypes), StructMap),
    %% check the default values for fields
    InitFieldNames = eUtil:filterVariableReferenceInMap(FieldNames, FieldDefaults),
    checkTypesInStructFields(InitFieldNames, FieldTypes, FieldDefaults, Name, {GlobalVarTypes, FunctionTypeMap, StructMap, #{}}),
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([_ | Rest], GlobalVarTypes, Maps) ->
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([], _, _) ->
    ok.

-type typeOfContext() :: {variableTypeMap(), functionTypeMap(), structTypeMap(), functionReturnTypeMap()}.

-spec checkTypesInASTNodeList([eExpression()], variableTypeMap(), {functionTypeMap(), structTypeMap()}) -> ok.
checkTypesInASTNodeList(Expressions, GlobalVarTypes, {FunctionTypeMap, StructMap}) ->
    typeOfASTNodeList(Expressions, {GlobalVarTypes, FunctionTypeMap, StructMap, #{}}),
    ok.

-spec typeOfASTNodeList([eExpression()], typeOfContext()) -> [eType()].
typeOfASTNodeList(Expressions, Context) ->
    lists:map(fun (Expression) -> typeOfASTNode(Expression, Context) end, Expressions).

-spec typeOfASTNode(eExpression(), typeOfContext()) -> eType().
typeOfASTNode(#operatorExpression2{operator = assign, operand1 = Operand1, operand2 = Operand2, line = Line}, {_, _, StructMap, _} = Context) ->
    TypeofOp1 = case Operand1 of
                    #operatorExpression2{operator = '.', operand1 = SubOp1, operand2 = SubOp2} ->
                        typeOfStructField(typeOfASTNode(SubOp1, Context), SubOp2, StructMap, Line);
                    #operatorExpression1{operator = '^', operand = SubOp} ->
                        decreasePointerDepth(typeOfASTNode(SubOp, Context), Line);
                    #variableReference{} ->
                        typeOfASTNode(Operand1, Context);
                    Any ->
                        throw({Line, eUtil:fmt("invalid left value (~s)", [eUtil:expressionToString(Any)])})
                end,
    TypeofOp2 = typeOfASTNode(Operand2, Context),
    case compareType(TypeofOp1, TypeofOp2) of
        true ->
            TypeofOp1;
        false ->
            throw({Line, eUtil:fmt("type mismatch in \"~s = ~s\"", [typeToString(TypeofOp1), typeToString(TypeofOp2)])})
    end;
typeOfASTNode(#operatorExpression2{operator = '.', operand1 = Operand1, operand2 = Operand2, line = Line}, {_, _, StructMap, _} = Context) ->
    typeOfStructField(typeOfASTNode(Operand1, Context), Operand2, StructMap, Line);
typeOfASTNode(#operatorExpression2{operator = '+', operand1 = Operand1, operand2 = Operand2, line = Line}, Context) ->
    TypeofOp1 = typeOfASTNode(Operand1, Context),
    TypeofOp2 = typeOfASTNode(Operand2, Context),
    case areBothNumberSameType(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            case isPointerAndInt(TypeofOp1, TypeofOp2) of
                {true, PointerType} ->
                    PointerType;
                false ->
                    throw({Line, typeErrorOfOp2('+', TypeofOp1, TypeofOp2)})
            end
    end;
%% integer + pointer is valid, but integer - pointer is invalid
typeOfASTNode(#operatorExpression2{operator = '-', operand1 = Operand1, operand2 = Operand2, line = Line}, Context) ->
    TypeofOp1 = typeOfASTNode(Operand1, Context),
    TypeofOp2 = typeOfASTNode(Operand2, Context),
    case areBothNumberSameType(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            case isPointerAndIntOrdered(TypeofOp1, TypeofOp2) of
                {true, PointerType} ->
                    PointerType;
                false ->
                    throw({Line, typeErrorOfOp2('-', TypeofOp1, TypeofOp2)})
            end
    end;
typeOfASTNode(#operatorExpression2{operator = Operator, operand1 = Operand1, operand2 = Operand2, line = Line}, Context) when Operator =:= '*'; Operator =:= '/' ->
    TypeofOp1 = typeOfASTNode(Operand1, Context),
    TypeofOp2 = typeOfASTNode(Operand2, Context),
    case areBothNumberSameType(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            throw({Line, typeErrorOfOp2(Operator, TypeofOp1, TypeofOp2)})
    end;
%% the left operators are: and, or, band, bor, bxor, bsl, bsr, >, <, ...
typeOfASTNode(#operatorExpression2{operator = Operator, operand1 = Operand1, operand2 = Operand2, line = Line}, Context) ->
    TypeofOp1 = typeOfASTNode(Operand1, Context),
    TypeofOp2 = typeOfASTNode(Operand2, Context),
    case areBothIntegers(TypeofOp1, TypeofOp2) of
        true ->
            TypeofOp1;
        false ->
            throw({Line, typeErrorOfOp2(Operator, TypeofOp1, TypeofOp2)})
    end;
typeOfASTNode(#operatorExpression1{operator = '^', operand = Operand, line = Line}, Context) ->
    case typeOfASTNode(Operand, Context) of
        #basicType{} = T ->
            decreasePointerDepth(T, Line);
        _ ->
            throw({Line, eUtil:fmt("invalid \"^\" on operand ~s", [eUtil:expressionToString(Operand)])})
    end;
typeOfASTNode(#operatorExpression1{operator = '@', operand = Operand, line = Line}, {_, _, StructMap, _} = Context) ->
    case Operand of
        #operatorExpression2{operator = '.', operand1 = Operand1, operand2 = Operand2} ->
            T = typeOfStructField(typeOfASTNode(Operand1, Context), Operand2, StructMap, Line),
            increasePointerDepth(T, Line);
        #variableReference{} ->
            increasePointerDepth(typeOfASTNode(Operand, Context), Line);
        _ ->
            throw({Line, eUtil:fmt("invalid \"@\" on operand ~s", [eUtil:expressionToString(Operand)])})
    end;
typeOfASTNode(#operatorExpression1{operand = Operand}, Context) ->
    typeOfASTNode(Operand, Context);
typeOfASTNode(#callExpression{fn = FunExpr, args = Arguments, line = Line}, Context) ->
    ArgsTypes = typeOfASTNodeList(Arguments, Context),
    case typeOfASTNode(FunExpr, Context) of
        #functionType{parameters = FnParamTypes, ret = FnRetType} ->
            case compareTypes(ArgsTypes, FnParamTypes) of
                true ->
                    FnRetType;
                false ->
                    throw({Line, argumentsErrorInformation(FnParamTypes, ArgsTypes)})
            end;
        T ->
            throw({Line, eUtil:fmt("invalid function expr: ~s", [typeToString(T)])})
    end;
typeOfASTNode(#ifStatement{condition = Condition, then = Then, else = Else, line = Line}, Context) ->
    typeOfASTNode(Condition, Context),
    typeOfASTNodeList(Then, Context),
    typeOfASTNodeList(Else, Context),
    eUtil:voidType(Line);
typeOfASTNode(#whileStatement{condition = Condition, statements = Expressions, line = Line}, Context) ->
    typeOfASTNode(Condition, Context),
    typeOfASTNodeList(Expressions, Context),
    eUtil:voidType(Line);
typeOfASTNode(#returnStatement{expression = Expression, line = Line}, {_, _, _, FnRetType} = Context) ->
    RealRet = typeOfASTNode(Expression, Context),
    case compareType(RealRet, FnRetType) of
        true ->
            RealRet;
        false ->
            throw({Line, eUtil:fmt("ret type should be (~s), not (~s)", [typeToString(FnRetType), typeToString(RealRet)])})
    end;
typeOfASTNode(#variableReference{name = Name, line = Line}, {VarTypes, FunctionTypeMap, StructMap, _}) ->
    Type = case maps:find(Name, VarTypes) of
               error ->
                   case maps:find(Name, FunctionTypeMap) of
                       {ok, T} ->
                           T;
                       error ->
                           throw({Line, eUtil:fmt("variable ~s is undefined", [Name])})
                   end;
               {ok, T} ->
                   T
           end,
    checkType(Type, StructMap),
    Type;
typeOfASTNode(#arrayInitializeExpression{elements = Elements, line = Line}, Context) ->
    ElementTypes = typeOfASTNodeList(Elements, Context),
    case areSameType(ElementTypes) of
        true ->
            #arrayType{elemtype = hd(ElementTypes), length = length(ElementTypes), line = Line};
        false ->
            throw({Line, eUtil:fmt("array init type conflict: {~s}", [joinTypesToString(ElementTypes)])})
    end;
typeOfASTNode(#structInitializeExpression{name = StructName, fieldNames = InitFieldNames, fieldValueMap = InitFieldValues, line = Line}, {_, _, StructMap, _} = Context) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{fieldTypeMap = FieldTypes}} ->
            checkTypesInStructFields(InitFieldNames, FieldTypes, InitFieldValues, StructName, Context),
            #basicType{class = struct, tag = StructName, pdepth = 0, line = Line};
        _ ->
            throw({Line, eUtil:fmt("struct ~s is not found", [StructName])})
    end;
typeOfASTNode(#sizeofExpression{line = Line}, _) ->
    #basicType{class = integer, pdepth = 0, tag = i64, line = Line};
typeOfASTNode(#gotoStatement{line = Line}, _) ->
    eUtil:voidType(Line);
typeOfASTNode(#gotoLabel{line = Line}, _) ->
    eUtil:voidType(Line);
typeOfASTNode(#typeConvert{expression = Expression, type = TargetType, line = Line}, Context) ->
    case {typeOfASTNode(Expression, Context), TargetType} of
        {#basicType{pdepth = D1}, #basicType{pdepth = D2}} when D1 > 0, D2 > 0 ->
            TargetType;
        {#basicType{class = integer, pdepth = 0}, #basicType{pdepth = D2}} when D2 > 0 ->
            TargetType;
        {#basicType{class = integer, pdepth = 0}, #basicType{class = integer, pdepth = 0}} ->
            TargetType;
        {ExpressionType, _} ->
            throw({Line, eUtil:fmt("incompatible type: ~w <-> ~w", [ExpressionType, TargetType])})
    end;
typeOfASTNode({float, Line, _}, _) ->
    #basicType{class = float, pdepth = 0, tag = f64, line = Line};
typeOfASTNode({integer, Line, _}, _) ->
    #basicType{class = integer, pdepth = 0, tag = i64, line = Line};
typeOfASTNode({string, Line, _}, _) ->
    #basicType{class = integer, pdepth = 1, tag = i8, line = Line}.

-spec argumentsErrorInformation([eType()], [eType()]) -> string().
argumentsErrorInformation(FnParamTypes, ArgsTypes) ->
    eUtil:fmt("args should be (~s), not (~s)", [joinTypesToString(FnParamTypes), joinTypesToString(ArgsTypes)]).

-spec increasePointerDepth(eType(), integer()) -> eType().
increasePointerDepth(#basicType{pdepth = PointerDepth} = T, _) ->
    T#basicType{pdepth = PointerDepth + 1};
increasePointerDepth(#arrayType{elemtype = #basicType{} = T}, OpLine) ->
    increasePointerDepth(T, OpLine);
increasePointerDepth(T, OpLine) ->
    throw({OpLine, eUtil:fmt("'@' on type ~s is invalid", [typeToString(T)])}).

-spec decreasePointerDepth(eType(), integer()) -> eType().
decreasePointerDepth(#basicType{pdepth = PointerDepth} = T, _) when PointerDepth > 0 ->
    T#basicType{pdepth = PointerDepth - 1};
decreasePointerDepth(T, OpLine) ->
    throw({OpLine, eUtil:fmt("'^' on type ~s is invalid", [typeToString(T)])}).

-spec checkTypesInStructFields([#variableReference{}], variableTypeMap(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
checkTypesInStructFields(FieldNames, FieldTypes, ValMap, StructName, Context) ->
    lists:foreach(fun (V) -> checkStructField(V, FieldTypes, ValMap, StructName, Context) end, FieldNames).

-spec checkStructField(#variableReference{}, variableTypeMap(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
checkStructField(#variableReference{name = FieldName, line = Line}, FieldTypes, ValMap, StructName, {_, _, StructMap, _} = Context) ->
    {ok, Val} = maps:find(FieldName, ValMap),
    ExpectedType = getFieldType(FieldName, FieldTypes, StructName, Line),
    checkType(ExpectedType, StructMap),
    GivenType = typeOfASTNode(Val, Context),
    case compareType(ExpectedType, GivenType) of
        true ->
            ok;
        false ->
            throw({Line, eUtil:fmt("~s.~s type error: ~s = ~s", [StructName, FieldName, typeToString(ExpectedType), typeToString(GivenType)])})
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
            throw({Line, eUtil:fmt("struct ~s is not found", [StructName])})
    end;
typeOfStructField(T, _, _, Line) ->
    throw({Line, eUtil:fmt("operand1 for \".\" is not struct ~s", [typeToString(T)])}).

-spec getFieldType(atom(), #{atom() => eType()}, atom(), integer()) -> eType().
getFieldType(FieldName, FieldTypes, StructName, Line) ->
    case maps:find(FieldName, FieldTypes) of
        {ok, Type} ->
            Type;
        error ->
            throw({Line, eUtil:fmt("~s.~s does not exist", [StructName, FieldName])})
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
    eUtil:fmt("type error in \"~s ~s ~s\"", [typeToString(TypeofOp1), Operator, typeToString(TypeofOp2)]).

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
            throw({Line, eUtil:fmt("struct ~s is not found", [Tag])})
    end;
checkType(#basicType{}, _) ->
    ok;
checkType(#arrayType{elemtype = ElementType}, StructMap) ->
    case ElementType of
        #arrayType{line = Line} ->
            throw({Line, "nested array is not supported"});
        _ ->
            checkType(ElementType, StructMap)
    end;
checkType(#functionType{parameters = Params, ret = ReturnType}, StructMap) ->
    checkTypes(Params, StructMap),
    checkType(ReturnType, StructMap).

-spec joinTypesToString([eType()]) -> string().
joinTypesToString(Types) ->
    lists:join(",", lists:map(fun typeToString/1, Types)).

-spec typeToString(eType()) -> string().
typeToString(#functionType{parameters = Params, ret = ReturnType}) ->
    io_lib:format("fun(~s): ~s", [joinTypesToString(Params), typeToString(ReturnType)]);
typeToString(#arrayType{elemtype = Type, length = N}) ->
    io_lib:format("{~s, ~w}", [typeToString(Type), N]);
typeToString(#basicType{tag = Tag, pdepth = PointerDepth}) when PointerDepth > 0 ->
    io_lib:format("(~s~s)", [Tag, lists:duplicate(PointerDepth, "^")]);
typeToString(#basicType{tag = Tag, pdepth = 0}) ->
    atom_to_list(Tag).
