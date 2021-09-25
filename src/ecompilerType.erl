-module(ecompilerType).

-export([checkTypesInAST/3, checkTypesInExpressions/3, typeOfExpression/2]).

-include("ecompilerFrameDef.hrl").

-spec checkTypesInAST(eAST(), variableTypeMap(), {functionTypeMap(), structTypeMap()}) -> ok.
checkTypesInAST([#function{var_types = VarTypes, exprs = Expressions, type = Fntype} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    checkTypes(maps:values(VarTypes), StructMap),
    checkType(Fntype#fun_type.ret, StructMap),
    CurrentVars = maps:merge(GlobalVarTypes, VarTypes),
    typeOfExpressions(Expressions, {CurrentVars, FunctionTypeMap, StructMap, Fntype#fun_type.ret}),
    checkTypesInAST(Rest, GlobalVarTypes, Maps);
checkTypesInAST([#struct{name = Name, field_types = FieldTypes, field_names = FieldNames, field_defaults = FieldDefaults} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
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
typeOfExpression(#op2{operator = assign, op1 = Operand1, op2 = Operand2, line = Line}, {_, _, StructMap, _} = Ctx) ->
    TypeofOp1 = case Operand1 of
                    #op2{operator = '.', op1 = SubOp1, op2 = SubOp2} ->
                        typeOfStructField(typeOfExpression(SubOp1, Ctx), SubOp2, StructMap, Line);
                    #op1{operator = '^', operand = SubOp} ->
                        decreasePointerDepth(typeOfExpression(SubOp, Ctx), Line);
                    #varref{} ->
                        typeOfExpression(Operand1, Ctx);
                    Any ->
                        throw({Line, ecompilerUtil:flatfmt("invalid left value (~s)", [ecompilerUtil:expressionToString(Any)])})
                end,
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case compareType(TypeofOp1, TypeofOp2) of
        true ->
            TypeofOp1;
        false ->
            throw({Line, ecompilerUtil:flatfmt("type mismatch in \"~s = ~s\"", [typeToString(TypeofOp1), typeToString(TypeofOp2)])})
    end;
typeOfExpression(#op2{operator = '.', op1 = Operand1, op2 = Operand2, line = Line}, {_, _, StructMap, _} = Ctx) ->
    typeOfStructField(typeOfExpression(Operand1, Ctx), Operand2, StructMap, Line);
typeOfExpression(#op2{operator = '::', op1 = #varref{name = self}, op2 = Operand2}, Ctx) ->
    typeOfExpression(Operand2, Ctx);
typeOfExpression(#op2{operator = '::', op1 = Operand1, op2 = Operand2, line = Line}, _) ->
    ecompilerUtil:assert(is_record(Operand1, varref), {Line, "invalid usage on ::"}),
    ecompilerUtil:assert(is_record(Operand2, varref), {Line, "invalid usage on ::"}),
    #varref{name = ModName} = Operand1,
    #varref{name = FunName} = Operand2,
    try ecompiler:queryFunctionInModule(ModName, FunName) of
        {ok, Type} ->
            Type;
        {error, functionNotFound} ->
            throw({Line, ecompilerUtil:flatfmt("~s:~s is not found", [ModName, FunName])})
    catch
        E ->
            throw({Line, E})
    end;
typeOfExpression(#op2{operator = '+', op1 = Operand1, op2 = Operand2, line = Line}, Ctx) ->
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
typeOfExpression(#op2{operator = '-', op1 = Operand1, op2 = Operand2, line = Line}, Ctx) ->
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
typeOfExpression(#op2{operator = Operator, op1 = Operand1, op2 = Operand2, line = Line}, Ctx) when Operator =:= '*'; Operator =:= '/' ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case areBothNumberSameType(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            throw({Line, typeErrorOfOp2(Operator, TypeofOp1, TypeofOp2)})
    end;
%% the left operators are: and, or, band, bor, bxor, bsl, bsr, >, <, ...
typeOfExpression(#op2{operator = Operator, op1 = Operand1, op2 = Operand2, line = Line}, Ctx) ->
    TypeofOp1 = typeOfExpression(Operand1, Ctx),
    TypeofOp2 = typeOfExpression(Operand2, Ctx),
    case areBothIntegers(TypeofOp1, TypeofOp2) of
        true ->
            TypeofOp1;
        false ->
            throw({Line, typeErrorOfOp2(Operator, TypeofOp1, TypeofOp2)})
    end;
typeOfExpression(#op1{operator = '^', operand = Operand, line = Line}, Ctx) ->
    case typeOfExpression(Operand, Ctx) of
        #basic_type{} = T ->
            decreasePointerDepth(T, Line);
        _ ->
            throw({Line, ecompilerUtil:flatfmt("invalid \"^\" on operand ~s", [ecompilerUtil:expressionToString(Operand)])})
    end;
typeOfExpression(#op1{operator = '@', operand = Operand, line = Line}, {_, _, StructMap, _} = Ctx) ->
    case Operand of
        #op2{operator = '.', op1 = Operand1, op2 = Operand2} ->
            T = typeOfStructField(typeOfExpression(Operand1, Ctx), Operand2, StructMap, Line),
            increasePointerDepth(T, Line);
        #varref{} ->
            increasePointerDepth(typeOfExpression(Operand, Ctx), Line);
        _ ->
            throw({Line, ecompilerUtil:flatfmt("invalid \"@\" on operand ~s", [ecompilerUtil:expressionToString(Operand)])})
    end;
typeOfExpression(#op1{operand = Operand}, Ctx) ->
    typeOfExpression(Operand, Ctx);
typeOfExpression(#call{fn = FunExpr, args = Arguments, line = Line}, Ctx) ->
    ArgsTypes = typeOfExpressions(Arguments, Ctx),
    case typeOfExpression(FunExpr, Ctx) of
        #fun_type{params = FnParamTypes, ret = FnRetType} ->
            case compareTypes(ArgsTypes, FnParamTypes) of
                true ->
                    FnRetType;
                false ->
                    throw({Line, argumentsErrorInformation(FnParamTypes, ArgsTypes)})
            end;
        T ->
            throw({Line, ecompilerUtil:flatfmt("invalid function expr: ~s", [typeToString(T)])})
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
    case compareType(RealRet, FnRetType) of
        true ->
            RealRet;
        false ->
            throw({Line, ecompilerUtil:flatfmt("ret type should be (~s), not (~s)", [typeToString(FnRetType), typeToString(RealRet)])})
    end;
typeOfExpression(#varref{name = Name, line = Line}, {VarTypes, FunctionTypeMap, StructMap, _}) ->
    Type = case maps:find(Name, VarTypes) of
               error ->
                   case maps:find(Name, FunctionTypeMap) of
                       {ok, T} ->
                           T;
                       error ->
                           throw({Line, ecompilerUtil:flatfmt("variable ~s is undefined", [Name])})
                   end;
               {ok, T} ->
                   T
           end,
    checkType(Type, StructMap),
    Type;
typeOfExpression(#array_init{elements = Elements, line = Line}, Ctx) ->
    ElementTypes = typeOfExpressions(Elements, Ctx),
    case areSameType(ElementTypes) of
        true ->
            #array_type{elemtype = hd(ElementTypes), len = length(ElementTypes), line = Line};
        false ->
            throw({Line, ecompilerUtil:flatfmt("array init type conflict: {~s}", [joinTypesToString(ElementTypes)])})
    end;
typeOfExpression(#struct_init{name = StructName, field_names = InitFieldNames, field_values = InitFieldValues, line = Line}, {_, _, StructMap, _} = Ctx) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{field_types = FieldTypes}} ->
            checkTypesInStructFields(InitFieldNames, FieldTypes, InitFieldValues, StructName, Ctx),
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

-spec argumentsErrorInformation([eType()], [eType()]) -> string().
argumentsErrorInformation(FnParamTypes, ArgsTypes) ->
    ecompilerUtil:flatfmt("args should be (~s), not (~s)", [joinTypesToString(FnParamTypes), joinTypesToString(ArgsTypes)]).

-spec increasePointerDepth(eType(), integer()) -> eType().
increasePointerDepth(#basic_type{pdepth = Pdepth} = T, _) ->
    T#basic_type{pdepth = Pdepth + 1};
increasePointerDepth(#array_type{elemtype = #basic_type{} = T}, OpLine) ->
    increasePointerDepth(T, OpLine);
increasePointerDepth(T, OpLine) ->
    throw({OpLine, ecompilerUtil:flatfmt("'@' on type ~s is invalid", [typeToString(T)])}).

-spec decreasePointerDepth(eType(), integer()) -> eType().
decreasePointerDepth(#basic_type{pdepth = Pdepth} = T, _) when Pdepth > 0 ->
    T#basic_type{pdepth = Pdepth - 1};
decreasePointerDepth(T, OpLine) ->
    throw({OpLine, ecompilerUtil:flatfmt("'^' on type ~s is invalid", [typeToString(T)])}).

-spec checkTypesInStructFields([#varref{}], variableTypeMap(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
checkTypesInStructFields(FieldNames, FieldTypes, ValMap, StructName, Ctx) ->
    lists:foreach(fun (V) -> checkStructField(V, FieldTypes, ValMap, StructName, Ctx) end, FieldNames).

-spec checkStructField(#varref{}, variableTypeMap(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
checkStructField(#varref{name = FieldName, line = Line}, FieldTypes, ValMap, StructName, {_, _, StructMap, _} = Ctx) ->
    {ok, Val} = maps:find(FieldName, ValMap),
    ExpectedType = getFieldType(FieldName, FieldTypes, StructName, Line),
    checkType(ExpectedType, StructMap),
    GivenType = typeOfExpression(Val, Ctx),
    case compareType(ExpectedType, GivenType) of
        true ->
            ok;
        false ->
            throw({Line, ecompilerUtil:flatfmt("~s.~s type error: ~s = ~s", [StructName, FieldName, typeToString(ExpectedType), typeToString(GivenType)])})
    end.

-spec areSameType([eType()]) -> boolean().
areSameType([TargetType, TargetType | Rest]) ->
    areSameType([TargetType | Rest]);
areSameType([_]) ->
    true;
areSameType(_) ->
    false.

-spec typeOfStructField(eType(), #varref{}, structTypeMap(), integer()) -> eType().
typeOfStructField(#basic_type{class = struct, tag = StructName, pdepth = 0}, #varref{name = FieldName}, StructMap, Line) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{field_types = FieldTypes}} ->
            getFieldType(FieldName, FieldTypes, StructName, Line);
        error ->
            throw({Line, ecompilerUtil:flatfmt("struct ~s is not found", [StructName])})
    end;
typeOfStructField(T, _, _, Line) ->
    throw({Line, ecompilerUtil:flatfmt("op1 for \".\" is not struct ~s", [typeToString(T)])}).

-spec getFieldType(atom(), #{atom() => eType()}, atom(), integer()) -> eType().
getFieldType(FieldName, FieldTypes, StructName, Line) ->
    case maps:find(FieldName, FieldTypes) of
        {ok, Type} ->
            Type;
        error ->
            throw({Line, ecompilerUtil:flatfmt("~s.~s does not exist", [StructName, FieldName])})
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
compareType(#fun_type{params = P1, ret = R1}, #fun_type{params = P2, ret = R2}) ->
    compareTypes(P1, P2) and compareType(R1, R2);
compareType(#array_type{elemtype = E1, len = L1}, #array_type{elemtype = E2, len = L2}) ->
    compareType(E1, E2) and (L1 =:= L2);
compareType(#basic_type{class = integer, pdepth = 0}, #basic_type{class = integer, pdepth = 0}) ->
    true;
compareType(#basic_type{class = C, tag = T, pdepth = P}, #basic_type{class = C, tag = T, pdepth = P}) ->
    true;
compareType(_, _) ->
    false.

-spec isPointerAndIntOrdered(eType(), eType()) -> {true, eType()} | false.
isPointerAndIntOrdered(#basic_type{pdepth = PointerDepth} = Type, #basic_type{class = integer, pdepth = 0}) when PointerDepth > 0 ->
    {true, Type};
isPointerAndIntOrdered(_, _) ->
    false.

-spec isPointerAndInt(eType(), eType()) -> {true, eType()} | false.
isPointerAndInt(#basic_type{pdepth = PointerDepth} = Type, #basic_type{class = integer, pdepth = 0}) when PointerDepth > 0 ->
    {true, Type};
isPointerAndInt(#basic_type{class = integer, pdepth = 0}, #basic_type{pdepth = PointerDepth} = Type) when PointerDepth > 0 ->
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
areBothIntegers(#basic_type{pdepth = 0, class = integer}, #basic_type{pdepth = 0, class = integer}) ->
    true;
areBothIntegers(_, _) ->
    false.

-spec areBothFloats(eType(), eType()) -> boolean().
areBothFloats(#basic_type{pdepth = 0, class = float}, #basic_type{pdepth = 0, class = float}) ->
    true;
areBothFloats(_, _) ->
    false.

-spec typeErrorOfOp2(atom(), eType(), eType()) -> string().
typeErrorOfOp2(Operator, TypeofOp1, TypeofOp2) ->
    ecompilerUtil:flatfmt("type error in \"~s ~s ~s\"", [typeToString(TypeofOp1), Operator, typeToString(TypeofOp2)]).

-spec checkTypes([eType()], structTypeMap()) -> ok.
checkTypes(TypeList, StructMap) ->
    lists:foreach(fun (T) -> checkType(T, StructMap) end, TypeList).

%% check type, ensure that all struct used by type exists.
-spec checkType(eType(), structTypeMap()) -> ok.
checkType(#basic_type{class = struct, tag = Tag, line = Line}, StructMap) ->
    case maps:find(Tag, StructMap) of
        {ok, _} ->
            ok;
        error ->
            throw({Line, ecompilerUtil:flatfmt("struct ~s is not found", [Tag])})
    end;
checkType(#basic_type{}, _) ->
    ok;
checkType(#array_type{elemtype = Elemtype}, StructMap) ->
    case Elemtype of
        #array_type{line = Line} ->
            throw({Line, "nested array is not supported"});
        _ ->
            checkType(Elemtype, StructMap)
    end;
checkType(#fun_type{params = Params, ret = Rettype}, StructMap) ->
    checkTypes(Params, StructMap),
    checkType(Rettype, StructMap).

-spec joinTypesToString([eType()]) -> string().
joinTypesToString(Types) ->
    lists:join(",", lists:map(fun typeToString/1, Types)).

-spec typeToString(eType()) -> string().
typeToString(#fun_type{params = Params, ret = Rettype}) ->
    io_lib:format("fun(~s): ~s", [joinTypesToString(Params), typeToString(Rettype)]);
typeToString(#array_type{elemtype = Type, len = N}) ->
    io_lib:format("{~s, ~w}", [typeToString(Type), N]);
typeToString(#basic_type{tag = Tag, pdepth = Pdepth}) when Pdepth > 0 ->
    io_lib:format("(~s~s)", [Tag, lists:duplicate(Pdepth, "^")]);
typeToString(#basic_type{tag = Tag, pdepth = 0}) ->
    atom_to_list(Tag).
