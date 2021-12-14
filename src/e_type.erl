-module(e_type).
-export([check_types_in_ast/3, check_type_in_ast_nodes/3, type_of_ast_node/2]).

-include("e_record_definition.hrl").

-spec check_types_in_ast(e_ast(), var_type_map(), {fn_type_map(), struct_type_map()}) -> ok.
check_types_in_ast([#function{variableTypeMap = VarTypes, statements = Expressions, type = FunctionType} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    check_types(maps:values(VarTypes), StructMap),
    check_type(FunctionType#function_type.ret, StructMap),
    CurrentVars = maps:merge(GlobalVarTypes, VarTypes),
    type_of_ast_nodes(Expressions, {CurrentVars, FunctionTypeMap, StructMap, FunctionType#function_type.ret}),
    check_types_in_ast(Rest, GlobalVarTypes, Maps);
check_types_in_ast([#struct{name = Name, fieldTypeMap = FieldTypes, fieldNames = FieldNames, fieldDefaultValueMap = FieldDefaults} | Rest], GlobalVarTypes, {FunctionTypeMap, StructMap} = Maps) ->
    check_types(maps:values(FieldTypes), StructMap),
    %% check the default values for fields
    InitFieldNames = e_util:filter_var_refs_in_map(FieldNames, FieldDefaults),
    check_types_in_struct_fields(InitFieldNames, FieldTypes, FieldDefaults, Name, {GlobalVarTypes, FunctionTypeMap, StructMap, #{}}),
    check_types_in_ast(Rest, GlobalVarTypes, Maps);
check_types_in_ast([_ | Rest], GlobalVarTypes, Maps) ->
    check_types_in_ast(Rest, GlobalVarTypes, Maps);
check_types_in_ast([], _, _) ->
    ok.

-type typeOfContext() :: {var_type_map(), fn_type_map(), struct_type_map(), functionReturnTypeMap()}.

-spec check_type_in_ast_nodes([e_expr()], var_type_map(), {fn_type_map(), struct_type_map()}) -> ok.
check_type_in_ast_nodes(Expressions, GlobalVarTypes, {FunctionTypeMap, StructMap}) ->
    type_of_ast_nodes(Expressions, {GlobalVarTypes, FunctionTypeMap, StructMap, #{}}),
    ok.

-spec type_of_ast_nodes([e_expr()], typeOfContext()) -> [e_type()].
type_of_ast_nodes(Expressions, Context) ->
    lists:map(fun (Expression) -> type_of_ast_node(Expression, Context) end, Expressions).

-spec type_of_ast_node(e_expr(), typeOfContext()) -> e_type().
type_of_ast_node(#operator_expression2{operator = assign, operand1 = Operand1, operand2 = Operand2, line = Line}, {_, _, StructMap, _} = Context) ->
    TypeofOp1 = case Operand1 of
                    #operator_expression2{operator = '.', operand1 = SubOp1, operand2 = SubOp2} ->
                        type_of_struct_field(type_of_ast_node(SubOp1, Context), SubOp2, StructMap, Line);
                    #operator_expression1{operator = '^', operand = SubOp} ->
                        dec_pointer_depth(type_of_ast_node(SubOp, Context), Line);
                    #variable_reference{} ->
                        type_of_ast_node(Operand1, Context);
                    Any ->
                        throw({Line, e_util:fmt("invalid left value (~s)", [e_util:expr_to_str(Any)])})
                end,
    TypeofOp2 = type_of_ast_node(Operand2, Context),
    case compare_type(TypeofOp1, TypeofOp2) of
        true ->
            TypeofOp1;
        false ->
            throw({Line, e_util:fmt("type mismatch in \"~s = ~s\"", [type_to_str(TypeofOp1), type_to_str(TypeofOp2)])})
    end;
type_of_ast_node(#operator_expression2{operator = '.', operand1 = Operand1, operand2 = Operand2, line = Line}, {_, _, StructMap, _} = Context) ->
    type_of_struct_field(type_of_ast_node(Operand1, Context), Operand2, StructMap, Line);
type_of_ast_node(#operator_expression2{operator = '+', operand1 = Operand1, operand2 = Operand2, line = Line}, Context) ->
    TypeofOp1 = type_of_ast_node(Operand1, Context),
    TypeofOp2 = type_of_ast_node(Operand2, Context),
    case are_both_number_of_same_type(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            case is_pointer_and_integer(TypeofOp1, TypeofOp2) of
                {true, PointerType} ->
                    PointerType;
                false ->
                    throw({Line, type_error_of_op2('+', TypeofOp1, TypeofOp2)})
            end
    end;
%% integer + pointer is valid, but integer - pointer is invalid
type_of_ast_node(#operator_expression2{operator = '-', operand1 = Operand1, operand2 = Operand2, line = Line}, Context) ->
    TypeofOp1 = type_of_ast_node(Operand1, Context),
    TypeofOp2 = type_of_ast_node(Operand2, Context),
    case are_both_number_of_same_type(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            case is_pointer_and_integer_ordered(TypeofOp1, TypeofOp2) of
                {true, PointerType} ->
                    PointerType;
                false ->
                    throw({Line, type_error_of_op2('-', TypeofOp1, TypeofOp2)})
            end
    end;
type_of_ast_node(#operator_expression2{operator = Operator, operand1 = Operand1, operand2 = Operand2, line = Line}, Context) when Operator =:= '*'; Operator =:= '/' ->
    TypeofOp1 = type_of_ast_node(Operand1, Context),
    TypeofOp2 = type_of_ast_node(Operand2, Context),
    case are_both_number_of_same_type(TypeofOp1, TypeofOp2) of
        {true, T} ->
            T;
        false ->
            throw({Line, type_error_of_op2(Operator, TypeofOp1, TypeofOp2)})
    end;
%% the left operators are: and, or, band, bor, bxor, bsl, bsr, >, <, ...
type_of_ast_node(#operator_expression2{operator = Operator, operand1 = Operand1, operand2 = Operand2, line = Line}, Context) ->
    TypeofOp1 = type_of_ast_node(Operand1, Context),
    TypeofOp2 = type_of_ast_node(Operand2, Context),
    case are_both_integers(TypeofOp1, TypeofOp2) of
        true ->
            TypeofOp1;
        false ->
            throw({Line, type_error_of_op2(Operator, TypeofOp1, TypeofOp2)})
    end;
type_of_ast_node(#operator_expression1{operator = '^', operand = Operand, line = Line}, Context) ->
    case type_of_ast_node(Operand, Context) of
        #basic_type{} = T ->
            dec_pointer_depth(T, Line);
        _ ->
            throw({Line, e_util:fmt("invalid \"^\" on operand ~s", [e_util:expr_to_str(Operand)])})
    end;
type_of_ast_node(#operator_expression1{operator = '@', operand = Operand, line = Line}, {_, _, StructMap, _} = Context) ->
    case Operand of
        #operator_expression2{operator = '.', operand1 = Operand1, operand2 = Operand2} ->
            T = type_of_struct_field(type_of_ast_node(Operand1, Context), Operand2, StructMap, Line),
            inc_pointer_depth(T, Line);
        #variable_reference{} ->
            inc_pointer_depth(type_of_ast_node(Operand, Context), Line);
        _ ->
            throw({Line, e_util:fmt("invalid \"@\" on operand ~s", [e_util:expr_to_str(Operand)])})
    end;
type_of_ast_node(#operator_expression1{operand = Operand}, Context) ->
    type_of_ast_node(Operand, Context);
type_of_ast_node(#call_expr{fn = FunExpr, args = Arguments, line = Line}, Context) ->
    ArgsTypes = type_of_ast_nodes(Arguments, Context),
    case type_of_ast_node(FunExpr, Context) of
        #function_type{parameters = FnParamTypes, ret = FnRetType} ->
            case compare_types(ArgsTypes, FnParamTypes) of
                true ->
                    FnRetType;
                false ->
                    throw({Line, argumentsErrorInformation(FnParamTypes, ArgsTypes)})
            end;
        T ->
            throw({Line, e_util:fmt("invalid function expr: ~s", [type_to_str(T)])})
    end;
type_of_ast_node(#if_statement{condition = Condition, then = Then, else = Else, line = Line}, Context) ->
    type_of_ast_node(Condition, Context),
    type_of_ast_nodes(Then, Context),
    type_of_ast_nodes(Else, Context),
    e_util:void_type(Line);
type_of_ast_node(#while_statement{condition = Condition, statements = Expressions, line = Line}, Context) ->
    type_of_ast_node(Condition, Context),
    type_of_ast_nodes(Expressions, Context),
    e_util:void_type(Line);
type_of_ast_node(#return_statement{expression = Expression, line = Line}, {_, _, _, FnRetType} = Context) ->
    RealRet = type_of_ast_node(Expression, Context),
    case compare_type(RealRet, FnRetType) of
        true ->
            RealRet;
        false ->
            throw({Line, e_util:fmt("ret type should be (~s), not (~s)", [type_to_str(FnRetType), type_to_str(RealRet)])})
    end;
type_of_ast_node(#variable_reference{name = Name, line = Line}, {VarTypes, FunctionTypeMap, StructMap, _}) ->
    Type = case maps:find(Name, VarTypes) of
               error ->
                   case maps:find(Name, FunctionTypeMap) of
                       {ok, T} ->
                           T;
                       error ->
                           throw({Line, e_util:fmt("variable ~s is undefined", [Name])})
                   end;
               {ok, T} ->
                   T
           end,
    check_type(Type, StructMap),
    Type;
type_of_ast_node(#array_init_expr{elements = Elements, line = Line}, Context) ->
    ElementTypes = type_of_ast_nodes(Elements, Context),
    case are_same_type(ElementTypes) of
        true ->
            #array_type{elemtype = hd(ElementTypes), length = length(ElementTypes), line = Line};
        false ->
            throw({Line, e_util:fmt("array init type conflict: {~s}", [join_types_to_str(ElementTypes)])})
    end;
type_of_ast_node(#struct_init_expr{name = StructName, fieldNames = InitFieldNames, fieldValueMap = InitFieldValues, line = Line}, {_, _, StructMap, _} = Context) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{fieldTypeMap = FieldTypes}} ->
            check_types_in_struct_fields(InitFieldNames, FieldTypes, InitFieldValues, StructName, Context),
            #basic_type{class = struct, tag = StructName, pdepth = 0, line = Line};
        _ ->
            throw({Line, e_util:fmt("struct ~s is not found", [StructName])})
    end;
type_of_ast_node(#sizeof_expression{line = Line}, _) ->
    #basic_type{class = integer, pdepth = 0, tag = i64, line = Line};
type_of_ast_node(#goto_statement{line = Line}, _) ->
    e_util:void_type(Line);
type_of_ast_node(#goto_label{line = Line}, _) ->
    e_util:void_type(Line);
type_of_ast_node(#type_convert{expression = Expression, type = TargetType, line = Line}, Context) ->
    case {type_of_ast_node(Expression, Context), TargetType} of
        {#basic_type{pdepth = D1}, #basic_type{pdepth = D2}} when D1 > 0, D2 > 0 ->
            TargetType;
        {#basic_type{class = integer, pdepth = 0}, #basic_type{pdepth = D2}} when D2 > 0 ->
            TargetType;
        {#basic_type{class = integer, pdepth = 0}, #basic_type{class = integer, pdepth = 0}} ->
            TargetType;
        {ExpressionType, _} ->
            throw({Line, e_util:fmt("incompatible type: ~w <-> ~w", [ExpressionType, TargetType])})
    end;
type_of_ast_node({float, Line, _}, _) ->
    #basic_type{class = float, pdepth = 0, tag = f64, line = Line};
type_of_ast_node({integer, Line, _}, _) ->
    #basic_type{class = integer, pdepth = 0, tag = i64, line = Line};
type_of_ast_node({string, Line, _}, _) ->
    #basic_type{class = integer, pdepth = 1, tag = i8, line = Line}.

-spec argumentsErrorInformation([e_type()], [e_type()]) -> string().
argumentsErrorInformation(FnParamTypes, ArgsTypes) ->
    e_util:fmt("args should be (~s), not (~s)", [join_types_to_str(FnParamTypes), join_types_to_str(ArgsTypes)]).

-spec inc_pointer_depth(e_type(), integer()) -> e_type().
inc_pointer_depth(#basic_type{pdepth = PointerDepth} = T, _) ->
    T#basic_type{pdepth = PointerDepth + 1};
inc_pointer_depth(#array_type{elemtype = #basic_type{} = T}, OpLine) ->
    inc_pointer_depth(T, OpLine);
inc_pointer_depth(T, OpLine) ->
    throw({OpLine, e_util:fmt("'@' on type ~s is invalid", [type_to_str(T)])}).

-spec dec_pointer_depth(e_type(), integer()) -> e_type().
dec_pointer_depth(#basic_type{pdepth = PointerDepth} = T, _) when PointerDepth > 0 ->
    T#basic_type{pdepth = PointerDepth - 1};
dec_pointer_depth(T, OpLine) ->
    throw({OpLine, e_util:fmt("'^' on type ~s is invalid", [type_to_str(T)])}).

-spec check_types_in_struct_fields([#variable_reference{}], var_type_map(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
check_types_in_struct_fields(FieldNames, FieldTypes, ValMap, StructName, Context) ->
    lists:foreach(fun (V) -> check_struct_field(V, FieldTypes, ValMap, StructName, Context) end, FieldNames).

-spec check_struct_field(#variable_reference{}, var_type_map(), #{atom() := any()}, atom(), typeOfContext()) -> ok.
check_struct_field(#variable_reference{name = FieldName, line = Line}, FieldTypes, ValMap, StructName, {_, _, StructMap, _} = Context) ->
    {ok, Val} = maps:find(FieldName, ValMap),
    ExpectedType = get_field_type(FieldName, FieldTypes, StructName, Line),
    check_type(ExpectedType, StructMap),
    GivenType = type_of_ast_node(Val, Context),
    case compare_type(ExpectedType, GivenType) of
        true ->
            ok;
        false ->
            throw({Line, e_util:fmt("~s.~s type error: ~s = ~s", [StructName, FieldName, type_to_str(ExpectedType), type_to_str(GivenType)])})
    end.

-spec are_same_type([e_type()]) -> boolean().
are_same_type([TargetType, TargetType | Rest]) ->
    are_same_type([TargetType | Rest]);
are_same_type([_]) ->
    true;
are_same_type(_) ->
    false.

-spec type_of_struct_field(e_type(), #variable_reference{}, struct_type_map(), integer()) -> e_type().
type_of_struct_field(#basic_type{class = struct, tag = StructName, pdepth = 0}, #variable_reference{name = FieldName}, StructMap, Line) ->
    case maps:find(StructName, StructMap) of
        {ok, #struct{fieldTypeMap = FieldTypes}} ->
            get_field_type(FieldName, FieldTypes, StructName, Line);
        error ->
            throw({Line, e_util:fmt("struct ~s is not found", [StructName])})
    end;
type_of_struct_field(T, _, _, Line) ->
    throw({Line, e_util:fmt("operand1 for \".\" is not struct ~s", [type_to_str(T)])}).

-spec get_field_type(atom(), #{atom() => e_type()}, atom(), integer()) -> e_type().
get_field_type(FieldName, FieldTypes, StructName, Line) ->
    case maps:find(FieldName, FieldTypes) of
        {ok, Type} ->
            Type;
        error ->
            throw({Line, e_util:fmt("~s.~s does not exist", [StructName, FieldName])})
    end.

-spec compare_types([e_type()], [e_type()]) -> boolean().
compare_types([T1 | Types1], [T2 | Types2]) ->
    case compare_type(T1, T2) of
        true ->
            compare_types(Types1, Types2);
        false ->
            false
    end;
compare_types([], []) ->
    true;
compare_types(_, _) ->
    false.

-spec compare_type(e_type(), e_type()) -> boolean().
compare_type(#function_type{parameters = P1, ret = R1}, #function_type{parameters = P2, ret = R2}) ->
    compare_types(P1, P2) and compare_type(R1, R2);
compare_type(#array_type{elemtype = E1, length = L1}, #array_type{elemtype = E2, length = L2}) ->
    compare_type(E1, E2) and (L1 =:= L2);
compare_type(#basic_type{class = integer, pdepth = 0}, #basic_type{class = integer, pdepth = 0}) ->
    true;
compare_type(#basic_type{class = C, tag = T, pdepth = P}, #basic_type{class = C, tag = T, pdepth = P}) ->
    true;
compare_type(_, _) ->
    false.

-spec is_pointer_and_integer_ordered(e_type(), e_type()) -> {true, e_type()} | false.
is_pointer_and_integer_ordered(#basic_type{pdepth = PointerDepth} = Type, #basic_type{class = integer, pdepth = 0}) when PointerDepth > 0 ->
    {true, Type};
is_pointer_and_integer_ordered(_, _) ->
    false.

-spec is_pointer_and_integer(e_type(), e_type()) -> {true, e_type()} | false.
is_pointer_and_integer(#basic_type{pdepth = PointerDepth} = Type, #basic_type{class = integer, pdepth = 0}) when PointerDepth > 0 ->
    {true, Type};
is_pointer_and_integer(#basic_type{class = integer, pdepth = 0}, #basic_type{pdepth = PointerDepth} = Type) when PointerDepth > 0 ->
    {true, Type};
is_pointer_and_integer(_, _) ->
    false.

-spec are_both_number_of_same_type(e_type(), e_type()) -> {true, e_type()} | false.
are_both_number_of_same_type(T1, T2) ->
    case are_both_integers(T1, T2) or are_both_floats(T1, T2) of
        true ->
            {true, T1};
        false ->
            false
    end.

-spec are_both_integers(e_type(), e_type()) -> boolean().
are_both_integers(#basic_type{pdepth = 0, class = integer}, #basic_type{pdepth = 0, class = integer}) ->
    true;
are_both_integers(_, _) ->
    false.

-spec are_both_floats(e_type(), e_type()) -> boolean().
are_both_floats(#basic_type{pdepth = 0, class = float}, #basic_type{pdepth = 0, class = float}) ->
    true;
are_both_floats(_, _) ->
    false.

-spec type_error_of_op2(atom(), e_type(), e_type()) -> string().
type_error_of_op2(Operator, TypeofOp1, TypeofOp2) ->
    e_util:fmt("type error in \"~s ~s ~s\"", [type_to_str(TypeofOp1), Operator, type_to_str(TypeofOp2)]).

-spec check_types([e_type()], struct_type_map()) -> ok.
check_types(TypeList, StructMap) ->
    lists:foreach(fun (T) -> check_type(T, StructMap) end, TypeList).

%% check type, ensure that all struct used by type exists.
-spec check_type(e_type(), struct_type_map()) -> ok.
check_type(#basic_type{class = struct, tag = Tag, line = Line}, StructMap) ->
    case maps:find(Tag, StructMap) of
        {ok, _} ->
            ok;
        error ->
            throw({Line, e_util:fmt("struct ~s is not found", [Tag])})
    end;
check_type(#basic_type{}, _) ->
    ok;
check_type(#array_type{elemtype = ElementType}, StructMap) ->
    case ElementType of
        #array_type{line = Line} ->
            throw({Line, "nested array is not supported"});
        _ ->
            check_type(ElementType, StructMap)
    end;
check_type(#function_type{parameters = Params, ret = ReturnType}, StructMap) ->
    check_types(Params, StructMap),
    check_type(ReturnType, StructMap).

-spec join_types_to_str([e_type()]) -> string().
join_types_to_str(Types) ->
    lists:join(",", lists:map(fun type_to_str/1, Types)).

-spec type_to_str(e_type()) -> string().
type_to_str(#function_type{parameters = Params, ret = ReturnType}) ->
    io_lib:format("fun(~s): ~s", [join_types_to_str(Params), type_to_str(ReturnType)]);
type_to_str(#array_type{elemtype = Type, length = N}) ->
    io_lib:format("{~s, ~w}", [type_to_str(Type), N]);
type_to_str(#basic_type{tag = Tag, pdepth = PointerDepth}) when PointerDepth > 0 ->
    io_lib:format("(~s~s)", [Tag, lists:duplicate(PointerDepth, "^")]);
type_to_str(#basic_type{tag = Tag, pdepth = 0}) ->
    atom_to_list(Tag).
