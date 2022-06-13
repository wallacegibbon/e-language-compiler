-module(e_type).
-export([check_types_in_ast/3, check_type_in_ast_nodes/3, type_of_ast_node/2]).

-include("e_record_definition.hrl").

-spec check_types_in_ast(e_ast(), var_type_map(), {fn_type_map(), struct_type_map()}) -> ok.
check_types_in_ast([#function{var_type_map = VarTypes, stmts = Exprs, type = FnType} | Rest],
                   GlobalVarTypes,
                   {FnTypeMap, StructMap} = Maps) ->
    check_types(maps:values(VarTypes), StructMap),
    check_type(FnType#fn_type.ret, StructMap),
    CurrentVars = maps:merge(GlobalVarTypes, VarTypes),
    type_of_ast_nodes(Exprs, {CurrentVars, FnTypeMap, StructMap, FnType#fn_type.ret}),
    check_types_in_ast(Rest, GlobalVarTypes, Maps);
check_types_in_ast([#struct{name = Name, field_type_map = FieldTypes, field_names = FieldNames, field_default_value_map = FieldDefaults} | Rest],
                   GlobalVarTypes,
                   {FnTypeMap, StructMap} = Maps) ->
    check_types(maps:values(FieldTypes), StructMap),
    %% check the default values for fields
    InitFieldNames = e_util:filter_var_refs_in_map(FieldNames, FieldDefaults),
    check_types_in_struct_fields(InitFieldNames,  FieldTypes, FieldDefaults, Name, {GlobalVarTypes, FnTypeMap, StructMap, #{}}),
    check_types_in_ast(Rest, GlobalVarTypes, Maps);
check_types_in_ast([_ | Rest], GlobalVarTypes, Maps) ->
    check_types_in_ast(Rest, GlobalVarTypes, Maps);
check_types_in_ast([], _, _) ->
    ok.

-type context() :: {var_type_map(), fn_type_map(), struct_type_map(), fn_ret_type_map()}.

-spec check_type_in_ast_nodes([e_expr()], var_type_map(), {fn_type_map(), struct_type_map()}) -> ok.
check_type_in_ast_nodes(Exprs, GlobalVarTypes, {FnTypeMap, StructMap}) ->
    type_of_ast_nodes(Exprs, {GlobalVarTypes, FnTypeMap, StructMap, #{}}),
    ok.

-spec type_of_ast_nodes([e_expr()], context()) -> [e_type()].
type_of_ast_nodes(Exprs, Ctx) ->
    lists:map(fun(Expr) -> type_of_ast_node(Expr, Ctx) end, Exprs).

-spec type_of_ast_node(e_expr(), context()) -> e_type().
type_of_ast_node(#op2_expr{operator = assign, operand1 = Op1, operand2 = Op2, line = Line},
                 {_, _, StructMap, _} = Ctx) ->
    Op1Type =
        case Op1 of
            #op2_expr{operator = '.', operand1 = SubOp1, operand2 = SubOp2} ->
                type_of_struct_field(type_of_ast_node(SubOp1, Ctx), SubOp2, StructMap, Line);
            #op1_expr{operator = '^', operand = SubOp} ->
                dec_pointer_depth(type_of_ast_node(SubOp, Ctx), Line);
            #var_ref{} ->
                type_of_ast_node(Op1, Ctx);
            Any ->
                throw({Line, e_util:fmt("invalid left value (~s)", [e_util:expr_to_str(Any)])})
        end,
    Op2Type = type_of_ast_node(Op2, Ctx),
    case compare_type(Op1Type, Op2Type) of
        true ->
            Op1Type;
        false ->
            throw({Line, e_util:fmt("type mismatch in \"~s = ~s\"", [type_to_str(Op1Type), type_to_str(Op2Type)])})
    end;
type_of_ast_node(#op2_expr{operator = '.', operand1 = Op1, operand2 = Op2, line = Line}, {_, _, StructMap, _} = Ctx) ->
    type_of_struct_field(type_of_ast_node(Op1, Ctx), Op2, StructMap, Line);
type_of_ast_node(#op2_expr{operator = '+', operand1 = Op1, operand2 = Op2, line = Line}, Ctx) ->
    Op1Type = type_of_ast_node(Op1, Ctx),
    Op2Type = type_of_ast_node(Op2, Ctx),
    case are_both_number_of_same_type(Op1Type, Op2Type) of
        {true, T} ->
            T;
        false ->
            case is_pointer_and_integer(Op1Type, Op2Type) of
                {true, PointerType} ->
                    PointerType;
                false ->
                    throw({Line, type_error_of_op2('+', Op1Type, Op2Type)})
            end
    end;
%% integer + pointer is valid, but integer - pointer is invalid
type_of_ast_node(#op2_expr{operator = '-', operand1 = Op1, operand2 = Op2, line = Line}, Ctx) ->
    Op1Type = type_of_ast_node(Op1, Ctx),
    Op2Type = type_of_ast_node(Op2, Ctx),
    case are_both_number_of_same_type(Op1Type, Op2Type) of
        {true, T} ->
            T;
        false ->
            case is_pointer_and_integer_ordered(Op1Type, Op2Type) of
                {true, PointerType} ->
                    PointerType;
                false ->
                    throw({Line, type_error_of_op2('-', Op1Type, Op2Type)})
            end
    end;
type_of_ast_node(#op2_expr{operator = Operator, operand1 = Op1, operand2 = Op2, line = Line}, Ctx)
  when Operator =:= '*'; Operator =:= '/' ->
    Op1Type = type_of_ast_node(Op1, Ctx),
    Op2Type = type_of_ast_node(Op2, Ctx),
    case are_both_number_of_same_type(Op1Type, Op2Type) of
        {true, T} ->
            T;
        false ->
            throw({Line, type_error_of_op2(Operator, Op1Type, Op2Type)})
    end;
%% the left operators are: and, or, band, bor, bxor, bsl, bsr, >, <, ...
type_of_ast_node(#op2_expr{operator = Operator, operand1 = Op1, operand2 = Op2, line = Line}, Ctx) ->
    Op1Type = type_of_ast_node(Op1, Ctx),
    Op2Type = type_of_ast_node(Op2, Ctx),
    case are_both_integers(Op1Type, Op2Type) of
        true ->
            Op1Type;
        false ->
            throw({Line, type_error_of_op2(Operator, Op1Type, Op2Type)})
    end;
type_of_ast_node(#op1_expr{operator = '^', operand = Operand, line = Line}, Ctx) ->
    case type_of_ast_node(Operand, Ctx) of
        #basic_type{} = T ->
            dec_pointer_depth(T, Line);
        _ ->
            throw({Line, e_util:fmt("invalid \"^\" on operand ~s", [e_util:expr_to_str(Operand)])})
    end;
type_of_ast_node(#op1_expr{operator = '@', operand = Operand, line = Line}, {_, _, StructMap, _} = Ctx) ->
    case Operand of
        #op2_expr{operator = '.', operand1 = Op1, operand2 = Op2} ->
            T = type_of_struct_field(type_of_ast_node(Op1, Ctx), Op2, StructMap, Line),
            inc_pointer_depth(T, Line);
        #var_ref{} ->
            inc_pointer_depth(type_of_ast_node(Operand, Ctx), Line);
        _ ->
            throw({Line, e_util:fmt("invalid \"@\" on operand ~s", [e_util:expr_to_str(Operand)])})
    end;
type_of_ast_node(#op1_expr{operand = Operand}, Ctx) ->
    type_of_ast_node(Operand, Ctx);
type_of_ast_node(#call_expr{fn = FunExpr, args = Args, line = Line}, Ctx) ->
    ArgTypes = type_of_ast_nodes(Args, Ctx),
    case type_of_ast_node(FunExpr, Ctx) of
        #fn_type{params = FnParamTypes, ret = FnRetType} ->
            case compare_types(ArgTypes, FnParamTypes) of
                true ->
                    FnRetType;
                false ->
                    throw({Line, argumentsErrorInformation(FnParamTypes, ArgTypes)})
            end;
        T ->
            throw({Line, e_util:fmt("invalid function expr: ~s", [type_to_str(T)])})
    end;
type_of_ast_node(#if_stmt{condi = Condi, then = Then, else = Else, line = Line}, Ctx) ->
    type_of_ast_node(Condi, Ctx),
    type_of_ast_nodes(Then, Ctx),
    type_of_ast_nodes(Else, Ctx),
    e_util:void_type(Line);
type_of_ast_node(#while_stmt{condi = Condi, stmts = Exprs, line = Line}, Ctx) ->
    type_of_ast_node(Condi, Ctx),
    type_of_ast_nodes(Exprs, Ctx),
    e_util:void_type(Line);
type_of_ast_node(#return_stmt{expr = Expr, line = Line}, {_, _, _, FnRetType} = Ctx) ->
    RealRet = type_of_ast_node(Expr, Ctx),
    case compare_type(RealRet, FnRetType) of
        true ->
            RealRet;
        false ->
            throw({Line, e_util:fmt("ret type should be (~s), not (~s)", [type_to_str(FnRetType), type_to_str(RealRet)])})
    end;
type_of_ast_node(#var_ref{name = Name, line = Line}, {VarTypes, FnTypeMap, StructMap, _}) ->
    Type =
        case maps:find(Name, VarTypes) of
            error ->
                case maps:find(Name, FnTypeMap) of
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
type_of_ast_node(#array_init_expr{elements = Elements, line = Line}, Ctx) ->
    ElementTypes = type_of_ast_nodes(Elements, Ctx),
    case are_same_type(ElementTypes) of
        true ->
            #array_type{elem_type = hd(ElementTypes), length = length(ElementTypes), line = Line};
        false ->
            throw({Line, e_util:fmt("array init type conflict: {~s}", [join_types_to_str(ElementTypes)])})
    end;
type_of_ast_node(#struct_init_expr{name = Name, field_names = FieldNames, field_value_map = FieldValues, line = Line}, {_, _, StructMap, _} = Ctx) ->
    case maps:find(Name, StructMap) of
        {ok, #struct{field_type_map = FieldTypes}} ->
            check_types_in_struct_fields(FieldNames, FieldTypes, FieldValues, Name, Ctx),
            #basic_type{class = struct, tag = Name, p_depth = 0, line = Line};
        _ ->
            throw({Line, e_util:fmt("struct ~s is not found", [Name])})
    end;
type_of_ast_node(#sizeof_expr{line = Line}, _) ->
    #basic_type{class = integer, p_depth = 0, tag = i64, line = Line};
type_of_ast_node(#goto_stmt{line = Line}, _) ->
    e_util:void_type(Line);
type_of_ast_node(#goto_label{line = Line}, _) ->
    e_util:void_type(Line);
type_of_ast_node(#type_convert{expr = Expr, type = Type, line = Line}, Ctx) ->
    case {type_of_ast_node(Expr, Ctx), Type} of
        {#basic_type{p_depth = D1}, #basic_type{p_depth = D2}} when D1 > 0, D2 > 0 ->
            Type;
        {#basic_type{class = integer, p_depth = 0}, #basic_type{p_depth = D2}} when D2 > 0 ->
            Type;
        {#basic_type{class = integer, p_depth = 0}, #basic_type{class = integer, p_depth = 0}} ->
            Type;
        {ExprType, _} ->
            throw({Line, e_util:fmt("incompatible type: ~w <-> ~w", [ExprType, Type])})
    end;
type_of_ast_node({float, Line, _}, _) ->
    #basic_type{class = float, p_depth = 0, tag = f64, line = Line};
type_of_ast_node({integer, Line, _}, _) ->
    #basic_type{class = integer, p_depth = 0, tag = i64, line = Line};
type_of_ast_node({string, Line, _}, _) ->
    #basic_type{class = integer, p_depth = 1, tag = i8, line = Line}.

-spec argumentsErrorInformation([e_type()], [e_type()]) -> string().
argumentsErrorInformation(FnParamTypes, ArgsTypes) ->
    e_util:fmt("args should be (~s), not (~s)", [join_types_to_str(FnParamTypes), join_types_to_str(ArgsTypes)]).

-spec inc_pointer_depth(e_type(), integer()) -> e_type().
inc_pointer_depth(#basic_type{p_depth = PDepth} = T, _) ->
    T#basic_type{p_depth = PDepth + 1};
inc_pointer_depth(#array_type{elem_type = #basic_type{} = T}, OpLine) ->
    inc_pointer_depth(T, OpLine);
inc_pointer_depth(T, OpLine) ->
    throw({OpLine, e_util:fmt("'@' on type ~s is invalid", [type_to_str(T)])}).

-spec dec_pointer_depth(e_type(), integer()) -> e_type().
dec_pointer_depth(#basic_type{p_depth = PDepth} = T, _) when PDepth > 0 ->
    T#basic_type{p_depth = PDepth - 1};
dec_pointer_depth(T, OpLine) ->
    throw({OpLine, e_util:fmt("'^' on type ~s is invalid", [type_to_str(T)])}).

-spec check_types_in_struct_fields([#var_ref{}], var_type_map(), #{atom() := any()}, atom(), context()) -> ok.
check_types_in_struct_fields(FieldNames, FieldTypes, ValMap, StructName, Ctx) ->
    lists:foreach(fun(V) -> check_struct_field(V, FieldTypes, ValMap, StructName, Ctx) end, FieldNames).

-spec check_struct_field(#var_ref{}, var_type_map(), #{atom() := any()}, atom(), context()) -> ok.
check_struct_field(#var_ref{name = FieldName, line = Line}, FieldTypes, ValMap, StructName, {_, _, StructMap, _} = Ctx) ->
    {ok, Val} = maps:find(FieldName, ValMap),
    ExpectedType = get_field_type(FieldName, FieldTypes, StructName, Line),
    check_type(ExpectedType, StructMap),
    GivenType = type_of_ast_node(Val, Ctx),
    case compare_type(ExpectedType, GivenType) of
        true ->
            ok;
        false ->
            throw({Line, e_util:fmt("~s.~s type error: ~s = ~s", [StructName, FieldName, type_to_str(ExpectedType), type_to_str(GivenType)])})
    end.

-spec are_same_type([e_type()]) -> boolean().
are_same_type([Type, Type | Rest]) ->
    are_same_type([Type | Rest]);
are_same_type([_]) ->
    true;
are_same_type(_) ->
    false.

-spec type_of_struct_field(e_type(), #var_ref{}, struct_type_map(), integer()) -> e_type().
type_of_struct_field(#basic_type{class = struct, tag = Name, p_depth = 0}, #var_ref{name = FieldName}, StructMap, Line) ->
    case maps:find(Name, StructMap) of
        {ok, #struct{field_type_map = FieldTypes}} ->
            get_field_type(FieldName, FieldTypes, Name, Line);
        error ->
            throw({Line, e_util:fmt("struct ~s is not found", [Name])})
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
compare_type(#fn_type{params = P1, ret = R1}, #fn_type{params = P2, ret = R2}) ->
    compare_types(P1, P2) and compare_type(R1, R2);
compare_type(#array_type{elem_type = E1, length = L1}, #array_type{elem_type = E2, length = L2}) ->
    compare_type(E1, E2) and (L1 =:= L2);
compare_type(#basic_type{class = integer, p_depth = 0}, #basic_type{class = integer, p_depth = 0}) ->
    true;
compare_type(#basic_type{class = C, tag = T, p_depth = P}, #basic_type{class = C, tag = T, p_depth = P}) ->
    true;
compare_type(_, _) ->
    false.

-spec is_pointer_and_integer_ordered(e_type(), e_type()) -> {true, e_type()} | false.
is_pointer_and_integer_ordered(#basic_type{p_depth = PDepth} = Type, #basic_type{class = integer, p_depth = 0}) when PDepth > 0 ->
    {true, Type};
is_pointer_and_integer_ordered(_, _) ->
    false.

-spec is_pointer_and_integer(e_type(), e_type()) -> {true, e_type()} | false.
is_pointer_and_integer(#basic_type{p_depth = PDepth} = Type, #basic_type{class = integer, p_depth = 0}) when PDepth > 0 ->
    {true, Type};
is_pointer_and_integer(#basic_type{class = integer, p_depth = 0}, #basic_type{p_depth = PDepth} = Type) when PDepth > 0 ->
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
are_both_integers(#basic_type{p_depth = 0, class = integer}, #basic_type{p_depth = 0, class = integer}) ->
    true;
are_both_integers(_, _) ->
    false.

-spec are_both_floats(e_type(), e_type()) -> boolean().
are_both_floats(#basic_type{p_depth = 0, class = float}, #basic_type{p_depth = 0, class = float}) ->
    true;
are_both_floats(_, _) ->
    false.

-spec type_error_of_op2(atom(), e_type(), e_type()) -> string().
type_error_of_op2(Operator, TypeofOp1, TypeofOp2) ->
    e_util:fmt("type error in \"~s ~s ~s\"", [type_to_str(TypeofOp1), Operator, type_to_str(TypeofOp2)]).

-spec check_types([e_type()], struct_type_map()) -> ok.
check_types(TypeList, StructMap) ->
    lists:foreach(fun(T) -> check_type(T, StructMap) end, TypeList).

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
check_type(#array_type{elem_type = Type}, StructMap) ->
    case Type of
        #array_type{line = Line} ->
            throw({Line, "nested array is not supported"});
        _ ->
            check_type(Type, StructMap)
    end;
check_type(#fn_type{params = Params, ret = RetType}, StructMap) ->
    check_types(Params, StructMap),
    check_type(RetType, StructMap).

-spec join_types_to_str([e_type()]) -> string().
join_types_to_str(Types) ->
    lists:join(",", lists:map(fun type_to_str/1, Types)).

-spec type_to_str(e_type()) -> string().
type_to_str(#fn_type{params = Params, ret = RetType}) ->
    io_lib:format("fun(~s): ~s", [join_types_to_str(Params), type_to_str(RetType)]);
type_to_str(#array_type{elem_type = Type, length = N}) ->
    io_lib:format("{~s, ~w}", [type_to_str(Type), N]);
type_to_str(#basic_type{tag = Tag, p_depth = PDepth}) when PDepth > 0 ->
    io_lib:format("(~s~s)", [Tag, lists:duplicate(PDepth, "^")]);
type_to_str(#basic_type{tag = Tag, p_depth = 0}) ->
    atom_to_list(Tag).
