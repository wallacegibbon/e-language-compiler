-module(e_type).
-export([check_types_in_ast/2, check_type_in_stmts/2, type_of_node/2, inc_pointer_depth/3]).
-export([replace_typeof_in_ast/2, replace_typeof_in_stmts/2, replace_typeof_in_vars/2]).
-include("e_record_definition.hrl").
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec check_types_in_ast(e_ast(), e_compile_context:context()) -> ok.
check_types_in_ast([#e_function{name = Name, param_names = [_ | _], attribute = #{interrupt := _},
                                loc = Loc} | _],
                   _) ->
  e_util:ethrow(Loc, "interrupt function \"~s\" should not have parameter(s)", [Name]);
check_types_in_ast([#e_function{type = #e_fn_type{ret = ?VOID()},
                                attribute = #{interrupt := _}} = Fn | Rest],
                   Ctx) ->
  check_types_in_fn(Fn, Ctx),
  check_types_in_ast(Rest, Ctx);
check_types_in_ast([#e_function{name = Name, attribute = #{interrupt := _}, loc = Loc} | _], _) ->
  e_util:ethrow(Loc, "interrupt function \"~s\" should not have return value", [Name]);
check_types_in_ast([#e_function{} = Fn | Rest], Ctx) ->
  check_types_in_fn(Fn, Ctx),
  check_types_in_ast(Rest, Ctx);
check_types_in_ast([#e_struct{name = Name, fields = Fields, default_value_map = ValMap} | Rest],
                   Ctx) ->
  #e_vars{type_map = FieldTypeMap} = Fields,
  [check_type(T, Ctx) || _ := T <- FieldTypeMap],
  %% check the default values for fields
  check_types_in_struct_fields(FieldTypeMap, ValMap, Name, Ctx),
  check_types_in_ast(Rest, Ctx);
check_types_in_ast([_ | Rest], Ctx) ->
  check_types_in_ast(Rest, Ctx);
check_types_in_ast([], _) ->
  ok.

check_types_in_fn(#e_function{vars = LocalVars, param_names = ParamNames, stmts = Stmts,
                              type = FnType, loc = Loc},
                  #{vars := GlobalVars} = Ctx) ->
  Ctx1 = Ctx#{vars := e_util:merge_vars(GlobalVars, LocalVars, ignore_tag)},
  #e_vars{type_map = TypeMap} = LocalVars,
  [check_type(T, Ctx1) || _ := T <- TypeMap],
  [restrict_param_type(T) || _ := T <- maps:with(ParamNames, TypeMap)],
  restrict_ret_type(FnType#e_fn_type.ret),
  check_ret_type(FnType#e_fn_type.ret, Stmts, Loc, top, Ctx1),
  check_type_in_stmts(Stmts, Ctx1).

-spec check_type_in_stmts([e_stmt()], e_compile_context:context()) -> ok.
check_type_in_stmts(Stmts, Ctx) ->
  check_type_of_nodes(Stmts, Ctx).

%% The `typeof` keyword will make the compiler complex without many benifits. So we drop it.
%% Implementing `typeof` is easy. But avoiding the recursive definition problem will make the code complex.
%% (e.g. `fn a(): typeof(a)`, `b: {typeof(b), 10}`, etc.)
-spec replace_typeof_in_ast(e_ast(), e_compile_context:context()) -> e_ast().
replace_typeof_in_ast([#e_function{vars = LocalVars, stmts = Stmts, type = FnType} = Fn | Rest],
                      #{vars := GlobalVars} = Ctx) ->
  Ctx1 = Ctx#{vars := e_util:merge_vars(GlobalVars, LocalVars, ignore_tag)},
  Fn1 = Fn#e_function{vars = replace_typeof_in_vars(LocalVars, Ctx1),
                      type = replace_typeof_in_type(FnType, Ctx1),
                      stmts = replace_typeof_in_stmts(Stmts, Ctx1)},
  [Fn1 | replace_typeof_in_ast(Rest, Ctx)];
replace_typeof_in_ast([#e_struct{fields = Fields, default_value_map = DefaultValueMap} = S | Rest],
                      Ctx) ->
  S1 = S#e_struct{fields = replace_typeof_in_vars(Fields, Ctx),
                  default_value_map = #{K => replace_typeof(V, Ctx) || K := V <- DefaultValueMap}},
  [S1 | replace_typeof_in_ast(Rest, Ctx)];
replace_typeof_in_ast([Any | Rest], Ctx) ->
  [Any | replace_typeof_in_ast(Rest, Ctx)];
replace_typeof_in_ast([], _) ->
  [].

-spec replace_typeof_in_stmts([e_stmt()], e_compile_context:context()) -> [e_stmt()].
replace_typeof_in_stmts(Stmts, Ctx) ->
  e_util:expr_map(fun(E) -> replace_typeof(E, Ctx) end, Stmts).

-spec replace_typeof_in_vars(#e_vars{}, e_compile_context:context()) -> #e_vars{}.
replace_typeof_in_vars(#e_vars{type_map = TypeMap} = Vars, Ctx) ->
  Vars#e_vars{type_map = #{K => replace_typeof_in_type(T, Ctx) || K := T <- TypeMap}}.

-spec replace_typeof(e_expr(), e_compile_context:context()) -> e_expr().
replace_typeof(#e_type_convert{type = #e_typeof{expr = Expr}} = E, Ctx) ->
  E#e_type_convert{type = type_of_node(Expr, Ctx)};
replace_typeof(?AREF(Arr, Index) = E, Ctx) ->
  E?AREF(replace_typeof(Arr, Ctx), replace_typeof(Index, Ctx));
replace_typeof(?CALL(Fn, Args) = E, Ctx) ->
  E?CALL(replace_typeof(Fn, Ctx), [replace_typeof(V, Ctx) || V <- Args]);
replace_typeof(#e_op{tag = {sizeof, Type}} = E, Ctx) ->
  E#e_op{tag = {sizeof, replace_typeof_in_type(Type, Ctx)}};
replace_typeof(#e_op{tag = {alignof, Type}} = E, Ctx) ->
  E#e_op{tag = {alignof, replace_typeof_in_type(Type, Ctx)}};
replace_typeof(#e_op{data = Data} = E, Ctx) ->
  E#e_op{data = [replace_typeof(V, Ctx) || V <- Data]};
replace_typeof(#e_type_convert{expr = Expr} = C, Ctx) ->
  C#e_type_convert{expr = replace_typeof(Expr, Ctx)};
replace_typeof(Any, _) ->
  Any.

%% `typeof` can appear in another type like array `{typeof(a), 10}` or
%% convert expressions like `a as typeof(b)`.
-spec replace_typeof_in_type(e_type(), e_compile_context:context()) -> e_type().
replace_typeof_in_type(#e_array_type{elem_type = ElemType} = Type, Ctx) ->
  Type#e_array_type{elem_type = replace_typeof_in_type(ElemType, Ctx)};
replace_typeof_in_type(#e_fn_type{params = Params, ret = Ret} = Type, Ctx) ->
  Type#e_fn_type{params = [replace_typeof_in_type(T, Ctx) || T <- Params],
                 ret = replace_typeof_in_type(Ret, Ctx)};
replace_typeof_in_type(#e_basic_type{} = Type, _) ->
  Type;
replace_typeof_in_type(#e_typeof{expr = Expr}, Ctx) ->
  replace_typeof_in_type(type_of_node(Expr, Ctx), Ctx).

-spec check_type_of_nodes([e_stmt()], e_compile_context:context()) -> ok.
check_type_of_nodes(Stmts, Ctx) ->
  [type_of_node(S, Ctx) || S <- Stmts],
  ok.

-spec type_of_nodes([e_stmt()], e_compile_context:context()) -> [e_type()].
type_of_nodes(Stmts, Ctx) ->
  [type_of_node(S, Ctx) || S <- Stmts].

-spec type_of_node(e_stmt(), e_compile_context:context()) -> e_type().
type_of_node(?VREF(Name, Loc), #{vars := #e_vars{type_map = TypeMap}, fn_map := FnTypeMap} = Ctx) ->
  case e_util:map_find_multi(Name, [TypeMap, FnTypeMap]) of
    {ok, Type} ->
      %% The `loc` of the found type should be updated to the `loc` of the `e_varref`.
      check_type(setelement(2, Type, Loc), Ctx);
    notfound ->
      e_util:ethrow(Loc, "variable ~s is undefined", [Name])
  end;
type_of_node(?AREF(Arr, Index, Loc), Ctx) ->
  ArrType = type_of_node(Arr, Ctx),
  IndexType = type_of_node(Index, Ctx),
  case {ArrType, IndexType} of
    {#e_basic_type{p_depth = N}, #e_basic_type{class = integer, p_depth = 0}} when N > 0 ->
      inc_pointer_depth(ArrType, -1, Loc);
    {#e_basic_type{p_depth = N}, _} when N > 0 ->
      e_util:ethrow(Loc, "invalid index type: ~s", [type_to_str(IndexType)]);
    _ ->
      e_util:ethrow(Loc, "\"[]\" on wrong type: ~s", [type_to_str(ArrType)])
  end;
type_of_node(?CALL(Fn, Args, Loc), Ctx) ->
  ArgTypes = type_of_nodes(Args, Ctx),
  case type_of_node(Fn, Ctx) of
    #e_fn_type{params = FnParamTypes, ret = FnRetType} ->
      case compare_types(ArgTypes, FnParamTypes, Ctx) of
        true ->
          FnRetType;
        false ->
          e_util:ethrow(Loc, arguments_error_info(FnParamTypes, ArgTypes))
      end;
    T ->
      e_util:ethrow(Loc, "invalid function type: ~s", [type_to_str(T)])
  end;
type_of_node(?OP2('=', ?OP2('.', _, _) = Op1, Op2, Loc), Ctx) ->
  compare_assign_types(type_of_node(Op1, Ctx), type_of_node(Op2, Ctx), Loc, Ctx);
type_of_node(?OP2('=', ?OP2('^', _, _) = Op1, Op2, Loc), Ctx) ->
  compare_assign_types(type_of_node(Op1, Ctx), type_of_node(Op2, Ctx), Loc, Ctx);
type_of_node(?OP2('=', ?AREF(_, _) = Op1, Op2, Loc), Ctx) ->
  compare_assign_types(type_of_node(Op1, Ctx), type_of_node(Op2, Ctx), Loc, Ctx);
type_of_node(?OP2('=', ?VREF(_) = Op1, Op2, Loc), Ctx) ->
  compare_assign_types(type_of_node(Op1, Ctx), type_of_node(Op2, Ctx), Loc, Ctx);
type_of_node(?OP2('=', Any, _, Loc), _) ->
  e_util:ethrow(Loc, "invalid left value (~s)", [e_util:stmt_to_str(Any)]);
type_of_node(?OP2('.', Op1, Op2, Loc), #{struct_map := StructMap} = Ctx) ->
  T1 = type_of_struct_field(type_of_node(Op1, Ctx), Op2, StructMap, Loc),
  check_type(T1, Ctx);
type_of_node(?OP2('+', Op1, Op2, Loc), Ctx) ->
  Op1Type = type_of_node(Op1, Ctx),
  Op2Type = type_of_node(Op2, Ctx),
  Checkers = [fun are_numbers_of_same_type/2, fun are_pointer_and_integer_ignore_order/2],
  case number_check_chain(Op1Type, Op2Type, Checkers) of
    {true, T} ->
      T;
    false ->
      e_util:ethrow(Loc, type_error_of('+', Op1Type, Op2Type))
  end;
%% `integer + pointer` is valid, but `integer - pointer` is invalid
type_of_node(?OP2('-', Op1, Op2, Loc), Ctx) ->
  Op1Type = type_of_node(Op1, Ctx),
  Op2Type = type_of_node(Op2, Ctx),
  case are_pointers_of_same_type(Op1Type, Op2Type) of
    true ->
      %% pointer - pointer --> integer.
      #e_basic_type{class = integer, tag = word, loc = Loc};
    false ->
      Checkers = [fun are_numbers_of_same_type/2, fun are_pointer_and_integer/2],
      case number_check_chain(Op1Type, Op2Type, Checkers) of
        {true, T} ->
          T;
        false ->
          e_util:ethrow(Loc, type_error_of('-', Op1Type, Op2Type))
      end
  end;
type_of_node(?OP2('^', Operand, _, Loc), Ctx) ->
  case type_of_node(Operand, Ctx) of
    #e_basic_type{} = T ->
      inc_pointer_depth(T, -1, Loc);
    _ ->
      e_util:ethrow(Loc, "invalid \"^\" on operand ~s", [e_util:stmt_to_str(Operand)])
  end;
type_of_node(?OP2(Tag, Op1, Op2, Loc), Ctx) when Tag =:= '*'; Tag =:= '/' ->
  Op1Type = type_of_node(Op1, Ctx),
  Op2Type = type_of_node(Op2, Ctx),
  case are_numbers_of_same_type(Op1Type, Op2Type) of
    {true, T} ->
      T;
    false ->
      e_util:ethrow(Loc, type_error_of(Tag, Op1Type, Op2Type))
  end;
%% Boolean operator accepts booleans and returns a boolean.
type_of_node(?OP2(Tag, Op1, Op2, Loc), Ctx) when ?IS_LOGIC(Tag) ->
  case {type_of_node(Op1, Ctx), type_of_node(Op2, Ctx)} of
    {#e_basic_type{class = boolean}, #e_basic_type{class = boolean}} ->
      #e_basic_type{class = boolean, loc = Loc};
    {T1, T2} ->
      e_util:ethrow(Loc, type_error_of(Tag, T1, T2))
  end;
%% Comparing operator accepts integers and returns a boolean.
type_of_node(?OP2(Tag, Op1, Op2, Loc), Ctx) when ?IS_COMPARE(Tag) ->
  Op1Type = type_of_node(Op1, Ctx),
  Op2Type = type_of_node(Op2, Ctx),
  case are_same_type_ignore_pos(Op1Type, Op2Type) of
    true ->
      #e_basic_type{class = boolean, loc = Loc};
    false ->
      e_util:ethrow(Loc, type_error_of(Tag, Op1Type, Op2Type))
  end;
%% the left operators are integer operators: band, bor, bxor, bsl, bsr, >, <, ...
type_of_node(?OP2(Tag, Op1, Op2, Loc), Ctx) ->
  Op1Type = type_of_node(Op1, Ctx),
  Op2Type = type_of_node(Op2, Ctx),
  case are_integers(Op1Type, Op2Type) of
    {true, Type} ->
      Type;
    false ->
      e_util:ethrow(Loc, type_error_of(Tag, Op1Type, Op2Type))
  end;
type_of_node(?OP1('@', Operand, Loc), Ctx) ->
  inc_pointer_depth(type_of_node(Operand, Ctx), 1, Loc);
type_of_node(?OP1('not', Operand, Loc), Ctx) ->
  case type_of_node(Operand, Ctx) of
    #e_basic_type{class = boolean} ->
      #e_basic_type{class = boolean, loc = Loc};
    _ ->
      e_util:ethrow(Loc, "invalid operand type for 'not'")
  end;
type_of_node(#e_op{tag = {sizeof, _}, loc = Loc}, _) ->
  #e_basic_type{class = integer, tag = word, loc = Loc};
type_of_node(#e_op{tag = {alignof, _}, loc = Loc}, _) ->
  #e_basic_type{class = integer, tag = word, loc = Loc};
type_of_node(?OP1(_, Operand), Ctx) ->
  type_of_node(Operand, Ctx);
type_of_node(#e_array_init_expr{elements = [?OP2('=', _, _, Loc) | _]}, _) ->
  e_util:ethrow(Loc, "invalid syntax for array init expression");
type_of_node(#e_array_init_expr{elements = Elements, loc = Loc}, Ctx) ->
  ElementTypes = type_of_nodes(Elements, Ctx),
  case are_same_type(ElementTypes) of
    true ->
      #e_array_type{elem_type = hd(ElementTypes), length = length(ElementTypes), loc = Loc};
    false ->
      e_util:ethrow(Loc, "array init type conflict: {~s}", [join_types_to_str(ElementTypes)])
  end;
type_of_node(#e_struct_init_expr{name = Name, field_value_map = ValMap, loc = Loc},
             #{struct_map := StructMap} = Ctx) ->
  case StructMap of
    #{Name := #e_struct{fields = #e_vars{type_map = FieldTypeMap}}} ->
      check_types_in_struct_fields(FieldTypeMap, ValMap, Name, Ctx),
      #e_basic_type{class = struct, tag = Name, loc = Loc};
    _ ->
      e_util:ethrow(Loc, "type ~s is not found", [Name])
  end;
type_of_node(#e_type_convert{expr = Expr, type = Type, loc = Loc}, Ctx) ->
  convert_type(type_of_node(Expr, Ctx), Type, Loc);
type_of_node(?I(_, Loc), _) ->
  #e_basic_type{class = integer, tag = word, loc = Loc};
type_of_node(?F(_, Loc), _) ->
  #e_basic_type{class = float, tag = float, loc = Loc};
type_of_node(?S(_, Loc), _) ->
  #e_basic_type{class = integer, p_depth = 1, tag = byte, loc = Loc};
type_of_node(#e_if_stmt{'cond' = Cond, then = Then, 'else' = Else, loc = Loc}, Ctx) ->
  case type_of_node(Cond, Ctx) of
    #e_basic_type{class = boolean} ->
      ok;
    _ ->
      e_util:ethrow(Loc, "invalid boolean expression for if")
  end,
  check_type_of_nodes(Then, Ctx),
  check_type_of_nodes(Else, Ctx),
  ?VOID(Loc);
type_of_node(#e_while_stmt{'cond' = Cond, stmts = Stmts, loc = Loc}, Ctx) ->
  case type_of_node(Cond, Ctx) of
    #e_basic_type{class = boolean} ->
      ok;
    _ ->
      e_util:ethrow(Loc, "invalid boolean expression for while")
  end,
  type_of_node(Cond, Ctx),
  check_type_of_nodes(Stmts, Ctx),
  ?VOID(Loc);
type_of_node(#e_return_stmt{expr = none, loc = Loc}, _) ->
  ?VOID(Loc);
type_of_node(#e_return_stmt{expr = Expr}, Ctx) ->
  %% Return type will be checked outside since we need to deal with the situation
  %% that `return` statement is missing.
  type_of_node(Expr, Ctx);
type_of_node(#e_goto_stmt{loc = Loc}, _) ->
  ?VOID(Loc);
type_of_node(#e_label{loc = Loc}, _) ->
  ?VOID(Loc);
type_of_node(Any, _) ->
  e_util:ethrow(element(2, Any), "invalid statement: ~s~n", [e_util:stmt_to_str(Any)]).

-spec convert_type(e_type(), e_type(), location()) -> e_type().
convert_type(Type1, Type2, Loc) ->
  case type_compatible(Type1, Type2) orelse type_compatible(Type2, Type1) of
    true ->
      Type2;
    false ->
      e_util:ethrow(Loc, type_error_of(to, Type1, Type2))
  end.

%% Functions can be converted to any kind of pointers in current design.
type_compatible(#e_fn_type{loc = Loc}, Type2) ->
  type_compatible(#e_basic_type{class = integer, p_depth = 1, loc = Loc}, Type2);
type_compatible(#e_basic_type{class = integer},
                #e_basic_type{p_depth = N}) when N > 0 ->
  true;
type_compatible(#e_basic_type{class = integer},
                #e_basic_type{class = integer}) ->
  true;
type_compatible(#e_basic_type{class = float, p_depth = N},
                #e_basic_type{class = float, p_depth = N}) ->
  true;
type_compatible(#e_basic_type{p_depth = N1},
                #e_basic_type{p_depth = N2}) when N1 > 0, N2 > 0 ->
  true;
type_compatible(_, _) ->
  false.

-spec compare_assign_types(e_type(), e_type(), location(),
                           e_compile_context:context()) -> e_type().
compare_assign_types(Type1, Type2, Loc, Ctx) ->
  case compare_type(Type1, Type2, Ctx) of
    true ->
      Type1;
    false ->
      e_util:ethrow(Loc, type_error_of('=', Type1, Type2))
  end.

-spec arguments_error_info([e_type()], [e_type()]) -> string().
arguments_error_info(FnParamTypes, ArgsTypes) ->
  e_util:fmt("args should be (~s), not (~s)",
             [join_types_to_str(FnParamTypes), join_types_to_str(ArgsTypes)]).

-spec inc_pointer_depth(e_type(), integer(), location()) -> e_type().
inc_pointer_depth(#e_basic_type{p_depth = P} = T, N, _) when P + N >= 0 ->
  T#e_basic_type{p_depth = P + N};
inc_pointer_depth(#e_array_type{elem_type = #e_basic_type{} = T}, N, Loc) ->
  inc_pointer_depth(T, N, Loc);
inc_pointer_depth(T, N, Loc) ->
  e_util:ethrow(Loc, "increase pointer depth by ~w on type <~s> is invalid",
                [N, type_to_str(T)]).

-spec check_types_in_struct_fields(#{atom() => e_type()}, #{atom() := e_expr()}, atom(),
                                   e_compile_context:context()) -> ok.
check_types_in_struct_fields(FieldTypeMap, ValMap, StructName, Ctx) ->
  [check_struct_field(FieldTypeMap, N, Val, StructName, Ctx)
   || N := Val <- ValMap],
  ok.

-spec check_struct_field(#{atom() => e_type()}, atom(), e_expr(), atom(),
                         e_compile_context:context()) -> ok.
check_struct_field(FieldTypeMap, FieldName, Val, StructName, Ctx) ->
  Loc = element(2, Val),
  ExpectedType = get_field_type(FieldName, FieldTypeMap, StructName, Loc),
  %% there may be `typeof` inside `ExpectedType`, call `check_type` to resolve `typeof`.
  ExpectedTypeFixed = check_type(ExpectedType, Ctx),
  GivenType = type_of_node(Val, Ctx),
  case compare_type(ExpectedTypeFixed, GivenType, Ctx) of
    true ->
      ok;
    false ->
      e_util:ethrow(Loc, type_error_of('=', ExpectedTypeFixed, GivenType))
  end.

-spec are_same_type([e_type()]) -> boolean().
are_same_type([#e_basic_type{class = integer, p_depth = 0},
               #e_basic_type{class = integer, p_depth = 0} = T | Rest]) ->
  are_same_type([T | Rest]);
are_same_type([#e_basic_type{class = float, p_depth = 0},
               #e_basic_type{class = float, p_depth = 0} = T | Rest]) ->
  are_same_type([T | Rest]);
are_same_type([T1, T2 | Rest]) ->
  case are_same_type_ignore_pos(T1, T2) of
    true ->
      are_same_type([T2 | Rest]);
    false ->
      false
  end;
are_same_type([_]) ->
  true;
are_same_type(_) ->
  false.

-spec are_same_type_ignore_pos(e_type(), e_type()) -> boolean().
are_same_type_ignore_pos(T1, T2) ->
  setelement(2, T1, {"", 0, 0}) =:= setelement(2, T2, {"", 0, 0}).

-spec type_of_struct_field(e_type(), #e_varref{}, #{atom() => #e_struct{}},
                           location()) -> e_type().
type_of_struct_field(#e_basic_type{class = struct, tag = Name, p_depth = 0} = S,
                     ?VREF(FieldName), StructMap, Loc) ->
  #e_struct{fields = Fields} = e_util:get_struct_from_type(S, StructMap),
  #e_vars{type_map = FieldTypeMap} = Fields,
  get_field_type(FieldName, FieldTypeMap, Name, Loc);
type_of_struct_field(T, _, _, Loc) ->
  e_util:ethrow(Loc, "the left operand for \".\" is the wrong type: ~s", [type_to_str(T)]).

-spec get_field_type(atom(), #{atom() => e_type()}, atom(), location()) -> e_type().
get_field_type(FieldName, FieldTypeMap, StructName, Loc) ->
  case FieldTypeMap of
    #{FieldName := Type} ->
      Type;
    _ ->
      e_util:ethrow(Loc, "~s.~s does not exist", [StructName, FieldName])
  end.

-spec compare_types([e_type()], [e_type()], e_compile_context:context()) -> boolean().
compare_types([T1 | Types1], [T2 | Types2], Ctx) ->
  case compare_type(T1, T2, Ctx) of
    true ->
      compare_types(Types1, Types2, Ctx);
    false ->
      false
  end;
compare_types([], [], _) ->
  true;
compare_types(_, _, _) ->
  false.

-spec compare_type(e_type(), e_type(), e_compile_context:context()) -> boolean().
compare_type(T1, T2, Ctx) ->
  compare_type_1(T1, T2, Ctx) orelse compare_type_1(T2, T1, Ctx).

-spec compare_type_1(e_type(), e_type(), e_compile_context:context()) -> boolean().
compare_type_1(#e_fn_type{params = P1, ret = R1}, #e_fn_type{params = P2, ret = R2}, Ctx) ->
  compare_types(P1, P2, Ctx) andalso compare_type_1(R1, R2, Ctx);
compare_type_1(#e_array_type{elem_type = E1, length = L1},
               #e_array_type{elem_type = E2, length = L2}, Ctx) ->
  (L1 =:= L2) andalso compare_type_1(E1, E2, Ctx);
compare_type_1(#e_basic_type{class = integer, p_depth = 0},
               #e_basic_type{class = integer, p_depth = 0}, _) ->
  true;
compare_type_1(#e_basic_type{class = C, tag = T, p_depth = P},
               #e_basic_type{class = C, tag = T, p_depth = P}, _) ->
  true;
compare_type_1(#e_basic_type{class = struct, tag = Tag, p_depth = N, loc = Loc},
               T2, Ctx) when N > 0 ->
  #{struct_map := StructMap} = Ctx,
  #{Tag := #e_struct{fields = Fields}} = StructMap,
  #e_vars{names = [First | _], type_map = TypeMap} = Fields,
  case TypeMap of
    #{First := #e_basic_type{class = struct} = T1Sub} ->
      compare_type_1(inc_pointer_depth(T1Sub, N, Loc), T2, Ctx);
    _ ->
      false
  end;
compare_type_1(_, _, _) ->
  false.


-type number_check_result() :: {true, e_type()} | false.
-type number_check_fn() :: fun((e_type(), e_type()) -> number_check_result()).

-spec number_check_chain(e_type(), e_type(), [number_check_fn()]) -> number_check_result().
number_check_chain(T1, T2, [Fn | RestFns]) ->
  case Fn(T1, T2) of
    {true, _} = R ->
      R;
    false ->
      number_check_chain(T1, T2, RestFns)
  end;
number_check_chain(_, _, []) ->
  false.

-spec are_pointer_and_integer_ignore_order(e_type(), e_type()) -> number_check_result().
are_pointer_and_integer_ignore_order(T1, T2) ->
  case are_pointer_and_integer(T1, T2) of
    {true, _} = R ->
      R;
    false ->
      are_pointer_and_integer(T2, T1)
  end.

-spec are_pointer_and_integer(e_type(), e_type()) -> number_check_result().
are_pointer_and_integer(#e_basic_type{p_depth = N} = Type,
                        #e_basic_type{class = integer, p_depth = 0}) when N > 0 ->
  {true, Type};
are_pointer_and_integer(#e_fn_type{} = Type,
                        #e_basic_type{class = integer, p_depth = 0}) ->
  {true, Type};
are_pointer_and_integer(_, _) ->
  false.

-spec are_pointers_of_same_type(e_type(), e_type()) -> boolean().
are_pointers_of_same_type(#e_basic_type{class = C, tag = T, p_depth = N},
                          #e_basic_type{class = C, tag = T, p_depth = N}) ->
  true;
are_pointers_of_same_type(#e_fn_type{}, #e_fn_type{}) ->
  true;
are_pointers_of_same_type(_, _) ->
  false.

-spec are_numbers_of_same_type(e_type(), e_type()) -> number_check_result().
are_numbers_of_same_type(T1, T2) ->
  number_check_chain(T1, T2, [fun are_integers/2, fun are_floats/2]).

-spec are_integers(e_type(), e_type()) -> number_check_result().
are_integers(#e_basic_type{class = integer, p_depth = 0} = T1,
             #e_basic_type{class = integer, p_depth = 0} = T2) ->
  {true, bigger_type(T1, T2)};
are_integers(_, _) ->
  false.

-spec are_floats(e_type(), e_type()) -> number_check_result().
are_floats(#e_basic_type{class = float, p_depth = 0} = T1,
           #e_basic_type{class = float, p_depth = 0} = T2) ->
  {true, bigger_type(T1, T2)};
are_floats(_, _) ->
  false.

-spec bigger_type(e_type(), e_type()) -> e_type().
bigger_type(#e_basic_type{class = integer, tag = word} = T1,
            #e_basic_type{class = integer}) ->
  T1;
bigger_type(#e_basic_type{class = integer},
            #e_basic_type{class = integer} = T2) ->
  T2;
bigger_type(#e_basic_type{class = float} = T,
            #e_basic_type{class = float}) ->
  T.

-ifdef(EUNIT).

-define(IOBJ(Tag, PDepth), #e_basic_type{class = integer, tag = Tag, p_depth = PDepth}).
-define(FOBJ(PDepth), #e_basic_type{class = float, tag = float, p_depth = PDepth}).

bigger_type_test() ->
  ?assertMatch(?IOBJ(word, 0), bigger_type(?IOBJ(byte, 0), ?IOBJ(word, 0))),
  ok.

are_numbers_of_same_type_test() ->
  ?assertMatch({true, ?IOBJ(word, 0)},
               are_numbers_of_same_type(?IOBJ(word, 0), ?IOBJ(byte, 0))),
  ?assertMatch(false,
               are_numbers_of_same_type(?IOBJ(i16, 0), ?FOBJ(0))),
  ok.

number_check_chain_test() ->
  L1 = [fun are_numbers_of_same_type/2,
        fun are_pointers_of_same_type/2,
        fun are_pointer_and_integer/2],
  ?assertMatch({true, ?IOBJ(word, 0)},
               number_check_chain(?IOBJ(word, 0), ?IOBJ(byte, 0), L1)),
  L2 = [fun are_numbers_of_same_type/2,
        fun are_pointers_of_same_type/2,
        fun are_pointer_and_integer/2],
  ?assertMatch(false,
               number_check_chain(?IOBJ(word, 0), ?IOBJ(byte, 1), L2)),
  L3 = [fun are_numbers_of_same_type/2,
        fun are_pointers_of_same_type/2,
        fun are_pointer_and_integer_ignore_order/2],
  ?assertMatch({true, ?IOBJ(byte, 1)},
               number_check_chain(?IOBJ(word, 0), ?IOBJ(byte, 1), L3)),
  ok.

-endif.

-spec type_error_of(atom(), e_type(), e_type()) -> string().
type_error_of(Tag, TypeofOp1, TypeofOp2) ->
  e_util:fmt("type error: <~s> ~s <~s>", [type_to_str(TypeofOp1), Tag, type_to_str(TypeofOp2)]).

%% check type, ensure that all struct used by type exists.
-spec check_type(e_type(), e_compile_context:context()) -> e_type().
check_type(#e_basic_type{class = struct} = Type, #{struct_map := StructMap}) ->
  #e_struct{} = e_util:get_struct_from_type(Type, StructMap),
  Type;
check_type(#e_basic_type{} = Type, _) ->
  Type;
check_type(#e_array_type{elem_type = #e_array_type{loc = Loc}}, _) ->
  e_util:ethrow(Loc, "nested array is not supported");
check_type(#e_array_type{elem_type = ElemType} = Type, Ctx) ->
  check_type(ElemType, Ctx),
  Type;
check_type(#e_fn_type{params = Params, ret = RetType} = Type, Ctx) ->
  [check_type(T, Ctx) || T <- Params],
  check_type(RetType, Ctx),
  Type;
check_type(#e_typeof{expr = Expr}, Ctx) ->
  check_type(type_of_node(Expr, Ctx), Ctx).

-spec restrict_param_type(e_type()) -> ok.
restrict_param_type(#e_basic_type{p_depth = N}) when N > 0 ->
  ok;
restrict_param_type(#e_basic_type{class = C}) when C =:= integer; C =:= float ->
  ok;
restrict_param_type(T) ->
  e_util:ethrow(element(2, T), "invalid parameter type here").

-spec restrict_ret_type(e_type()) -> ok.
restrict_ret_type(#e_basic_type{p_depth = N}) when N > 0 ->
  ok;
restrict_ret_type(#e_basic_type{class = C}) when C =:= integer; C =:= float; C =:= void ->
  ok;
restrict_ret_type(#e_fn_type{}) ->
  ok;
restrict_ret_type(T) ->
  e_util:ethrow(element(2, T), "invalid returning type here").

-spec check_ret_type(e_type(), [e_stmt()], location(), top | inner, e_compile_context:context()) -> ok.
check_ret_type(RetType, [#e_return_stmt{loc = Loc} = Expr], _, _, Ctx) ->
  RetTrueType = type_of_node(Expr, Ctx),
  case compare_type(RetType, RetTrueType, Ctx) of
    true ->
      ok;
    false ->
      e_util:ethrow(Loc, "return type declared: ~s, inferred ~s",
                    [type_to_str(RetType), type_to_str(RetTrueType)])
  end;
check_ret_type(RetType, [#e_return_stmt{loc = Loc} = Expr | Rest], _, Scope, Ctx) ->
  RetTrueType = type_of_node(Expr, Ctx),
  case compare_type(RetType, RetTrueType, Ctx) of
    true ->
      check_ret_type(RetType, Rest, Loc, Scope, Ctx);
    false ->
      e_util:ethrow(Loc, "return type declared: ~s, inferred ~s",
                    [type_to_str(RetType), type_to_str(RetTrueType)])
  end;
check_ret_type(RetType, [#e_if_stmt{then = Then, 'else' = Else} | Rest], Loc, Scope, Ctx) ->
  check_ret_type(RetType, Then, Loc, inner, Ctx),
  check_ret_type(RetType, Else, Loc, inner, Ctx),
  check_ret_type(RetType, Rest, Loc, Scope, Ctx);
check_ret_type(RetType, [#e_while_stmt{stmts = Body} | Rest], Loc, Scope, Ctx) ->
  check_ret_type(RetType, Body, Loc, inner, Ctx),
  check_ret_type(RetType, Rest, Loc, Scope, Ctx);
check_ret_type(RetType, [_ | Rest], Loc, Scope, Ctx) ->
  check_ret_type(RetType, Rest, Loc, Scope, Ctx);
check_ret_type(#e_basic_type{class = void}, [], _, _, _) ->
  ok;
check_ret_type(RetType, [], Loc, top, Ctx) ->
  check_ret_type(RetType, [#e_return_stmt{expr = none, loc = Loc}], Loc, top, Ctx);
check_ret_type(_, [], _, _, _) ->
  ok.

-spec join_types_to_str([e_type()]) -> string().
join_types_to_str(Types) ->
  lists:join(",", [type_to_str(T) || T <- Types]).

-spec type_to_str(e_type()) -> string().
type_to_str(#e_typeof{expr = Expr}) ->
  e_util:fmt("typeof(~s)", [e_util:stmt_to_str(Expr)]);
type_to_str(#e_fn_type{params = Params, ret = RetType}) ->
  e_util:fmt("fn(~s): ~s", [join_types_to_str(Params), type_to_str(RetType)]);
type_to_str(#e_array_type{elem_type = Type, length = N}) ->
  e_util:fmt("{~s, ~w}", [type_to_str(Type), N]);
type_to_str(#e_struct_init_expr{name = Name}) ->
  atom_to_list(Name);
type_to_str(#e_basic_type{tag = Tag, p_depth = N}) ->
  e_util:fmt("~s~s", [Tag, lists:duplicate(N, "^")]).
