-module(e_struct).
-export([check_struct_recursion_in_map/1, eliminate_dot_in_ast/2, eliminate_dot_in_stmts/2]).
-include("e_record_definition.hrl").

-spec check_struct_recursion_in_map(#{atom() => #e_struct{}}) -> ok.
check_struct_recursion_in_map(StructMap) ->
  [check_struct_recursion_top(S, StructMap) || _ := S <- StructMap],
  ok.

-spec check_struct_recursion_top(#e_struct{}, #{atom() => #e_struct{}}) -> ok.
check_struct_recursion_top(#e_struct{name = Name, loc = Loc} = Struct, StructMap) ->
  try
    check_struct_recursion(Struct, StructMap, [])
  catch
    {recur, Chain} ->
      Str = string:join([atom_to_list(C) || C <- Chain], "/"),
      e_util:ethrow(Loc, "recursion in struct ~s: ~s", [Name, Str])
  end.

-spec check_struct_recursion(#e_struct{}, #{atom() => #e_struct{}}, [atom()]) -> ok.
check_struct_recursion(#e_struct{name = Name, fields = #e_vars{type_map = FieldTypeMap}},
                       StructMap, UsedStructs) ->
  [check_type_recursion(Type, StructMap, [Name | UsedStructs])
   || _ := Type <- FieldTypeMap],
  ok.

-spec check_type_recursion(e_type(), #{atom() => #e_struct{}}, [atom()]) -> ok.
check_type_recursion(Type, StructMap, UsedStructs) ->
  case contain_unused_struct(Type, StructMap, UsedStructs) of
    {yes, Struct} ->
      check_struct_recursion(Struct, StructMap, UsedStructs);
    no ->
      ok
  end.

-spec contain_unused_struct(e_type(), #{atom() => #e_struct{}}, [atom()]) -> {yes, #e_struct{}} | no.
contain_unused_struct(FieldType, StructMap, UsedStructs) ->
  case contain_struct(FieldType, StructMap) of
    {yes, #e_struct{name = Name}} = R ->
      case lists:member(Name, UsedStructs) of
        true ->
          throw({recur, lists:reverse([Name | UsedStructs])});
        false ->
          R
      end;
    no ->
      no
  end.

-spec contain_struct(e_type(), #{atom() => #e_struct{}}) -> {yes, #e_struct{}} | no.
contain_struct(#e_basic_type{class = struct, p_depth = 0, tag = Name, loc = Loc}, StructMap) ->
  case StructMap of
    #{Name := Struct} ->
      {yes, Struct};
    _ ->
      e_util:ethrow(Loc, "undefined struct \"~s\"", [Name])
  end;
contain_struct(#e_array_type{elem_type = BaseType}, StructMap) ->
  contain_struct(BaseType, StructMap);
contain_struct(_, _) ->
  no.

-spec eliminate_dot_in_ast(e_ast(), e_compile_context:context()) -> e_ast().
eliminate_dot_in_ast([#e_function{vars = LocalVars, stmts = Stmts0} = Fn | Rest],
                     #{vars := GlobalVars} = Ctx) ->
  Vars = e_util:merge_vars(GlobalVars, LocalVars, ignore_tag),
  Stmts1 = eliminate_dot_in_stmts(Stmts0, Ctx#{vars := Vars}),
  [Fn#e_function{stmts = Stmts1} | eliminate_dot_in_ast(Rest, Ctx)];
eliminate_dot_in_ast([Any | Rest], Ctx) ->
  [Any | eliminate_dot_in_ast(Rest, Ctx)];
eliminate_dot_in_ast([], _) ->
  [].

-spec eliminate_dot_in_stmts([e_stmt()], e_compile_context:context()) -> [e_stmt()].
eliminate_dot_in_stmts(Stmts0, Ctx) ->
  Stmts1 = e_util:expr_map(fun(E) -> eliminate_dot(E, Ctx) end, Stmts0),
  e_util:eliminate_pointer(Stmts1).

%% `a.b` will be converted to `(a@ + OFFSET_OF_b)^`.
-spec eliminate_dot(e_expr(), e_compile_context:context()) -> e_expr().
eliminate_dot(?OP2('.', O, ?VREF(FieldName), Loc), #{struct_map := StructMap} = Ctx) ->
  #e_basic_type{class = struct, tag = Name, p_depth = 0} = e_type:type_of_node(O, Ctx),
  #{Name := #e_struct{fields = #e_vars{offset_map = FieldOffsetMap}}} = StructMap,
  #{FieldName := {Offset, Size}} = FieldOffsetMap,
  A = ?OP1('@', eliminate_dot(O, Ctx), Loc),
  B = ?OP2('+', A, ?I(Offset, Loc), Loc),
  ?OP2('^', B, ?I(Size, Loc), Loc);
eliminate_dot(?CALL(Fn, Args) = Op, Ctx) ->
  Op?CALL(eliminate_dot(Fn, Ctx), [eliminate_dot(E, Ctx) || E <- Args]);
eliminate_dot(#e_op{data = Operands} = Op, Ctx) ->
  Op#e_op{data = [eliminate_dot(E, Ctx) || E <- Operands]};
eliminate_dot(#e_type_convert{expr = Expr} = C, Ctx) ->
  C#e_type_convert{expr = eliminate_dot(Expr, Ctx)};
eliminate_dot(Any, _) ->
  Any.
