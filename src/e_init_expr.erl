-module(e_init_expr).
-export([expand_in_ast/2, expand_in_stmts/2]).
-include("e_record_definition.hrl").

-spec expand_in_ast(e_ast(), e_compile_context:context()) -> e_ast().
expand_in_ast([#e_function{stmts = Stmts} = Fn | Rest], Ctx) ->
    e_util:expr_map(fun check_position/1, Stmts),
    [Fn#e_function{stmts = expand_in_stmts(Stmts, Ctx)} | expand_in_ast(Rest, Ctx)];
expand_in_ast([Any | Rest], Ctx) ->
    [Any | expand_in_ast(Rest, Ctx)];
expand_in_ast([], _) ->
    [].

-spec expand_in_stmts([e_stmt()], e_compile_context:context()) -> [e_stmt()].
expand_in_stmts(Stmts, Ctx) ->
    expand(Stmts, [], Ctx).

expand([#e_if_stmt{then = Then, 'else' = Else} = E | Rest], NewAST, Ctx) ->
    expand(Rest, [E#e_if_stmt{then = expand(Then, [], Ctx), 'else' = expand(Else, [], Ctx)} | NewAST], Ctx);
expand([#e_while_stmt{stmts = Stmts} = E | Rest], NewAST, Ctx) ->
    expand(Rest, [E#e_while_stmt{stmts = expand(Stmts, [], Ctx)} | NewAST], Ctx);
expand([#e_op{} = Op | Rest], NewAST, Ctx) ->
    expand(Rest, replace_init_ops(Op, Ctx) ++ NewAST, Ctx);
expand([Any | Rest], NewAST, Ctx) ->
    expand(Rest, [Any | NewAST], Ctx);
expand([], NewAST, _) ->
    lists:reverse(NewAST).

%% for now, array and struct init expression is only allowed in assignment
check_position(?OP2('=', _, #e_struct_init_expr{})) ->
    ok;
check_position(?OP2('=', _, #e_array_init_expr{})) ->
    ok;
check_position(#e_struct_init_expr{loc = Loc}) ->
    e_util:ethrow(Loc, "struct init expression is only allowed in assignments");
check_position(#e_array_init_expr{loc = Loc}) ->
    e_util:ethrow(Loc, "array init expression is only allowed in assignments");
check_position(?CALL(Fn, Args)) ->
    check_position(Fn),
    lists:foreach(fun check_position/1, Args);
check_position(#e_op{data = Data}) ->
    lists:foreach(fun check_position/1, Data);
check_position(#e_type_convert{expr = Expr}) ->
    check_position(Expr);
check_position(_) ->
    ok.

replace_init_ops(?OP2('=', Op1, #e_struct_init_expr{name = Name, loc = Loc, field_value_map = FieldValues}), #{struct_map := StructMap} = Ctx) ->
    Struct = e_util:get_struct_from_name(Name, StructMap, Loc),
    #e_struct{fields = Fields, default_value_map = FieldDefaultMap} = Struct,
    #e_vars{names = FieldNames, type_map = FieldTypeMap} = Fields,
    VarRefs = [?VREF(N, Loc) || N <- FieldNames],
    FieldInitMap = maps:merge(FieldDefaultMap, FieldValues),
    struct_init_to_ops(Op1, VarRefs, FieldInitMap, FieldTypeMap, [], Ctx);
replace_init_ops(?OP2('=', Op1, #e_array_init_expr{elements = Elements, loc = Loc}), Ctx) ->
    array_init_to_ops(Op1, Elements, 0, Loc, [], Ctx);
replace_init_ops(Any, _) ->
    [Any].

struct_init_to_ops(Target, [?VREF(Name, Loc) = Field | Rest], FieldInitMap, FieldTypeMap, NewCode, Ctx) ->
    RValue = maps:get(Name, FieldInitMap, default_value_of(maps:get(Name, FieldTypeMap), Loc)),
    Ops = replace_init_ops(?OP2('=', ?OP2('.', Target, Field, Loc), RValue, Loc), Ctx),
    struct_init_to_ops(Target, Rest, FieldInitMap, FieldTypeMap, Ops ++ NewCode, Ctx);
struct_init_to_ops(_, [], _, _, NewCode, _) ->
    NewCode.

default_value_of(#e_array_type{elem_type = Type, length = Len}, Loc) ->
    #e_array_init_expr{elements = lists:duplicate(Len, default_value_of(Type, Loc)), loc = Loc};
default_value_of(#e_basic_type{class = struct, tag = Tag, p_depth = 0}, Loc) ->
    #e_struct_init_expr{name = Tag, loc = Loc, field_value_map = #{}};
default_value_of(#e_basic_type{class = integer, p_depth = 0}, Loc) ->
    ?I(0, Loc);
default_value_of(#e_basic_type{class = float, p_depth = 0}, Loc) ->
    ?F(0.0, Loc);
default_value_of(#e_basic_type{p_depth = PDepth}, Loc) when PDepth > 0 ->
    ?I(0, Loc);
default_value_of(#e_fn_type{}, Loc) ->
    ?I(0, Loc).

array_init_to_ops(Target, [E | Rest], Cnt, Loc, NewCode, #{wordsize := WordSize} = Ctx) ->
    A = ?OP2('+', ?OP1('@', Target, Loc), ?I(Cnt * WordSize, Loc), Loc),
    B = ?OP2('^', A, ?I(0, Loc), Loc),
    Ops = replace_init_ops(?OP2('=', B, E, Loc), Ctx),
    array_init_to_ops(Target, Rest, Cnt + 1, Loc, Ops ++ NewCode, Ctx);
array_init_to_ops(_, [], _, _, NewCode, _) ->
    NewCode.
