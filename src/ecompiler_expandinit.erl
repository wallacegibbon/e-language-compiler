-module(ecompiler_expandinit).

-export([expand_initexpr_infun/2, expand_initexprs/2]).

-import(ecompiler_util, [exprsmap/2, flat_format/2]).

-include("./ecompiler_frame.hrl").

%% for now, array and struct init expression is only allowed in assignment
check_initexpr_pos(#op2{operator = assign, op2 = Op2})
    when is_record(Op2, struct_init);
         is_record(Op2, array_init) ->
    ok;
check_initexpr_pos(#struct_init{line = Line}) ->
    throw({Line,
           "struct init expression is only allowed "
           "in assignments"});
check_initexpr_pos(#array_init{line = Line}) ->
    throw({Line,
           "array init expression is only allowed "
           "in assignments"});
check_initexpr_pos(_) -> ok.

expand_initexpr_infun([#function{exprs = Exprs} = F
                       | Rest],
                      StructMap) ->
    exprsmap(fun check_initexpr_pos/1, Exprs),
    [F#function{exprs = expand_initexprs(Exprs, StructMap)}
     | expand_initexpr_infun(Rest, StructMap)];
expand_initexpr_infun([Any | Rest], StructMap) ->
    [Any | expand_initexpr_infun(Rest, StructMap)];
expand_initexpr_infun([], _) -> [].

expand_initexprs(Exprs, StructMap) ->
    expand_init(Exprs, [], StructMap).

expand_init([#if_expr{then = Then, else = Else} = E
             | Rest],
            NewAst, StructMap) ->
    expand_init(Rest,
                [E#if_expr{then = expand_init(Then, [], StructMap),
                           else = expand_init(Else, [], StructMap)}
                 | NewAst],
                StructMap);
expand_init([#while_expr{exprs = Exprs} = E | Rest],
            NewAst, StructMap) ->
    expand_init(Rest,
                [E#while_expr{exprs = expand_init(Exprs, [], StructMap)}
                 | NewAst],
                StructMap);
expand_init([#op2{} = Op | Rest], NewAst, StructMap) ->
    expand_init(Rest,
                replace_init_ops(Op, StructMap) ++ NewAst,
                StructMap);
expand_init([Any | Rest], NewAst, StructMap) ->
    expand_init(Rest, [Any | NewAst], StructMap);
expand_init([], NewAst, _) -> lists:reverse(NewAst).

replace_init_ops(#op2{operator = assign, op1 = Op1,
                      op2 =
                          #struct_init{name = Name, field_values = FieldValues,
                                       line = Line}},
                 StructMap) ->
    case maps:find(Name, StructMap) of
        {ok,
         #struct{field_names = FieldNames,
                 field_types = FieldTypes,
                 field_defaults = FieldDefaults}} ->
            FieldValueMap = maps:merge(FieldDefaults, FieldValues),
            structinit_to_op(Op1,
                             FieldNames,
                             FieldValueMap,
                             FieldTypes,
                             [],
                             StructMap);
        error ->
            throw({Line,
                   flat_format("struct ~s is not found", [Name])})
    end;
replace_init_ops(#op2{operator = assign, op1 = Op1,
                      op2 = #array_init{elements = Elements, line = Line}},
                 StructMap) ->
    arrayinit_to_op(Op1, Elements, 0, Line, [], StructMap);
replace_init_ops(Any, _) -> [Any].

structinit_to_op(Target,
                 [#varref{line = Line, name = Fname} = Field | Rest],
                 FieldInitMap, FieldTypes, Newcode, StructMap) ->
    Op2 = case maps:find(Fname, FieldInitMap) of
              error ->
                  default_initof(maps:get(Fname, FieldTypes), Line);
              {ok, InitOp} -> InitOp
          end,
    NewAssign = #op2{operator = assign, op2 = Op2,
                     line = Line,
                     op1 =
                         #op2{operator = '.', op1 = Target, op2 = Field,
                              line = Line}},
    Ops = replace_init_ops(NewAssign, StructMap),
    structinit_to_op(Target,
                     Rest,
                     FieldInitMap,
                     FieldTypes,
                     Ops ++ Newcode,
                     StructMap);
structinit_to_op(_, [], _, _, Newcode, _) -> Newcode.

default_initof(#array_type{elemtype = Etype, len = Len},
               Line) ->
    #array_init{elements =
                    lists:duplicate(Len, default_initof(Etype, Line)),
                line = Line};
default_initof(#basic_type{class = struct, tag = Tag,
                           pdepth = 0},
               Line) ->
    #struct_init{name = Tag, line = Line,
                 field_values = #{}, field_names = []};
default_initof(#basic_type{class = integer, pdepth = 0},
               Line) ->
    {integer, Line, 0};
default_initof(#basic_type{class = float, pdepth = 0},
               Line) ->
    {float, Line, 0.0};
default_initof(#basic_type{pdepth = Pdepth}, Line)
    when Pdepth > 0 ->
    {integer, Line, 0}.

arrayinit_to_op(Target, [E | Rest], Cnt, Line, Newcode,
                StructMap) ->
    A = #op1{operator = '@', line = Line, operand = Target},
    B = #op2{operator = '+', op2 = {integer, Line, Cnt},
             line = Line, op1 = A},
    C = #op1{operator = '^', line = Line, operand = B},
    NewAssign = #op2{operator = assign, op1 = C, op2 = E,
                     line = Line},
    Ops = replace_init_ops(NewAssign, StructMap),
    arrayinit_to_op(Target,
                    Rest,
                    Cnt + 1,
                    Line,
                    Ops ++ Newcode,
                    StructMap);
arrayinit_to_op(_, [], _, _, Newcode, _) -> Newcode.
