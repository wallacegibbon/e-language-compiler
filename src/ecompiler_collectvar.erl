%%% this is the 2nd pass, variable map will be created after this pass.
-module(ecompiler_collectvar).

-export([fetch_vars/1]).

-include("./ecompiler_frame.hrl").

fetch_vars(Ast) ->
    Ast2 = prepare_structinit_expr(Ast),
    {Ast3, VarTypes, InitCode} = fetch_vars(Ast2, [], {#{}, [], true}),
    {Ast3, VarTypes, InitCode}.

%% struct_init's fields were assign expressions, convert it to a map
prepare_structinit_expr([#function_raw{exprs = Exprs} = F | Rest]) ->
    [F#function_raw{exprs = fix_structinit_ast(Exprs)}
     | prepare_structinit_expr(Rest)];
prepare_structinit_expr([#struct_raw{fields = Exprs} = S | Rest]) ->
    [S#struct_raw{fields = fix_structinit_ast(Exprs)}
     | prepare_structinit_expr(Rest)];
prepare_structinit_expr([#vardef{initval = Initval} = V | Rest]) ->
    [V#vardef{initval = fix_structinit(Initval)}
     | prepare_structinit_expr(Rest)];
prepare_structinit_expr([]) ->
    [].

fix_structinit_ast(Lst) ->
    ecompiler_util:exprsmap(fun fix_structinit/1, Lst).

fix_structinit(#struct_init_raw{name = Name,
                                fields = Fields, line = Line}) ->
    {FieldNames, InitExprMap} = structinit_tomap(Fields),
    #struct_init{name = Name, field_names = FieldNames,
                 field_values = InitExprMap, line = Line};
fix_structinit(#array_init{elements = Elements} = A) ->
    A#array_init{elements = fix_structinit_ast(Elements)};
fix_structinit(#vardef{initval = Initval} = V) ->
    V#vardef{initval = fix_structinit(Initval)};
fix_structinit(#op2{op1 = Op1, op2 = Op2} = O) ->
    O#op2{op1 = fix_structinit(Op1),
          op2 = fix_structinit(Op2)};
fix_structinit(#op1{operand = Operand} = O) ->
    O#op1{operand = fix_structinit(Operand)};
fix_structinit(Any) ->
    Any.

structinit_tomap(Exprs) ->
    structinit_tomap(Exprs, [], #{}).

-define(ASSIGN_OP(Op1, Op2),
        #op2{operator = assign, op1 = Op1, op2 = Op2}).

structinit_tomap([?ASSIGN_OP((#varref{name = Field} = Op1), Val) | Rest],
                 FieldNames, ExprMap) ->
    structinit_tomap(Rest, [Op1 | FieldNames],
                     ExprMap#{Field => fix_structinit(Val)});
structinit_tomap([], FieldNames, ExprMap) ->
    {FieldNames, ExprMap}.

%% in function expressions, the init code of defvar can not be simply
%% fetched out from the code, it should be replaced as assignment in the
%% same place.
fetch_vars([#vardef{name = Name, type = Type,
                    line = Line, initval = Initval} | Rest],
           NewAst, {VarTypes, InitCode, CollectInitCode}) ->
    ensure_no_conflict(Name, VarTypes, Line),
    case CollectInitCode of
        true ->
            fetch_vars(Rest, NewAst,
                       {VarTypes#{Name => Type},
                        append_to_ast(InitCode, Name, Initval, Line),
                        CollectInitCode});
        _ ->
            fetch_vars(Rest,
                       append_to_ast(NewAst, Name, Initval, Line),
                       {VarTypes#{Name => Type}, InitCode, CollectInitCode})
    end;
fetch_vars([#function_raw{name = Name, ret = Ret,
                          params = Params, exprs = Exprs, line = Line} | Rest],
           NewAst, {GlobalVars, _, _} = Ctx) ->
    {[], ParamVars, ParamInitCode} = fetch_vars(Params,
                                                [],
                                                {#{}, [], true}),
    ecompiler_util:assert(ParamInitCode =:= [],
                          {Line,
                           "function parameters can not have default "
                           "value"}),
    {NewExprs, FunVarTypes, []} = fetch_vars(Exprs,
                                             [],
                                             {ParamVars, [], false}),
    %% local variables should have different names from global variables
    check_varconflict(GlobalVars, FunVarTypes),
    %% lable names should be different from variables,
    %% because the operand of goto could be a pointer variable.
    Labels = lists:filter(fun (E) -> element(1, E) =:= label
                          end,
                          Exprs),
    check_labelconflict(Labels, GlobalVars, FunVarTypes),
    ParamsForType = getvalues_bydefs(Params, ParamVars),
    Fn = #function{name = Name, var_types = FunVarTypes,
                   exprs = NewExprs,
                   param_names = varrefs_from_vardefs(Params), line = Line,
                   type =
                       #fun_type{params = ParamsForType, ret = Ret,
                                 line = Line}},
    fetch_vars(Rest, [Fn | NewAst], Ctx);
fetch_vars([#struct_raw{name = Name, fields = Fields,
                        line = Line} | Rest], NewAst, Ctx) ->
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = fetch_vars(Fields,
                                                  [],
                                                  {#{}, [], true}),
    {_, FieldInitMap} = structinit_tomap(StructInitCode),
    FieldNames = varrefs_from_vardefs(Fields),
    S = #struct{name = Name, field_types = FieldTypes,
                field_names = FieldNames, field_defaults = FieldInitMap,
                line = Line},
    fetch_vars(Rest, [S | NewAst], Ctx);
fetch_vars([Any | Rest], NewAst, Ctx) ->
    fetch_vars(Rest, [Any | NewAst], Ctx);
fetch_vars([], NewAst, {VarTypes, InitCode, _}) ->
    {lists:reverse(NewAst), VarTypes, lists:reverse(InitCode)}.

append_to_ast(Ast, Varname, Initval, Line)
    when Initval =/= none ->
    [#op2{operator = assign, op1 = #varref{name = Varname, line = Line},
          op2 = Initval, line = Line} | Ast];
append_to_ast(Ast, _, _, _) ->
    Ast.

check_labelconflict([#label{name = Name, line = Line} | Rest],
                    GlobalVars, LocalVars) ->
    ensure_no_conflict(Name, LocalVars, Line),
    ensure_no_conflict(Name, GlobalVars, Line),
    check_labelconflict(Rest, GlobalVars, LocalVars);
check_labelconflict([], _, _) ->
    ok.

check_varconflict(GlobalVars, LocalVars) ->
    ConflictMap = maps:with(maps:keys(GlobalVars), LocalVars),
    maps:map(fun (Name, T) ->
                     throw_name_conflict(Name, element(2, T))
             end,
             ConflictMap).

ensure_no_conflict(Name, VarMap, Line) ->
    case maps:find(Name, VarMap) of
        {ok, _} ->
            throw_name_conflict(Name, Line);
        _ ->
            ok
    end.

throw_name_conflict(Name, Line) ->
    throw({Line,
           ecompiler_util:flat_format("name ~s has already been used",
                                      [Name])}).

getvalues_bydefs(DefList, Map) ->
    ecompiler_util:getvalues_bykeys(ecompiler_util:names_of_vardefs(DefList),
                                    Map).

varrefs_from_vardefs(Vardefs) ->
    lists:map(fun (#vardef{name = N, line = Line}) ->
                      #varref{name = N, line = Line}
              end,
              Vardefs).
