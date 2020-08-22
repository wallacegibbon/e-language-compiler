%%% this is the 2nd pass, variable map will be created after this pass.

-module(ecompiler_collectvar).

-export([fetch_vars/1]).

-import(ecompiler_utils, [flat_format/2, getvalues_bykeys/2, exprsmap/2]).

-include("./ecompiler_frame.hrl").

fetch_vars(Ast) ->
    Ast2 = prepare_structinit_expr(Ast),
    {Ast3, VarTypes, InitCode} = fetch_vars(Ast2, [], {#{}, [], true}),
    {Ast3, VarTypes, InitCode}.

%% struct_init's fields were assign expressions, convert it to a map
prepare_structinit_expr([#function_raw{exprs=Exprs} = F | Rest]) ->
    [F#function_raw{exprs=fix_structinit_ast(Exprs)} |
     prepare_structinit_expr(Rest)];
prepare_structinit_expr([#struct_raw{fields=Exprs} = S | Rest]) ->
    [S#struct_raw{fields=fix_structinit_ast(Exprs)} |
     prepare_structinit_expr(Rest)];
prepare_structinit_expr([#vardef{initval=Initval} = V | Rest]) ->
    [V#vardef{initval=fix_structinit(Initval)} |
     prepare_structinit_expr(Rest)];
prepare_structinit_expr([]) ->
    [].

fix_structinit_ast(Lst) ->
    exprsmap(fun fix_structinit/1, Lst).

fix_structinit(#struct_init_raw{name=Name, fields=Fields, line=Line}) ->
    {FieldNames, InitExprMap} = structinit_tomap(Fields),
    #struct_init{name=Name, field_names=FieldNames, field_values=InitExprMap,
		 line=Line};
fix_structinit(#array_init{elements=Elements} = A) ->
    A#array_init{elements=fix_structinit_ast(Elements)};
fix_structinit(#vardef{initval=Initval} = V) ->
    V#vardef{initval=fix_structinit(Initval)};
fix_structinit(#op2{op1=Op1, op2=Op2} = O) ->
    O#op2{op1=fix_structinit(Op1), op2=fix_structinit(Op2)};
fix_structinit(#op1{operand=Operand} = O) ->
    O#op1{operand=fix_structinit(Operand)};
fix_structinit(Any) ->
    Any.

structinit_tomap(Exprs) ->
    structinit_tomap(Exprs, [], #{}).

structinit_tomap([#op2{operator=assign, op1=#varref{name=Field} = Op1,
		       op2=Val} | Rest], FieldNames, ExprMap) ->
    structinit_tomap(Rest, [Op1 | FieldNames],
		     ExprMap#{Field => fix_structinit(Val)});
structinit_tomap([], FieldNames, ExprMap) ->
    {FieldNames, ExprMap}.

%% in function expressions, the init code of defvar can not be simply
%% fetched out from the code, it should be replaced as assignment in the
%% same place.
fetch_vars([#vardef{name=Name, type=Type, line=Line, initval=Initval} | Rest],
	   NewAst, {VarTypes, InitCode, CollectInitCode}) ->
    case maps:find(Name, VarTypes) of
	error ->
	    if CollectInitCode ->
		   NewCtx = {VarTypes#{Name => Type},
			     append_to_ast(InitCode, Name, Initval, Line),
			     CollectInitCode},
		   fetch_vars(Rest, NewAst, NewCtx);
	       true ->
		   NewCtx = {VarTypes#{Name => Type}, InitCode,
			     CollectInitCode},
		   fetch_vars(Rest, append_to_ast(NewAst, Name, Initval, Line),
			      NewCtx)
	    end;
	{ok, _} ->
	    throw({Line, flat_format("var name conflict: \"~s\"",
				     [Name])})
    end;
fetch_vars([#function_raw{name=Name, ret=Ret, params=Params, exprs=Exprs,
			  line=Line} | Rest], NewAst, Ctx) ->
    {[], ParamVars, ParamInitCode} = fetch_vars(Params, [],
						{#{}, [], true}),
    if ParamInitCode =/= [] ->
	   throw({Line, "function parameters can not have default value"});
       true ->
	   ok
    end,
    {NewExprs, FunVarTypes, []} = fetch_vars(Exprs, [],
					     {ParamVars, [], false}),
    ParamsForType = getvalues_bykeys(names_of_vardefs(Params), ParamVars),
    Fn = #function{name=Name, var_types=FunVarTypes, exprs=NewExprs,
		   param_names=varrefs_from_vardefs(Params),
		   type=#fun_type{params=ParamsForType, ret=Ret, line=Line}},
    fetch_vars(Rest, [Fn | NewAst], Ctx);
fetch_vars([#struct_raw{name=Name, fields=Fields} | Rest], NewAst, Ctx) ->
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = fetch_vars(Fields, [],
						 {#{}, [], true}),
    {_, FieldInitMap} = structinit_tomap(StructInitCode),
    FieldNames = varrefs_from_vardefs(Fields),
    S = #struct{name=Name, field_types=FieldTypes, field_names=FieldNames,
		field_defaults=FieldInitMap},
    fetch_vars(Rest, [S | NewAst], Ctx);
fetch_vars([Any | Rest], NewAst, Ctx) ->
    fetch_vars(Rest, [Any | NewAst], Ctx);
fetch_vars([], NewAst, {VarTypes, InitCode, _}) ->
    {lists:reverse(NewAst), VarTypes, lists:reverse(InitCode)}.

append_to_ast(Ast, Varname, Initval, Line) when Initval =/= none ->
    [#op2{operator=assign, op1=#varref{name=Varname, line=Line},
	  op2=Initval, line=Line} | Ast];
append_to_ast(Ast, _, _, _) ->
    Ast.

names_of_vardefs(Vardefs) ->
    lists:map(fun(#vardef{name=N}) -> N end, Vardefs).

varrefs_from_vardefs(Vardefs) ->
    lists:map(fun(#vardef{name=N, line=Line}) ->
		      #varref{name=N, line=Line}
	      end, Vardefs).

