%%% this is the 2nd pass, variable map will be created after this pass.

-module(ecompiler_collectvar).

-export([fetch_vars/1]).

-import(ecompiler_utils, [flat_format/2, getvalues_bykeys/2, exprsmap/2]).

-include("./ecompiler_frame.hrl").

fetch_vars(Ast) ->
    Ast2 = prepare_structinit_expr(Ast),
    {MixedAst, VarTypes, InitCode} = fetch_vars(Ast2, [], {#{}, [], true}),
    Ast3 = split_functions_and_structs(MixedAst),
    {Ast3, VarTypes, InitCode}.

%% struct_init's fields were assign expressions, convert it to a map
prepare_structinit_expr([#function_raw{exprs=Exprs} = F | Rest]) ->
    [F#function_raw{exprs=exprsmap(fun fix_structinit/1, Exprs)} |
     prepare_structinit_expr(Rest)];
prepare_structinit_expr([#struct_raw{fields=Exprs} = S | Rest]) ->
    [S#struct_raw{fields=exprsmap(fun fix_structinit/1, Exprs)} |
     prepare_structinit_expr(Rest)];
prepare_structinit_expr([#vardef{initval=Initval} = V | Rest]) ->
    [V#vardef{initval=hd(exprsmap(fun fix_structinit/1, [Initval]))} |
     prepare_structinit_expr(Rest)];
prepare_structinit_expr([]) ->
    [].

fix_structinit(#struct_init_raw{name=Name, fields=Fields, line=Line}) ->
    #struct_init{name=Name, fields=structinit_tomap(Fields), line=Line};
fix_structinit(#array_init{elements=Elements} = A) ->
    A#array_init{elements=exprsmap(fun fix_structinit/1, Elements)};
fix_structinit(#vardef{initval=Initval} = V) ->
    V#vardef{initval=hd(exprsmap(fun fix_structinit/1, [Initval]))};
fix_structinit(Any) ->
    Any.

structinit_tomap(Exprs) ->
    structinit_tomap(Exprs, #{}).

structinit_tomap([#op2{operator=assign, op1=#varref{name=Field}, op2=Val} |
		  Rest], Result) ->
    structinit_tomap(Rest, Result#{Field => hd(exprsmap(fun fix_structinit/1,
							[Val]))});
structinit_tomap([], Result) ->
    Result.

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
    ParamNames = names_of_vardefs(Params),
    %% collect function parameter variables, variable can have default value
    {[], ParamVars, ParamInitCode} = fetch_vars(Params, [],
						{#{}, [], true}),
    if ParamInitCode =/= [] ->
	   throw({Line, "function parameters can not have default value"});
       true ->
	   ok
    end,
    %% collect function variables, parameters are variables, too.
    {NewExprs, FunVarTypes, []} = fetch_vars(Exprs, [],
					     {ParamVars, [], false}),
    Fn = #function{name=Name, var_types=FunVarTypes, exprs=NewExprs,
		   param_names=ParamNames,
		   type=#fun_type{params=getvalues_bykeys(ParamNames,
							  ParamVars),
				  ret=Ret, line=Line}},
    fetch_vars(Rest, [Fn | NewAst], Ctx);
fetch_vars([#struct_raw{name=Name, fields=Fields} | Rest], NewAst, Ctx) ->
    FieldNames = names_of_vardefs(Fields),
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = fetch_vars(Fields, [],
						 {#{}, [], true}),
    S = #struct{name=Name, field_types=FieldTypes, field_names=FieldNames,
		field_defaults=structinit_tomap(StructInitCode)},
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

%% the list version ast is also return for generating C code.
%% (in C language, the order of definition matters)
split_functions_and_structs(MixedAst) ->
    {Fns, Structs} = lists:partition(fun(A) ->
					     element(1, A) =:= function
				     end, MixedAst),
    FnMap = maps:from_list(lists:map(fun(#function{name=Name} = Fn) ->
					     {Name, Fn}
				     end, Fns)),
    StructMap = maps:from_list(lists:map(fun(#struct{name=Name} = S) ->
						 {Name, S}
					 end, Structs)),
    {{FnMap, StructMap}, {Fns, Structs}}.

names_of_vardefs(Vardefs) ->
    lists:map(fun(#vardef{name=N}) -> N end, Vardefs).

