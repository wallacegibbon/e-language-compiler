-module(ecompiler_collectvar).

-export([fetch_vars/1]).

-import(ecompiler_utils, [flat_format/2]).

-include("./ecompiler_frame.hrl").

fetch_vars(Ast) ->
    {MixedAst, Vars, InitCode} = fetch_vars(Ast, [], #{}, [], true),
    {split_functions_and_structs(MixedAst), Vars, InitCode}.

%% in function expressions, the init code of defvar can not be simply
%% fetched out from the code, it should be replaced as assignment in the
%% same place.
fetch_vars([#vardef{name=Name, type=Type, line=Line, initval=Initval} | Rest],
	   NewAst, Vars, InitCode, CollectInitCode) ->
    case maps:find(Name, Vars) of
	error ->
	    if CollectInitCode ->
		   fetch_vars(Rest, NewAst, Vars#{Name => Type},
			      append_to_ast(InitCode, Name, Initval, Line),
			      CollectInitCode);
	       true ->
		   fetch_vars(Rest, append_to_ast(NewAst, Name, Initval, Line),
			      Vars#{Name => Type}, InitCode, CollectInitCode)
	    end;
	{ok, _} ->
	    throw({Line, flat_format("var name conflict: \"~s\"",
				     [Name])})
    end;
fetch_vars([#function_raw{name=Name, ret=Ret, params=Params, exprs=Exprs} |
	    Rest],
	   NewAst, Vars, InitCode, CollectInitCode) ->
    ParamNames = lists:map(fun (#vardef{name=VarName}) -> VarName end, Params),
    %% collect function parameter variables, variable can have default value
    {[], ParamVars, ParamInitCode} = fetch_vars(Params, [], #{}, [], true),
    %% collect function variables, parameters are variables, too.
    {NewExprs, FunVars, []} = fetch_vars(Exprs, [], ParamVars, [], false),
    Fn = #function{name=Name, ret=Ret, exprs=NewExprs, vars=FunVars,
		     params_defaultinit=ParamInitCode, params=ParamNames},
    fetch_vars(Rest, [Fn | NewAst], Vars, InitCode, CollectInitCode);
fetch_vars([#struct_raw{name=Name, fields=Fields} | Rest],
	   NewAst, Vars, InitCode, CollectInitCode) ->
    %% struct can have default value
    {[], NewFields, StructInitCode} = fetch_vars(Fields, [], #{}, [], true),
    S = #struct{name=Name, fields=NewFields, initcode=StructInitCode},
    fetch_vars(Rest, [S | NewAst], Vars, InitCode, CollectInitCode);
fetch_vars([Any | Rest], NewAst, Vars, InitCode, CollectInitCode) ->
    fetch_vars(Rest, [Any | NewAst], Vars, InitCode, CollectInitCode);
fetch_vars([], NewAst, Vars, InitCode, _) ->
    {lists:reverse(NewAst), Vars, lists:reverse(InitCode)}.


append_to_ast(Ast, Varname, Initval, Line) when Initval =/= none ->
    [#op2{operator=assign, op1=#varref{name=Varname, line=Line},
	  op2=Initval, line=Line} | Ast];
append_to_ast(Ast, _, _, _) ->
    Ast.

split_functions_and_structs(MixedAst) ->
    {Fns, Structs} = lists:partition(fun (A) ->
					     element(1, A) =:= function
				     end, MixedAst),
    FnMap = maps:from_list(lists:map(fun (#function{name=Name} = Fn) ->
					     {Name, Fn}
				     end ,Fns)),
    StructMap = maps:from_list(lists:map(fun (#struct{name=Name} = S) ->
						 {Name, S}
					 end, Structs)),
    {FnMap, StructMap}.

