-module(ecompiler_collectvar).

-export([fetch_vars/1]).

-import(ecompiler_utils, [flat_format/2]).

-include("./ecompiler_frame.hrl").

fetch_vars(Ast) ->
    fetch_vars(Ast, true).

fetch_vars(Ast, CollectInitCode) ->
    fetch_vars(Ast, [], #{}, [], CollectInitCode).

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
fetch_vars([#function{name=Name, ret=Ret, params=Params, exprs=Exprs} | Rest],
	   NewAst, Vars, InitCode, CollectInitCode) ->
    ParamNames = lists:map(fun (#vardef{name=VarName}) -> VarName end, Params),
    {[], ParamVars, ParamInitCode} = fetch_vars(Params),
    {NewExprs, FunVars, []} = fetch_vars(Exprs, [], ParamVars, [], false),
    Fn = #function_1{name=Name, ret=Ret, exprs=NewExprs, vars=FunVars,
		     params_defaultinit=ParamInitCode, params=ParamNames},
    fetch_vars(Rest, [Fn | NewAst], Vars, InitCode, CollectInitCode);
fetch_vars([#struct{name=Name, fields=Fields} | Rest], NewAst, Vars, InitCode,
	   CollectInitCode) ->
    {[], NewFields, StructInitCode} = fetch_vars(Fields),
    S = #struct_1{name=Name, fields=NewFields, initcode=StructInitCode},
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

