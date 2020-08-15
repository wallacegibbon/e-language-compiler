-module(ecompiler_collectvar).

-export([fetch_vars/1]).

-import(ecompiler_utils, [flat_format/2]).

-include("./ecompiler_frame.hrl").

fetch_vars(Ast) -> fetch_vars(Ast, [], #{}, []).

fetch_vars([#vardef{name=Name, type=Type, line=Line, initval=Initval} | Rest],
	   NewAst, Vars, InitCode) ->
    case maps:find(Name, Vars) of
	{ok, _} ->
	    throw({Line, flat_format("name <~s> conflict", [Name])});
	error ->
	    fetch_vars(Rest, NewAst, Vars#{Name => Type},
		       update_initcode(InitCode, Name, Initval, Line))
    end;
fetch_vars([#function{name=Name, ret=Rettype, params=Params, exprs=Exprs}
	    | Rest],
	   NewAst, Vars, InitCode) ->
    %check params and variable name conflict
    {NewExprs, FunVars, FunInitCode} = fetch_vars(Exprs),
    {[], ParamVars, ParamInitCode} = fetch_vars(Params),
    Fn = #function_1{name=Name, ret=Rettype, exprs=FunInitCode ++ NewExprs,
		     vars=maps:merge(ParamVars, FunVars),
		     params_defaultinit=ParamInitCode,
		     params=lists:map(fun (#vardef{name=VarName}) ->
					      VarName
				      end, Params)},
    fetch_vars(Rest, [Fn | NewAst], Vars, InitCode);
fetch_vars([#struct{name=Name, fields=Fields} | Rest],
	   NewAst, Vars, InitCode) ->
    {[], NewFields, StructInitCode} = fetch_vars(Fields),
    S = #struct_1{name=Name, fields=NewFields, initcode=StructInitCode},
    fetch_vars(Rest, [S | NewAst], Vars, InitCode);
fetch_vars([Any | Rest], NewAst, Vars, InitCode) ->
    fetch_vars(Rest, [Any | NewAst], Vars, InitCode);
fetch_vars([], NewAst, Vars, InitCode) ->
    {lists:reverse(NewAst), Vars, lists:reverse(InitCode)}.

update_initcode(InitCode, Varname, Initval, Line) when Initval =/= none ->
    [#op2{operator=assign, op1=#varref{name=Varname, line=Line},
	  op2=Initval, line=Line} | InitCode];
update_initcode(InitCode, _, _, _) ->
    InitCode.

