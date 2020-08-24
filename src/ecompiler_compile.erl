-module(ecompiler_compile).

-export([compile_from_rawast/2, fn_struct_map/1]).

-include("./ecompiler_frame.hrl").

compile_from_rawast(Ast, CustomOptions) ->
    _Options = maps:merge(default_options(), CustomOptions),
    Ast1 = ecompiler_fillconst:parse_and_remove_const(Ast),
    {Ast2, Vars, InitCode} = ecompiler_collectvar:fetch_vars(Ast1),
    {FnMap, StructMap} = fn_struct_map(Ast2),
    ecompiler_type:checktype_ast(Ast2, Vars, {FnMap, StructMap}),
    Ast3 = ecompiler_expandinit:expand_initexpr_infun(Ast2, StructMap),
    {Ast3, Vars, InitCode}.

default_options() ->
    #{pointer_width => 8}.

fn_struct_map(Ast) ->
    {Fns, Structs} = lists:partition(fun(A) ->
					     element(1, A) =:= function
				     end, Ast),
    %% FnMap stores function type only
    FnMap = maps:from_list(lists:map(fun(#function{name=Name} = Fn) ->
					     {Name, Fn#function.type}
				     end, Fns)),
    StructMap = maps:from_list(lists:map(fun(#struct{name=Name} = S) ->
						 {Name, S}
					 end, Structs)),
    {FnMap, StructMap}.

