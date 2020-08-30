-module(ecompiler_compile).

-export([compile_from_rawast/2]).

-import(ecompiler_utils, [flat_format/2, fn_struct_map/1]).

-include("./ecompiler_frame.hrl").

compile_from_rawast(Ast, CustomOptions) ->
    Options = maps:merge(default_options(), CustomOptions),
    Ast1 = ecompiler_fillconst:parse_and_remove_const(Ast),

    {Ast2, Vars, InitCode} = ecompiler_collectvar:fetch_vars(Ast1),
    %io:format(">>> ~p~n", [Ast2]),
    {FnMap, StructMap} = fn_struct_map(Ast2),

    %% struct recursion is not allowed.
    check_struct_recursion(StructMap),

    %% calculate struct size, filed offsets
    #{pointer_width := PointerWidth} = Options,
    Ast3 = ecompiler_fillsize:fill_structinfo(Ast2,
					      {StructMap, PointerWidth}),
    %io:format(">>> ~p~n", [Ast3]),

    %% the struct data is updated (struct size contained), so StructMap
    %% needs to be updated, too
    {_, StructMap1} = fn_struct_map(Ast3),

    %% type checking
    ecompiler_type:checktype_ast(Ast3, Vars, {FnMap, StructMap1}),

    %% expand init exprs like A{a=1} and {1,2,3}
    Ast4 = ecompiler_expandinit:expand_initexpr_infun(Ast3, StructMap1),

    %% sizeof expression may also contained in struct init code,
    %% dod sizeof expanding after struct_init expanding is more convenient
    Ast5 = ecompiler_fillsize:expand_size(Ast4, {StructMap1, PointerWidth}),

    {Ast5, Vars, InitCode}.

default_options() ->
    #{pointer_width => 8}.

check_struct_recursion(StructMap) ->
    lists:map(fun(S) -> check_struct_rec(S, StructMap, #{}) end,
	      maps:values(StructMap)).

check_struct_rec(#struct{name=Name, field_types=FieldTypes, line=Line},
		 StructMap, UsedMap) ->
    try
	check_struct_rec_1(maps:to_list(FieldTypes), StructMap,
			   UsedMap#{Name => true})
    catch
	throw:recur ->
	    throw({Line, flat_format("some field in ~s is recursive",
				     [Name])})
    end;
check_struct_rec(_, _, _) ->
    ok.

check_struct_rec_1([{_, FieldType} | Rest], StructMap, UsedMap) ->
    case contain_struct(FieldType) of
	{yes, N} ->
	    case maps:find(N, UsedMap) of
		error ->
		    check_struct_rec(maps:get(N, StructMap), StructMap,
				     UsedMap#{N => true}),
		    check_struct_rec_1(Rest, StructMap, UsedMap);
		{ok, _} ->
		    throw(recur)
	    end;
	no ->
	    check_struct_rec_1(Rest, StructMap, UsedMap)
    end;
check_struct_rec_1([], _, _) ->
    ok.

contain_struct(#basic_type{class=struct, pdepth=0, tag=Name}) ->
    {yes, Name};
contain_struct(#array_type{elemtype=BaseT}) ->
    contain_struct(BaseT);
contain_struct(_) ->
    no.

