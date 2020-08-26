-module(ecompiler_compile).

-export([compile_from_rawast/2, fn_struct_map/1]).

-import(ecompiler_utils, [is_primitive_type/1, flat_format/2]).

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
    Ast3 = ecompiler_fillstruct:fill_structinfo(Ast2,
						{StructMap, PointerWidth}),

    %% type checking
    ecompiler_type:checktype_ast(Ast3, Vars, {FnMap, StructMap}),

    %% expand init exprs like A{a=1} and {1,2,3}
    Ast4 = ecompiler_expandinit:expand_initexpr_infun(Ast3, StructMap),
    {Ast4, Vars, InitCode}.

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
    case is_struct(FieldType) of
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

is_struct(#basic_type{type={Name, 0}}) ->
    case is_primitive_type(Name) of
	false ->
	    {yes, Name};
	true ->
	    no
    end;
is_struct(#array_type{elemtype=BaseT}) ->
    is_struct(BaseT);
is_struct(_) ->
    no.

