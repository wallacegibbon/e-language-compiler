-module(ecompiler_compile).

-export([compile_from_rawast/2]).

-import(ecompiler_util,
	[flat_format/2, fn_struct_map/1, value_inlist/2]).

-include("./ecompiler_frame.hrl").

compile_from_rawast(Ast, CustomOptions) ->
    Options = maps:merge(default_options(), CustomOptions),
    Ast1 = ecompiler_fillconst:parse_and_remove_const(Ast),
    %io:format(">>> ~p~n", [Ast1]),
    {Ast2, Vars, InitCode0} =
	ecompiler_collectvar:fetch_vars(Ast1),
    %io:format(">>> ~p~n", [Ast2]),
    {FnMap, StructMap0} = fn_struct_map(Ast2),
    %% struct recursion is not allowed.
    check_struct_recursion(StructMap0),
    #{pointer_width := PointerWidth} = Options,
    Ctx0 = {StructMap0, PointerWidth},
    %% calculate struct size, filed offsets
    Ast3 = ecompiler_fillsize:fill_structinfo(Ast2, Ctx0),
    %% struct size is updated, so StructMap needs to be updated, too
    {_, StructMap1} = fn_struct_map(Ast3),
    %% expand sizeof expression
    Ctx1 = {StructMap1, PointerWidth},
    Ast4 = ecompiler_fillsize:expand_sizeof(Ast3, Ctx1),
    %% initcode is not in main ast, do not forget it
    InitCode1 =
	ecompiler_fillsize:expand_sizeof_inexprs(InitCode0,
						 Ctx1),
    %% sizeof expressions are expanded, so StructMap needs to be updated
    {_, StructMap2} = fn_struct_map(Ast4),
    %% type checking
    Maps = {FnMap, StructMap2},
    ecompiler_type:checktype_ast(Ast4, Vars, Maps),
    ecompiler_type:checktype_exprs(InitCode1, Vars, Maps),
    %% expand init exprs like A{a=1} and {1,2,3}
    Ast5 = ecompiler_expandinit:expand_initexpr_infun(Ast4,
						      StructMap2),
    InitCode2 =
	ecompiler_expandinit:expand_initexprs(InitCode1,
					      StructMap2),
    {Ast5, Vars, InitCode2, FnMap}.

default_options() -> #{pointer_width => 8}.

check_struct_recursion(StructMap) ->
    lists:map(fun (S) -> check_struct_rec(S, StructMap, [])
	      end,
	      maps:values(StructMap)).

check_struct_rec(#struct{name = Name,
			 field_types = FieldTypes, line = Line},
		 StructMap, UsedStructs) ->
    try check_field_rec1(maps:to_list(FieldTypes),
			 StructMap, [Name | UsedStructs])
    catch
      {recur, Chain} ->
	  throw({Line,
		 flat_format("recursive struct ~s -> ~w",
			     [Name, Chain])})
    end;
check_struct_rec(_, _, _) -> ok.

check_field_rec1([{_, FieldType} | Rest], StructMap,
		 UsedStructs) ->
    case contain_struct(FieldType) of
      {yes, StructName} ->
	  check_field_rec2(StructName, StructMap, UsedStructs);
      no -> ok
    end,
    check_field_rec1(Rest, StructMap, UsedStructs);
check_field_rec1([], _, _) -> ok.

check_field_rec2(Name, StructMap, UsedStructs) ->
    case value_inlist(Name, UsedStructs) of
      false ->
	  check_struct_rec(maps:get(Name, StructMap), StructMap,
			   UsedStructs);
      true -> throw({recur, lists:reverse(UsedStructs)})
    end.

contain_struct(#basic_type{class = struct, pdepth = 0,
			   tag = Name}) ->
    {yes, Name};
contain_struct(#array_type{elemtype = BaseT}) ->
    contain_struct(BaseT);
contain_struct(_) -> no.
