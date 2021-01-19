-module(ecompiler_fillsize).

-export([fill_structinfo/2,expand_sizeof/2,expand_sizeof_inexprs/2]).

-import(ecompiler_utils, [names_of_varrefs/1,getvalues_bykeys/2,exprsmap/2,
			  primitive_size/1,flat_format/2,fn_struct_map/1,
			  fillto_pointerwidth/2,fill_offset/2,cut_extra/2]).

-include("./ecompiler_frame.hrl").

expand_sizeof([#function{exprs=Exprs}=F|Rest], Ctx) ->
    [F#function{exprs=expand_sizeof_inexprs(Exprs, Ctx)}|
     expand_sizeof(Rest, Ctx)];
expand_sizeof([#struct{field_defaults=FieldDefaults}=S|Rest], Ctx) ->
    [S#struct{field_defaults=expand_sizeof_inmap(FieldDefaults, Ctx)}|
     expand_sizeof(Rest, Ctx)];
expand_sizeof([], _) ->
    [].

expand_sizeof_inmap(Map, Ctx) ->
    maps:map(fun(_, V1) -> expand_sizeof_inexpr(V1, Ctx) end, Map).

expand_sizeof_inexprs(Exprs, Ctx) ->
    exprsmap(fun(E) -> expand_sizeof_inexpr(E, Ctx) end, Exprs).

expand_sizeof_inexpr(#sizeof{type=T,line=Line}, Ctx) ->
    try
	{integer,Line,sizeof(T, Ctx)}
    catch
	throw:I ->
	    throw({Line,I})
    end;
expand_sizeof_inexpr(#op2{op1=Op1,op2=Op2}=O, Ctx) ->
    O#op2{op1=expand_sizeof_inexpr(Op1, Ctx),
	  op2=expand_sizeof_inexpr(Op2, Ctx)};
expand_sizeof_inexpr(#op1{operand=Operand}=O, Ctx) ->
    O#op1{operand=expand_sizeof_inexpr(Operand, Ctx)};
expand_sizeof_inexpr(#struct_init{field_values=ExprMap}=Si, Ctx) ->
    Si#struct_init{field_values=expand_sizeof_inmap(ExprMap, Ctx)};
expand_sizeof_inexpr(#array_init{elements=Elements}=Ai, Ctx) ->
    Ai#array_init{elements=expand_sizeof_inexprs(Elements, Ctx)};
expand_sizeof_inexpr(Any, _) ->
    Any.

%% calculate struct size and collect field offsets.
%%
%% In the current algorithm, the size of the same struct will be calculated
%% for multiple times, which is not necessary. But the code is beautiful, so
%% I will just keep it as it is now.
%% use a process to hold the calculated struct info when the speed really
%% becomes a problem.
fill_structinfo(Ast, {_,PointerWidth}=Ctx) ->
    Ast1 = lists:map(fun(E) -> fill_structsize(E, Ctx) end, Ast),
    {_,StructMap1} = fn_struct_map(Ast1),
    Ctx1 = {StructMap1,PointerWidth},
    Ast2 = lists:map(fun(E) -> fill_structoffsets(E, Ctx1) end, Ast1),
    Ast2.

fill_structsize(#struct{}=S, Ctx) ->
    S#struct{size=sizeof_struct(S, Ctx)};
fill_structsize(Any, _) ->
    Any.

fill_structoffsets(#struct{}=S, Ctx) ->
    S#struct{field_offsets=offsetsof_struct(S, Ctx)};
fill_structoffsets(Any, _) ->
    Any.

offsetsof_struct(#struct{field_names=FieldNames,field_types=FieldTypes},
		 Ctx) ->
    FieldTypeList = getkvs_byrefs(FieldNames, FieldTypes),
    {_,OffsetMap} = sizeof_fields(FieldTypeList, 0, #{}, Ctx),
    OffsetMap.

sizeof_struct(#struct{size=Size}, _) when is_integer(Size) ->
    Size;
sizeof_struct(#struct{field_names=FieldNames,field_types=FieldTypes}, Ctx) ->
    FieldTypeList = getkvs_byrefs(FieldNames, FieldTypes),
    {Size,_} = sizeof_fields(FieldTypeList, 0, #{}, Ctx),
    Size.

getkvs_byrefs(RefList, Map) ->
    Keys = names_of_varrefs(RefList),
    Values = getvalues_bykeys(Keys, Map),
    lists:zip(Keys, Values).

%% this is the function that calculate size and offsets
sizeof_fields([{Fname,Ftype}|Rest], CurrentOffset, OffsetMap,
	      {_,PointerWidth}=Ctx) ->
    FieldSize = sizeof(Ftype, Ctx),
    NextOffset = CurrentOffset + FieldSize,
    if (CurrentOffset rem PointerWidth) =/= 0 ->
	   OffsetFixed = fix_offset(CurrentOffset, NextOffset, PointerWidth),
	   sizeof_fields(Rest, OffsetFixed + FieldSize,
			 OffsetMap#{Fname=>OffsetFixed}, Ctx);
       true ->
	   sizeof_fields(Rest, NextOffset,
			 OffsetMap#{Fname=>CurrentOffset}, Ctx)
    end;
sizeof_fields([], CurrentOffset, OffsetMap, _) ->
    {CurrentOffset,OffsetMap}.

fix_offset(CurrentOffset, NextOffset, PointerWidth) ->
    case (cut_extra(NextOffset, PointerWidth) >
	  cut_extra(CurrentOffset, PointerWidth)) of
	true ->
	    fill_offset(CurrentOffset, PointerWidth);
	_ ->
	    CurrentOffset
    end.

%%
sizeof(#array_type{elemtype=T,len=Len}, {_,PointerWidth}=Ctx) ->
    ElemSize = sizeof(T, Ctx),
    if (ElemSize < PointerWidth) ->
	   if (PointerWidth rem ElemSize =:= 0) ->
		  ElemSize * Len;
	      true ->
		  PointerWidth * Len
	   end;
       true ->
	   fillto_pointerwidth(ElemSize, PointerWidth) * Len
    end;
sizeof(#basic_type{pdepth=N}, {_,PointerWidth}) when N > 0 ->
    PointerWidth;
sizeof(#basic_type{class=struct,tag=Tag}, {StructMap,_}=Ctx) ->
    case maps:find(Tag, StructMap) of
	{ok,S} ->
	    sizeof_struct(S, Ctx);
	error ->
	    throw(flat_format("~s is not found", [Tag]))
    end;
sizeof(#basic_type{class=C,tag=Tag}, {_,PointerWidth}) when C =:= integer;
							    C =:= float ->
    case primitive_size(Tag) of
	pwidth ->
	    PointerWidth;
	V when is_integer(V) ->
	    V;
	_ ->
	    throw(flat_format("primitive_size(~s) is invalid", [Tag]))
    end;
sizeof(#fun_type{}, {_,PointerWidth}) ->
    PointerWidth;
sizeof(A, _) ->
    throw(flat_format("invalid type ~p on sizeof", [A])).

