-module(ecompiler_fillstruct).

-export([fill_structinfo/2]).

-import(ecompiler_utils, [names_of_varrefs/1, getvalues_bykeys/2]).

-include("./ecompiler_frame.hrl").

fill_structinfo([#struct{field_names=FieldNames, field_types=FieldTypes} = S |
		 Rest], Ctx) ->
    {Size, FieldOffsets} = calc_fieldoffsets(getvalues_byrefs(FieldNames,
							      FieldTypes),
					     Ctx),
    [S#struct{field_offsets=FieldOffsets, size=Size} |
     fill_structinfo(Rest, Ctx)];
fill_structinfo([Any | Rest], Ctx) ->
    [Any | fill_structinfo(Rest, Ctx)];
fill_structinfo([], _) ->
    [].

calc_fieldoffsets(Fields, Ctx) ->
    calc_fieldoffsets(Fields, #{}, 0, Ctx).

calc_fieldoffsets(Fields, OffsetMap, CurrentOffset, Ctx) ->
    {0, #{}}.

getvalues_byrefs(RefList, Map) ->
    getvalues_bykeys(names_of_varrefs(RefList), Map).

