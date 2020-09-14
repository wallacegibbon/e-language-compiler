-module(ecompiler_utils).

-export([exprsmap/2, expr2str/1, flat_format/2, getvalues_bykeys/2,
	 names_of_varrefs/1, names_of_vardefs/1]).

-export([void_type/1, primitive_size/1]).

-export([fillto_pointerwidth/2, fill_offset/2, cut_extra/2]).

-export([fn_struct_map/1]).

-export([assert/2]).

-include("./ecompiler_frame.hrl").

%% when do simple convertions, this function can be used to avoid boilerplate
%% code for if, while, return, call...,  so you can concentrate on op1, op2...
exprsmap(Fn, [#if_expr{condition=Cond, then=Then, else=Else} = If | Rest]) ->
    [If#if_expr{condition=Fn(Cond), then=exprsmap(Fn, Then),
		else=exprsmap(Fn, Else)} | exprsmap(Fn, Rest)];
exprsmap(Fn, [#while_expr{condition=Cond, exprs=Exprs} = While | Rest]) ->
    [While#while_expr{condition=Fn(Cond), exprs=exprsmap(Fn, Exprs)} |
     exprsmap(Fn, Rest)];
exprsmap(Fn, [#call{fn=Callee, args=Args} = Fncall | Rest]) ->
    [Fncall#call{fn=Fn(Callee), args=exprsmap(Fn, Args)} | exprsmap(Fn, Rest)];
exprsmap(Fn, [#return{expr=Retexpr} = Return | Rest]) ->
    [Return#return{expr=Fn(Retexpr)} | exprsmap(Fn, Rest)];
exprsmap(Fn, [Any | Rest]) ->
    [Fn(Any) | exprsmap(Fn, Rest)];
exprsmap(_, []) ->
    [].

expr2str(#if_expr{condition=Cond, then=Then, else=Else}) ->
    io_lib:format("if (~s) ~s else ~s end", [expr2str(Cond), expr2str(Then),
					     expr2str(Else)]);
expr2str(#while_expr{condition=Cond, exprs=Exprs}) ->
    io_lib:format("while (~s) ~s end", [expr2str(Cond), expr2str(Exprs)]);
expr2str(#call{fn=Callee, args=Args}) ->
    io_lib:format("(~s)(~s)", [expr2str(Callee), expr2str(Args)]);
expr2str(#return{expr=Retexpr}) ->
    io_lib:format("return (~s)", [expr2str(Retexpr)]);
expr2str(#varref{name=Name}) ->
    io_lib:format("~s", [expr2str(Name)]);
expr2str(#op2{operator=Operator, op1=Op1, op2=Op2}) ->
    io_lib:format("~s ~s ~s", [expr2str(Op1), Operator, expr2str(Op2)]);
expr2str(#op1{operator=Operator, operand=Operand}) ->
    io_lib:format("~s ~s", [expr2str(Operand), Operator]);
expr2str({Immi, _, Val}) when Immi =:= integer; Immi =:= float ->
    io_lib:format("~w", [Val]);
expr2str({Immi, _, Val}) when Immi =:= string ->
    Val;
expr2str(Any) ->
    Any.

flat_format(FmtStr, Args) ->
    lists:flatten(io_lib:format(FmtStr, Args)).

-spec getvalues_bykeys([atom()], #{atom() => any()}) -> [any()].
getvalues_bykeys(Fields, Map) when is_map(Map) ->
    getvalues_bykeys(Fields, Map, []).

getvalues_bykeys([Field | Rest], Map, Result) ->
    getvalues_bykeys(Rest, Map, [maps:get(Field, Map) | Result]);
getvalues_bykeys([], _, Result) ->
    lists:reverse(Result).

%% make function and struct map from ast list
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

%% address calculations
fillto_pointerwidth(Num, PointerWidth) ->
    (Num + PointerWidth - 1) div PointerWidth * PointerWidth.

fill_offset(Offset, PointerWidth) ->
    (Offset + PointerWidth) div PointerWidth * PointerWidth.

cut_extra(Offset, PointerWidth) ->
    Offset div PointerWidth * PointerWidth.

primitive_size(usize) -> pwidth;
primitive_size(isize) -> pwidth;
primitive_size(u64) -> 8;
primitive_size(i64) -> 8;
primitive_size(u32) -> 4;
primitive_size(i32) -> 4;
primitive_size(u16) -> 2;
primitive_size(i16) -> 2;
primitive_size(u8) -> 1;
primitive_size(i8) -> 1;
primitive_size(f64) -> 8;
primitive_size(f32) -> 4;
primitive_size(T) ->
    throw(flat_format("size of ~p is not defined", [T])).

void_type(Line) ->
    #basic_type{class=void, tag=void, pdepth=0, line=Line}.

names_of_varrefs(VarRefs) ->
    lists:map(fun(#varref{name=N}) -> N end, VarRefs).

names_of_vardefs(VarDefs) ->
    lists:map(fun(#vardef{name=N}) -> N end, VarDefs).

assert(false, Info) -> throw(Info);
assert(true, _) -> ok.

