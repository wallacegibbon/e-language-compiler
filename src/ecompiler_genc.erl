-module(ecompiler_genc).

-export([generate_ccode/2]).

-include("./ecompiler_frame.hrl").


generate_ccode(Ast, OutputFile) ->
    Statements = statements_tostr(Ast),
    file:write_file(OutputFile, [common_code() | Statements]).


statements_tostr([#function{name=Name, params=Params,
			    ret=Rettype, exprs=Exprs} | Rest]) ->
    [io_lib:format("~s~n~s(~s)~n{~n~s~n}~n~n",
		   [type_tostr(Rettype), Name,
		    defvar_tostr(Params, {",", false}), exprs_tostr(Exprs)])
     | statements_tostr(Rest)];
statements_tostr([#struct{name=Name, fields=Fields} | Rest]) ->
    [io_lib:format("typedef struct {~n~s~n} ~s;~n~n",
		   [defvar_tostr(Fields), Name])
     | statements_tostr(Rest)];
statements_tostr([#const{name=Name, val=Expr} | Rest]) ->
    [io_lib:format("#define ~s (~s)~n~n",
		   [Name, expr_tostr(Expr)]) | statements_tostr(Rest)];
statements_tostr([]) ->
    [].


defvar_tostr(Vars, {SplitChar, true}) ->
    [defvar_tostr(Vars, SplitChar, []), SplitChar];
defvar_tostr(Vars, {SplitChar, false}) ->
    defvar_tostr(Vars, SplitChar, []).

defvar_tostr(Vars) ->
    [defvar_tostr(Vars, ";\n", []), ";\n"].

-define(VARDEF(Name, Type, Initval),
	#vardef{name=Name, type=Type, initval=Initval}).

defvar_tostr([?VARDEF(Name, Type, Initval) | Rest], SplitChar, Defs)
  when Initval =/= none ->
    S = io_lib:format("~s ~s = ~s", [type_tostr(Type), Name,
				     expr_tostr(Initval)]),
    defvar_tostr(Rest, SplitChar, [S | Defs]);
defvar_tostr([?VARDEF(Name, Type, _Initval) | Rest], SplitChar, Defs) ->
    S = io_lib:format("~s ~s", [type_tostr(Type), Name]),
    defvar_tostr(Rest, SplitChar, [S | Defs]);
defvar_tostr([], SplitChar, Defs) ->
    lists:join(SplitChar, lists:reverse(Defs)).


type_tostr({box_type, _, Size, Typeanno}) ->
    io_lib:format("~s[~w]", [type_anno_tostr(Typeanno), Size]);
type_tostr({basic_type, _, Typeanno}) ->
    type_anno_tostr(Typeanno).

type_anno_tostr({T, Depth}) ->
    io_lib:format("~s~s", [T, lists:duplicate(Depth, "*")]);
type_anno_tostr(T) ->
    io_lib:format("~s", [T]).

expr_tostr(_Any) ->
    "...".

exprs_tostr([Expr | Rest]) ->
    [expr_tostr(Expr) | exprs_tostr(Rest)];
exprs_tostr([]) ->
    [].

common_code() ->
    "#include <stdio.h>\n#include <stdlib.h>\n\n"
    "typedef u8 unsigned char;\ntypedef i8 char;\n"
    "typedef u16 unsigned short;\ntypedef i16 short;\n"
    "typedef u32 unsigned int;\ntypedef i32 int;\n"
    "typedef u64 unsigned long;\ntypedef i64 long;\n"
    "typedef f64 double;\ntypedef f32 float;\n\n".


