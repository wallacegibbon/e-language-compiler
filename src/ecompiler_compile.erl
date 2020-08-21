-module(ecompiler_compile).

-export([compile_from_rawast/2]).

compile_from_rawast(Ast, CustomOptions) ->
    _Options = maps:merge(default_options(), CustomOptions),
    Ast1 = ecompiler_fillconst:parse_and_remove_const(Ast),
    {{MapAst, ListAst}, Vars, InitCode} = ecompiler_collectvar:fetch_vars(Ast1),
    ecompiler_type:checktype_ast(MapAst, Vars),
    {{MapAst, ListAst}, Vars, InitCode}.

default_options() ->
    #{pointer_width => 8}.

