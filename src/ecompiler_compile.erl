-module(ecompiler_compile).

-export([compile_from_rawast/1]).

compile_from_rawast(Ast) ->
    Ast1 = ecompiler_fillconst:parse_and_remove_const(Ast),
    {Ast2, Vars, InitCode} = ecompiler_collectvar:fetch_vars(Ast1),
    ecompiler_type:checktype_ast(Ast2, Vars),
    {Ast2, Vars, InitCode}.

