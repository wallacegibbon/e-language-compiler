-module(ecompiler_genc).

-export([generate_ccode/4]).

-import(ecompiler_util,
        [exprsmap/2, fn_struct_map/1, getvalues_bykeys/2]).

-include("./ecompiler_frame.hrl").

generate_ccode(Ast, GlobalVars, InitCode, OutputFile) ->
    {FnMap, StructMap} = fn_struct_map(Ast),
    Ctx = {FnMap, StructMap, GlobalVars},
    Ast2 = lists:map(fun (A) -> fixfunction_for_c(A, Ctx)
                     end,
                     Ast),
    InitCode2 = fixexprs_for_c(InitCode, Ctx),
    %io:format(">>>~p~n", [Ast2]),
    %% struct definition have to be before function declarations
    CheckStruct = fun (A) -> element(1, A) =:= struct end,
    {StructAst, FnAst} = lists:partition(CheckStruct, Ast2),
    {StructStatements, []} = statements_tostr(StructAst,
                                              []),
    {FnStatements, FnDeclars} = statements_tostr(FnAst,
                                                 InitCode2),
    VarStatements = mapvars_to_str(GlobalVars),
    Code = [common_code(),
            "\n\n",
            StructStatements,
            "\n\n",
            VarStatements,
            "\n\n",
            FnDeclars,
            "\n\n",
            FnStatements],
    file:write_file(OutputFile, Code).

fixfunction_for_c(#function{exprs = Exprs,
                            var_types = VarTypes} =
                      F,
                  {FnMap, StructMap, GlobalVars}) ->
    Ctx1 = {FnMap,
            StructMap,
            maps:merge(GlobalVars, VarTypes)},
    F#function{exprs = fixexprs_for_c(Exprs, Ctx1)};
fixfunction_for_c(Any, _) -> Any.

fixexprs_for_c(Exprs, Ctx) ->
    exprsmap(fun (E) -> fixexpr_for_c(E, Ctx) end, Exprs).

fixexpr_for_c(#op1{operator = '@', operand = Operand,
                   line = Line} =
                  E,
              {FnMap, StructMap, VarTypes} = Ctx) ->
    case ecompiler_type:typeof_expr(Operand,
                                    {VarTypes, FnMap, StructMap, none})
        of
        #array_type{} ->
            #op2{operator = '.', op1 = fixexpr_for_c(Operand, Ctx),
                 op2 = #varref{name = val, line = Line}};
        _ -> E
    end;
fixexpr_for_c(#op1{operand = Operand} = E, Ctx) ->
    E#op1{operand = fixexpr_for_c(Operand, Ctx)};
fixexpr_for_c(#op2{op1 = Op1, op2 = Op2} = E, Ctx) ->
    E#op2{op1 = fixexpr_for_c(Op1, Ctx),
          op2 = fixexpr_for_c(Op2, Ctx)};
fixexpr_for_c(Any, _) -> Any.

common_code() ->
    "#include <stdio.h>\n#include <stdlib.h>\n#inc"
    "lude <string.h>\n\ntypedef unsigned "
    "int usize;\ntypedef int isize;\ntypedef "
    "unsigned char u8;\ntypedef char i8;\ntypedef "
    "unsigned short u16;\ntypedef short i16;\ntype"
    "def unsigned int u32;\ntypedef int i32;\ntype"
    "def unsigned long u64;\ntypedef long "
    "i64;\ntypedef double f64;\ntypedef float "
    "f32;\n\n".

statements_tostr(Statements, InitCode) ->
    statements_tostr(Statements, InitCode, [], []).

statements_tostr([#function{name = Name,
                            param_names = ParamNames, type = Fntype,
                            var_types = VarTypes, exprs = Exprs}
                  | Rest],
                 InitCode, StatementStrs, FnDeclars) ->
    ParamNameAtoms = get_names(ParamNames),
    PureParams = kvlist_frommap(ParamNameAtoms,
                                maps:with(ParamNameAtoms, VarTypes)),
    PureVars = maps:without(ParamNameAtoms, VarTypes),
    Declar = fn_declar_str(Name,
                           params_to_str(PureParams),
                           Fntype#fun_type.ret),
    Exprs2 = case Name =:= main of
                 true -> InitCode ++ Exprs;
                 _ -> Exprs
             end,
    S = io_lib:format("~s~n{~n~s~n~n~s~n}~n~n",
                      [Declar,
                       mapvars_to_str(PureVars),
                       exprs_tostr(Exprs2)]),
    statements_tostr(Rest,
                     InitCode,
                     [S | StatementStrs],
                     [Declar ++ ";\n" | FnDeclars]);
statements_tostr([#struct{name = Name,
                          field_types = FieldTypes, field_names = FieldNames}
                  | Rest],
                 InitCode, StatementStrs, FnDeclars) ->
    FieldList = kvlist_frommap(get_names(FieldNames),
                               FieldTypes),
    S = io_lib:format("struct ~s {~n~s~n};~n~n",
                      [Name, listvars_to_str(FieldList)]),
    statements_tostr(Rest,
                     InitCode,
                     [S | StatementStrs],
                     FnDeclars);
statements_tostr([], _, StatementStrs, FnDeclars) ->
    {lists:reverse(StatementStrs),
     lists:reverse(FnDeclars)}.

fn_declar_str(Name, ParamStr,
              #basic_type{pdepth = N} = Rettype)
    when N > 0 ->
    fnret_type_tostr(Rettype,
                     io_lib:format("(*~s(~s))", [Name, ParamStr]));
fn_declar_str(Name, ParamStr, #fun_type{} = Rettype) ->
    fnret_type_tostr(Rettype,
                     io_lib:format("(*~s(~s))", [Name, ParamStr]));
fn_declar_str(Name, ParamStr, Rettype) ->
    type_tostr(Rettype,
               io_lib:format("~s(~s)", [Name, ParamStr])).

params_to_str(NameTypePairs) ->
    lists:join(",",
               lists:map(fun ({N, T}) -> type_tostr(T, N) end,
                         NameTypePairs)).

params_to_str_noname(Types) ->
    lists:join(",",
               lists:map(fun (T) -> type_tostr(T, "") end, Types)).

%% order is not necessary for vars
mapvars_to_str(VarsMap) when is_map(VarsMap) ->
    lists:flatten(lists:join(";\n",
                             vars_to_str(maps:to_list(VarsMap), [])),
                  ";").

listvars_to_str(VarList) when is_list(VarList) ->
    lists:flatten(lists:join(";\n",
                             vars_to_str(VarList, [])),
                  ";").

vars_to_str([{Name, Type} | Rest], Strs) ->
    vars_to_str(Rest, [type_tostr(Type, Name) | Strs]);
vars_to_str([], Strs) -> lists:reverse(Strs).

get_names(VarrefList) ->
    lists:map(fun (#varref{name = N}) -> N end, VarrefList).

kvlist_frommap(NameAtoms, ValueMap) ->
    lists:zip(NameAtoms,
              getvalues_bykeys(NameAtoms, ValueMap)).

fnret_type_tostr(#fun_type{params = Params,
                           ret = Rettype},
                 NameParams) ->
    Paramstr = params_to_str_noname(Params),
    NewNameParams = io_lib:format("~s(~s)",
                                  [NameParams, Paramstr]),
    type_tostr(Rettype, NewNameParams);
fnret_type_tostr(#basic_type{pdepth = N} = T,
                 NameParams)
    when N > 0 ->
    type_tostr(T#basic_type{pdepth = N - 1}, NameParams).

%% convert type to C string
type_tostr(#array_type{len = Len,
                       elemtype = ElementType},
           Varname) ->
    io_lib:format("struct {~s val[~w];} ~s",
                  [type_tostr(ElementType, ""), Len, Varname]);
type_tostr(#basic_type{class = Class, tag = Tag,
                       pdepth = Depth},
           Varname)
    when Depth > 0 ->
    io_lib:format("~s~s ~s",
                  [typetag_tostr(Class, Tag),
                   lists:duplicate(Depth, "*"),
                   Varname]);
type_tostr(#basic_type{class = Class, tag = Tag,
                       pdepth = 0},
           Varname) ->
    io_lib:format("~s ~s",
                  [typetag_tostr(Class, Tag), Varname]);
type_tostr(#fun_type{params = Params, ret = Rettype},
           Varname) ->
    Paramstr = params_to_str_noname(Params),
    NameParams = io_lib:format("(*~s)(~s)",
                               [Varname, Paramstr]),
    type_tostr(Rettype, NameParams).

typetag_tostr(struct, Name) ->
    io_lib:format("struct ~s", [Name]);
typetag_tostr(_, Name) -> atom_to_list(Name).

%% convert expression to C string
exprs_tostr(Exprs) ->
    [lists:join("\n", exprs_tostr(Exprs, []))].

exprs_tostr([Expr | Rest], ExprList) ->
    exprs_tostr(Rest, [expr_tostr(Expr, $;) | ExprList]);
exprs_tostr([], ExprList) -> lists:reverse(ExprList).

expr_tostr(#if_expr{condition = Condition, then = Then,
                    else = Else},
           _) ->
    io_lib:format("if (~s) {\n~s\n} else {\n~s}",
                  [expr_tostr(Condition, $\s),
                   exprs_tostr(Then),
                   exprs_tostr(Else)]);
expr_tostr(#while_expr{condition = Condition,
                       exprs = Exprs},
           _) ->
    io_lib:format("while (~s) {\n~s\n}\n",
                  [expr_tostr(Condition, $\s), exprs_tostr(Exprs)]);
expr_tostr(#op2{operator = '::',
                op1 = #varref{name = c}, op2 = Op2},
           Endchar) ->
    expr_tostr(Op2, Endchar);
expr_tostr(#op2{operator = Operator, op1 = Op1,
                op2 = Op2},
           Endchar) ->
    io_lib:format("(~s ~s ~s)~c",
                  [expr_tostr(Op1, $\s),
                   translate_operator(Operator),
                   expr_tostr(Op2, $\s),
                   Endchar]);
expr_tostr(#op1{operator = Operator, operand = Operand},
           Endchar) ->
    io_lib:format("(~s ~s)~c",
                  [translate_operator(Operator),
                   expr_tostr(Operand, $\s),
                   Endchar]);
expr_tostr(#call{fn = Fn, args = Args}, Endchar) ->
    ArgStr = lists:join(",",
                        lists:map(fun (E) -> expr_tostr(E, $\s) end, Args)),
    io_lib:format("~s(~s)~c",
                  [expr_tostr(Fn, $\s), ArgStr, Endchar]);
expr_tostr(#return{expr = Expr}, Endchar) ->
    io_lib:format("return ~s~c",
                  [expr_tostr(Expr, $\s), Endchar]);
expr_tostr(#goto{expr = Expr}, Endchar) ->
    io_lib:format("goto ~s~c",
                  [expr_tostr(Expr, $\s), Endchar]);
expr_tostr(#label{name = Name}, _) ->
    io_lib:format("~s:", [Name]);
expr_tostr(#varref{name = Name}, Endchar) ->
    io_lib:format("~s~c", [Name, Endchar]);
expr_tostr({Any, _Line, Value}, Endchar)
    when Any =:= integer; Any =:= float ->
    io_lib:format("~w~c", [Value, Endchar]);
expr_tostr({Any, _Line, S}, Endchar)
    when Any =:= string ->
    io_lib:format("\"~s\"~c",
                  [handle_special_char_instr(S), Endchar]).

-define(SPECIAL_CHARMAP,
        #{$\n => "\\n", $\r => "\\r", $\t => "\\t",
          $\f => "\\f", $\b => "\\b"}).

handle_special_char_instr(Str) ->
    lists:map(fun (C) -> maps:get(C, ?SPECIAL_CHARMAP, C)
              end,
              Str).

translate_operator(assign) -> "=";
translate_operator('rem') -> "%";
translate_operator('bxor') -> "^";
translate_operator('bsr') -> ">>";
translate_operator('bsl') -> "<<";
translate_operator('band') -> "&";
translate_operator('bor') -> "|";
translate_operator('and') -> "&&";
translate_operator('or') -> "||";
translate_operator('@') -> "&";
translate_operator('^') -> "*";
translate_operator(Any) -> Any.
