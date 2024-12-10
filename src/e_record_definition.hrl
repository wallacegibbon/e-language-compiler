-ifndef(E_RECORD_DEFINITION_HRL).
-define(E_RECORD_DEFINITION_HRL, 1).

-type raw_location() :: {Row :: non_neg_integer(), Column :: non_neg_integer()}.
-type raw_token() :: {atom(), raw_location(), _} | {atom(), raw_location()}.

-type location() :: {Filename :: string(), Row :: non_neg_integer(), Column :: non_neg_integer()}.
-type token() :: {atom(), location(), _} | {atom(), location()}.

-type e_var_attribute() :: #{atom() => any()}.
-type e_fn_attribute() :: #{atom() => any()}.

%% There are 3 kinds of data types in E language: `basic type`, `array type` and `function type`.
-record(e_basic_type,
        {
         loc = {"", 0, 0} :: location(),
         p_depth = 0 :: non_neg_integer(),
         class = void :: struct | integer | float | boolean | void | any,
         tag :: atom() %% byte|word|float|void|any|StructName.
        }).

-record(e_array_type,
        {
         loc = {"", 0, 0} :: location(),
         elem_type :: e_type(),
         length :: non_neg_integer()
        }).

-record(e_fn_type,
        {
         loc = {"", 0, 0} :: location(),
         params = [] :: [e_type()],
         ret :: e_type()
        }).

-record(e_typeof,
        {
         loc = {"", 0, 0} :: location(),
         expr :: e_expr()
        }).

-type e_type() :: #e_basic_type{} | #e_array_type{} | #e_fn_type{} | #e_typeof{}.

-record(e_label,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom()
        }).

%% Variable definition. Will be used on both variable definition and struct field definition.
-record(e_vardef,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom(),
         type :: e_type(),
         init_value = none :: none | e_expr(),
         attribute = #{} :: e_var_attribute()
        }).

-record(e_varref,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom()
        }).

-type e_var_type() :: none | local | global.
-type e_var_offset() :: {Offset :: non_neg_integer(), Size :: pos_integer()}.

%% `e_vars` is used by functions, structs and global variables to hold variables (including parameters) and fields.
%% The alignment of e_vars is the alignment of the biggest element in this `e_vars`.
%% The alignment should be higher than or equals `1`. `0` is the uninitialized value for alignments.
-record(e_vars,
        {
         names = [] :: [atom()], %% Keeps the declare order of variables/fields.
         type_map = #{} :: #{atom() => e_type()},
         offset_map = #{} :: #{atom() => e_var_offset()},
         align = 0 :: non_neg_integer(),
         size = 0 :: non_neg_integer(), %% The whole size of this `e_vars`.
         shifted_size = 0 :: non_neg_integer(),
         tag = none :: e_var_type()
        }).

-record(e_function,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom(),
         vars = #e_vars{} :: #e_vars{}, %% Contains both parameters and variables.
         param_names = [] :: [atom()],
         type :: #e_fn_type{},
         stmts = [] :: [e_stmt()],
         attribute = #{} :: e_fn_attribute()
        }).

-record(e_struct,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom(),
         fields = #e_vars{} :: #e_vars{},
         default_value_map = #{} :: #{atom() := e_expr()}
        }).

-record(e_struct_init_raw_expr,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom(),
         fields = [] :: [e_expr()]
        }).

-record(e_struct_init_expr,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom(),
         field_value_map = #{} :: #{atom() := e_expr()}
        }).

-record(e_array_init_expr,
        {
         loc = {"", 0, 0} :: location(),
         elements = [] :: [e_expr()]
        }).

-record(e_type_convert,
        {
         loc = {"", 0, 0} :: location(),
         expr :: e_expr(),
         type :: e_type()
        }).

-type e_op_tag() :: '*' | '/' | '+' | '-' | '@' | '^' | 'rem' | 'and' | 'or' | 'band' | 'bor' | 'bxor' | 'bsl' | 'bsr' | '~' | '!' | '.' | '=' | '>' | '<' | '>=' | '<=' | '!=' | '==' | {call, e_expr()} | {aref, e_expr()} | {sizeof, e_type()} | {alignof, e_type()}.

-record(e_op,
        {
         loc = {"", 0, 0} :: location(),
         tag :: e_op_tag(),
         data = [] :: [e_expr()]
        }).

-record(e_integer,
        {
         loc = {"", 0, 0} :: location(),
         value :: integer()
        }).

-record(e_float,
        {
         loc = {"", 0, 0} :: location(),
         value :: float()
        }).

-record(e_string,
        {
         loc = {"", 0, 0} :: location(),
         value :: string()
        }).

-record(e_goto_stmt,
        {
         loc = {"", 0, 0} :: location(),
         label :: atom()
        }).

-record(e_return_stmt,
        {
         loc = {"", 0, 0} :: location(),
         expr = none :: none | e_expr()
        }).

-record(e_while_stmt,
        {
         loc = {"", 0, 0} :: location(),
         'cond' :: e_expr(),
         stmts = [] :: [e_stmt()]
        }).

-record(e_if_stmt,
        {
         loc = {"", 0, 0} :: location(),
         'cond' :: e_expr(),
         then = [] :: [e_stmt()],
         'else' = [] :: [e_stmt()]
        }).

-record(e_struct_raw,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom(),
         fields = [] :: [e_stmt()]
        }).

-record(e_function_raw,
        {
         loc = {"", 0, 0} :: location(),
         name :: atom(),
         params = [] :: [e_stmt()],
         ret_type :: e_type(),
         stmts = [] :: [e_stmt()],
         attribute = #{} :: e_fn_attribute()
        }).

-type e_expr() :: #e_op{} | #e_integer{} | #e_float{} | #e_string{} | #e_varref{} | #e_struct_init_expr{} | #e_array_init_expr{} | #e_type_convert{}.
-type e_stmt() :: #e_if_stmt{} | #e_while_stmt{} | #e_return_stmt{} | #e_goto_stmt{} | #e_label{} | e_expr().

-type e_ast_raw_elem() :: #e_function_raw{} | #e_struct_raw{} | #e_vardef{} | e_stmt().
-type e_ast_elem() :: #e_function{} | #e_struct{} | #e_vardef{} | e_stmt().

-type e_ast_raw() :: [e_ast_raw_elem()].
-type e_ast() :: [e_ast_elem()].

-define(OP2(Tag, O1, O2, Loc), #e_op{tag = Tag, data = [O1, O2], loc = Loc}).
-define(OP2(Tag, O1, O2), #e_op{tag = Tag, data = [O1, O2]}).

-define(OP1(Tag, O, Loc), #e_op{tag = Tag, data = [O], loc = Loc}).
-define(OP1(Tag, O), #e_op{tag = Tag, data = [O]}).

-define(CALL(Fn, ARGS, Loc), #e_op{tag = {call, Fn}, data = ARGS, loc = Loc}).
-define(CALL(Fn, ARGS), #e_op{tag = {call, Fn}, data = ARGS}).

-define(AREF(P, Index, Loc), #e_op{tag = {aref, P}, data = [Index], loc = Loc}).
-define(AREF(P, Index), #e_op{tag = {aref, P}, data = [Index]}).

-define(VREF(Name, Loc), #e_varref{name = Name, loc = Loc}).
-define(VREF(Name), #e_varref{name = Name}).

-define(I(V, Loc), #e_integer{value = V, loc = Loc}).
-define(I(V), #e_integer{value = V}).

-define(F(V, Loc), #e_float{value = V, loc = Loc}).
-define(F(V), #e_float{value = V}).

-define(S(V, Loc), #e_string{value = V, loc = Loc}).
-define(S(V), #e_string{value = V}).

-define(VOID(Loc), #e_basic_type{class = void, tag = void, p_depth = 0, loc = Loc}).
-define(VOID(), #e_basic_type{class = void, tag = void, p_depth = 0}).

-define(IS_ARITH(Tag),
        (
          Tag =:= '+' orelse Tag =:= '-' orelse Tag =:= '*' orelse Tag =:= '/' orelse Tag =:= 'rem' orelse
          Tag =:= 'band' orelse Tag =:= 'bor' orelse Tag =:= 'bxor' orelse Tag =:= 'bnot' orelse
          Tag =:= 'bsl' orelse Tag =:= 'bsr'
        )).

-define(IS_IMMID_ARITH(Tag),
        (
          Tag =:= '+' orelse Tag =:= 'band' orelse Tag =:= 'bor' orelse Tag =:= 'bxor'
        )).

-define(IS_SHIFT(Tag),
        (
          Tag =:= 'bsl' orelse Tag =:= 'bsr'
        )).

-define(IS_LOGIC(Tag),
        (
          Tag =:= 'and' orelse Tag =:= 'or' orelse Tag =:= 'not'
        )).

-define(IS_COMPARE(Tag),
        (
          Tag =:= '>' orelse Tag =:= '<' orelse Tag =:= '==' orelse Tag =:= '!=' orelse
          Tag =:= '>=' orelse Tag =:= '<='
        )).

-endif.
