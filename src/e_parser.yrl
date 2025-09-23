Nonterminals

e_root_stmts e_root_stmt e_struct_def e_vardefs e_vardef e_vardef_head e_function_def
e_function_head_0 e_function_head_1 e_args
e_attributes e_attribute e_attribute_lst
e_function_stmts e_function_stmt e_if_stmt e_else_stmt e_while_stmt e_return_stmt e_label
e_expr e_call_expr e_array_ref_expr e_pre_minus_plus_expr e_sizeof_expr e_alignof_expr
e_assign_expr e_not_expr e_bnot_expr
e_typeof_type e_type_annos e_type_anno e_calc1 e_calc2 e_bitwise e_cmp e_bool_op
e_pointer_depth e_atomic_literal_values e_reserved
e_array_init_expr e_array_init_elements e_struct_init_expr e_struct_init_fields
e_op2_with_assignment e_struct_init_assignment

.

Terminals

%% operators
';' ':' ',' '=' '{' '}' '[' ']' '(' ')' '<' '>' '+' '-' '*' '/' '^' '@' '.' '!=' '==' '>=' '<='

%% keywords
struct 'end' 'fn' 'rem' 'and' 'or' 'not' 'band' 'bor' 'bnot' 'bxor' 'bsl' 'bsr'
while do 'if' then elif 'else' return sizeof alignof goto as attribute

%% reserved keywords
'cond' 'case' for break continue typeof new void_type

%%
identifier integer float string int_type float_type any_type

.

Rootsymbol e_root_stmts.

e_root_stmts -> e_root_stmt e_root_stmts : ['$1' | '$2'].
e_root_stmts -> e_root_stmt : ['$1'].

e_root_stmt -> e_vardef ';' : '$1'.
e_root_stmt -> e_struct_def : '$1'.
e_root_stmt -> e_function_def : '$1'.

%% type annotation inside function type
e_type_annos -> e_type_anno ';' e_type_annos : ['$1' | '$3'].
e_type_annos -> e_type_anno : ['$1'].

e_type_anno -> 'fn' '(' e_vardefs ')' ':' e_type_anno :
  #e_fn_type{params = types_from_vardefs('$3'), ret = '$6', loc = token_loc('$1')}.
e_type_anno -> 'fn' '(' e_vardefs ')' :
  #e_fn_type{params = types_from_vardefs('$3'), ret = ?VOID(token_loc('$4')), loc = token_loc('$1')}.
e_type_anno -> 'fn' '(' e_type_annos ')' ':' e_type_anno :
  #e_fn_type{params = '$3', ret = '$6', loc = token_loc('$1')}.
e_type_anno -> 'fn' '(' e_type_annos ')' :
  #e_fn_type{params = '$3', ret = ?VOID(token_loc('$4')), loc = token_loc('$1')}.
e_type_anno -> 'fn' '(' ')' ':' e_type_anno :
  #e_fn_type{params = [], ret = '$5', loc = token_loc('$1')}.
e_type_anno -> 'fn' '(' ')' :
  #e_fn_type{params = [], ret = ?VOID(token_loc('$3')), loc = token_loc('$1')}.
e_type_anno -> '{' e_type_anno ',' e_expr '}' :
  #e_array_type{elem_type = '$2', length = token_value('$4'), loc = token_loc('$1')}.
e_type_anno -> int_type e_pointer_depth :
  #e_basic_type{class = integer, p_depth = '$2', tag = token_value('$1'), loc = token_loc('$1')}.
e_type_anno -> int_type :
  #e_basic_type{class = integer, p_depth = 0, tag = token_value('$1'), loc = token_loc('$1')}.
e_type_anno -> float_type e_pointer_depth :
  %#e_basic_type{class = float, p_depth = '$2', tag = token_value('$1'), loc = token_loc('$1')}.
  return_error(token_loc('$1'), "type \"float\" is not supported yet").
e_type_anno -> float_type :
  %#e_basic_type{class = float, p_depth = 0, tag = token_value('$1'), loc = token_loc('$1')}.
  return_error(token_loc('$1'), "type \"float\" is not supported yet").
e_type_anno -> identifier e_pointer_depth :
  #e_basic_type{class = struct, p_depth = '$2', tag = token_value('$1'), loc = token_loc('$1')}.
e_type_anno -> identifier :
  #e_basic_type{class = struct, p_depth = 0, tag = token_value('$1'), loc = token_loc('$1')}.
e_type_anno -> any_type e_pointer_depth :
  #e_basic_type{class = any, p_depth = '$2', tag = any, loc = token_loc('$1')}.
e_type_anno -> any_type :
  return_error(token_loc('$1'), "type \"any\" is only allowed as pointer").
e_type_anno -> void_type :
  return_error(token_loc('$1'), "\"void\" is not a valid type annotation").
e_type_anno -> e_typeof_type :
  '$1'.

%% pointer depth
e_pointer_depth -> '^' e_pointer_depth : '$2' + 1.
e_pointer_depth -> '^' : 1.

%e_typeof_type -> typeof '(' e_expr ')' :
%  #e_typeof{expr = '$3', loc = token_loc('$2')}.
e_typeof_type -> typeof '(' e_expr ')' :
  return_error(token_loc('$1'), "\"typeof\" is not supported by E languge.").

e_vardefs -> e_vardef ';' e_vardefs : ['$1' | '$3'].
e_vardefs -> e_vardef ';' : ['$1'].
e_vardefs -> e_vardef : ['$1'].

e_vardef -> e_vardef_head '=' e_expr :
  '$1'#e_vardef{init_value = '$3'}.
e_vardef -> e_vardef_head :
  '$1'.

e_vardef_head -> identifier ':' e_type_anno e_attributes :
  #e_vardef{name = token_value('$1'), type = '$3', attribute = '$4', loc = token_loc('$1')}.
e_vardef_head -> identifier ':' e_type_anno :
  #e_vardef{name = token_value('$1'), type = '$3', loc = token_loc('$1')}.

%% struct definition
e_struct_def -> struct identifier e_vardefs 'end' :
  #e_struct_raw{name = token_value('$2'), fields = '$3', loc = token_loc('$2')}.

%% function definition
e_function_head_0 -> 'fn' identifier '(' e_vardefs ')' ':' e_type_anno :
  #e_function_raw{name = token_value('$2'), params = '$4', ret_type = '$7', loc = token_loc('$2')}.
e_function_head_0 -> 'fn' identifier '(' e_vardefs ')' :
  #e_function_raw{name = token_value('$2'), params = '$4', ret_type = ?VOID(token_loc('$5')),
                  loc = token_loc('$2')}.
e_function_head_0 -> 'fn' identifier '(' ')' ':' e_type_anno :
  #e_function_raw{name = token_value('$2'), params = [], ret_type = '$6', loc = token_loc('$2')}.
e_function_head_0 -> 'fn' identifier '(' ')' :
  #e_function_raw{name = token_value('$2'), params = [], ret_type = ?VOID(token_loc('$4')),
                  loc = token_loc('$2')}.

e_function_head_1 -> e_function_head_0 e_attributes :
  '$1'#e_function_raw{attribute = '$2'}.
e_function_head_1 -> e_function_head_0 :
  '$1'.

e_function_def -> e_function_head_1 e_function_stmts 'end' :
  '$1'#e_function_raw{stmts = '$2'}.
e_function_def -> e_function_head_1 'end' :
  '$1'.

e_attributes -> attribute '(' e_attribute_lst ')' :
  maps:from_list('$3').

e_attribute_lst -> e_attribute ',' e_attribute_lst :
  ['$1' | '$3'].
e_attribute_lst -> e_attribute ',' :
  ['$1'].
e_attribute_lst -> e_attribute :
  ['$1'].

e_attribute -> identifier '(' string ')' :
  {token_value('$1'), token_value('$3')}.
e_attribute -> identifier '(' integer ')' :
  {token_value('$1'), token_value('$3')}.
e_attribute -> identifier :
  {token_value('$1'), true}.

%% while
e_while_stmt -> while e_expr do e_function_stmts 'end' :
  #e_while_stmt{'cond' = '$2', stmts = '$4', loc = token_loc('$1')}.

%% if
e_if_stmt -> 'if' e_expr then e_function_stmts e_else_stmt :
  #e_if_stmt{'cond' = '$2', then = '$4', 'else' = '$5', loc = token_loc('$1')}.

e_else_stmt -> elif e_expr then e_function_stmts e_else_stmt :
  [#e_if_stmt{'cond' = '$2', then = '$4', 'else' = '$5', loc = token_loc('$1')}].
e_else_stmt -> 'else' e_function_stmts 'end' :
  '$2'.
e_else_stmt -> 'end' :
  [].

Nonassoc 100 e_array_init_expr.
Nonassoc 100 e_struct_init_expr.
Nonassoc 100 return.

e_return_stmt -> return e_expr :
  #e_return_stmt{expr = '$2', loc = token_loc('$1')}.
e_return_stmt -> return :
  #e_return_stmt{expr = 'none', loc = token_loc('$1')}.

e_array_init_expr -> '{' e_array_init_elements '}' :
  #e_array_init_expr{elements = '$2', loc = token_loc('$1')}.
e_array_init_expr -> '{' string '}' :
  #e_array_init_expr{elements = str_to_int_tokens('$2'), loc = token_loc('$1')}.
e_array_init_expr -> '{' '}' :
  #e_array_init_expr{elements = [], loc = token_loc('$1')}.

e_array_init_elements -> e_expr ',' e_array_init_elements :
  ['$1' | '$3'].
e_array_init_elements -> e_expr ',' :
  ['$1'].
e_array_init_elements -> e_expr :
  ['$1'].

e_struct_init_expr -> identifier '{' e_struct_init_fields '}' :
  #e_struct_init_raw_expr{name = token_value('$1'), fields = '$3', loc = token_loc('$1')}.
e_struct_init_expr -> identifier '{' '}' :
  #e_struct_init_raw_expr{name = token_value('$1'), fields = [], loc = token_loc('$1')}.

e_struct_init_fields -> e_struct_init_assignment ',' e_struct_init_fields :
  ['$1' | '$3'].
e_struct_init_fields -> e_struct_init_assignment ',' :
  ['$1'].
e_struct_init_fields -> e_struct_init_assignment :
  ['$1'].

e_struct_init_assignment -> identifier '=' e_expr :
  #e_op{tag = '=',
        data = [#e_varref{name = token_value('$1'), loc = token_loc('$1')}, '$3'],
        loc = token_loc('$2')}.

Nonassoc 1 e_assign_expr.
e_assign_expr -> e_expr e_op2_with_assignment e_expr :
  #e_op{tag = '=',
        data = ['$1', #e_op{tag = token_symbol('$2'), data = ['$1', '$3'], loc = token_loc('$2')}],
        loc = token_loc('$2')}.
e_assign_expr -> e_expr '=' e_expr :
  #e_op{tag = '=',
        data = ['$1', '$3'],
        loc = token_loc('$2')}.

e_op2_with_assignment -> e_calc1 '=' : '$1'.
e_op2_with_assignment -> e_calc2 '=' : '$1'.
e_op2_with_assignment -> e_bitwise '=' : '$1'.

e_args -> e_expr ',' e_args : ['$1' | '$3'].
e_args -> e_expr ',' : ['$1'].
e_args -> e_expr : ['$1'].

e_atomic_literal_values -> integer : replace_tag('$1', e_integer).
e_atomic_literal_values -> float : replace_tag('$1', e_float).
e_atomic_literal_values -> string : replace_tag('$1', e_string).

e_function_stmts -> e_function_stmt e_function_stmts : ['$1' | '$2'].
e_function_stmts -> e_function_stmt : ['$1'].

e_function_stmt -> e_expr ';' : '$1'.
e_function_stmt -> e_vardef ';' : '$1'.
e_function_stmt -> e_if_stmt : '$1'.
e_function_stmt -> e_while_stmt : '$1'.
e_function_stmt -> goto identifier ';' : #e_goto_stmt{label = token_value('$2'), loc = token_loc('$1')}.
e_function_stmt -> e_return_stmt ';' : '$1'.
e_function_stmt -> e_label : '$1'.

e_label -> '@' '@' identifier :
  #e_label{name = token_value('$3'), loc = token_loc('$3')}.

%% sizeof
e_sizeof_expr -> sizeof '(' e_type_anno ')' :
  #e_op{tag = {sizeof, '$3'}, loc = token_loc('$1')}.
%% alignof
e_alignof_expr -> alignof '(' e_type_anno ')' :
  #e_op{tag = {alignof, '$3'}, loc = token_loc('$1')}.
%% not
e_not_expr -> 'not' '(' e_expr ')' :
  #e_op{tag = 'not', data = ['$3'], loc = token_loc('$1')}.
%% bnot
e_bnot_expr -> 'bnot' '(' e_expr ')' :
  #e_op{tag = 'bnot', data = ['$3'], loc = token_loc('$1')}.

%% function invocation
e_call_expr -> e_expr '(' e_args ')' :
  #e_op{tag = {call, '$1'}, data = '$3', loc = token_loc('$2')}.
e_call_expr -> e_expr '(' ')' :
  #e_op{tag = {call, '$1'}, data = [], loc = token_loc('$2')}.

%% array reference
e_array_ref_expr -> e_expr '[' e_expr ']' :
  #e_op{tag = {aref, '$1'}, data = ['$3'], loc = token_loc('$2')}.

e_expr -> e_reserved :
  return_error(token_loc('$1'), e_util:fmt("~s is reserved", [token_symbol('$1')])).
e_expr -> e_expr '.' identifier :
  #e_op{tag = token_symbol('$2'),
        data = ['$1', #e_varref{name = token_value('$3'), loc = token_loc('$3')}],
        loc = token_loc('$2')}.
e_expr -> e_expr '^' :
  %% The memory size `0` is an invalid value, replace it later.
  #e_op{tag = token_symbol('$2'),
        data = ['$1', #e_integer{value = 0, loc = token_loc('$2')}],
        loc = token_loc('$2')}.
e_expr -> e_expr '@' :
  #e_op{tag = token_symbol('$2'), data = ['$1'], loc = token_loc('$2')}.
e_expr -> e_expr e_calc1 e_expr :
  #e_op{tag = token_symbol('$2'), data = ['$1', '$3'], loc = token_loc('$2')}.
e_expr -> e_expr e_calc2 e_expr :
  #e_op{tag = token_symbol('$2'), data = ['$1', '$3'], loc = token_loc('$2')}.
e_expr -> e_expr e_bitwise e_expr :
  #e_op{tag = token_symbol('$2'), data = ['$1', '$3'], loc = token_loc('$2')}.
e_expr -> e_expr e_cmp e_expr :
  #e_op{tag = token_symbol('$2'), data = ['$1', '$3'], loc = token_loc('$2')}.
e_expr -> e_expr e_bool_op e_expr :
  #e_op{tag = token_symbol('$2'), data = ['$1', '$3'], loc = token_loc('$2')}.
e_expr -> identifier :
  #e_varref{name = token_value('$1'), loc = token_loc('$1')}.
e_expr -> e_expr as '(' e_type_anno ')' :
  #e_type_convert{expr = '$1', type = '$4', loc = token_loc('$2')}.
e_expr -> e_pre_minus_plus_expr : '$1'.
e_expr -> e_array_init_expr : '$1'.
e_expr -> e_struct_init_expr : '$1'.
e_expr -> e_atomic_literal_values : '$1'.
e_expr -> e_call_expr : '$1'.
e_expr -> e_array_ref_expr : '$1'.
e_expr -> e_assign_expr : '$1'.
e_expr -> e_sizeof_expr : '$1'.
e_expr -> e_alignof_expr : '$1'.
e_expr -> e_not_expr : '$1'.
e_expr -> e_bnot_expr : '$1'.
e_expr -> '(' e_expr ')' : '$2'.

e_reserved -> ';' : '$1'.
e_reserved -> new : '$1'.
e_reserved -> 'cond' : '$1'.
e_reserved -> 'case' : '$1'.
e_reserved -> for : '$1'.
e_reserved -> break : '$1'.
e_reserved -> continue : '$1'.

Unary 1000 '('.
Unary 1000 '['.
Left 1000 '.'.
Unary 900 '^'.
Unary 900 '@'.

%% the precedence of 'e_pre_minus_plus_expr' needs to be higher than "operator +/-"
Unary 300 e_pre_minus_plus_expr.
e_pre_minus_plus_expr -> '-' e_expr :
  #e_op{tag = token_symbol('$1'), data = ['$2'], loc = token_loc('$1')}.
e_pre_minus_plus_expr -> '+' e_expr :
  #e_op{tag = token_symbol('$1'), data = ['$2'], loc = token_loc('$1')}.

Left 290 e_calc1.
e_calc1 -> '*' : '$1'.
e_calc1 -> '/' : '$1'.
e_calc1 -> 'rem' : '$1'.

Left 280 e_calc2.
e_calc2 -> '+' : '$1'.
e_calc2 -> '-' : '$1'.

Left 270 e_bitwise.
e_bitwise -> 'bsl' : '$1'.
e_bitwise -> 'bsr' : '$1'.
e_bitwise -> 'band' : '$1'.
e_bitwise -> 'bor' : '$1'.
e_bitwise -> 'bxor' : '$1'.

Nonassoc 260 e_cmp.
e_cmp -> '==' : '$1'.
e_cmp -> '!=' : '$1'.
e_cmp -> '>=' : '$1'.
e_cmp -> '<=' : '$1'.
e_cmp -> '>' : '$1'.
e_cmp -> '<' : '$1'.

Left 250 e_bool_op.
e_bool_op -> 'and' : '$1'.
e_bool_op -> 'or' : '$1'.


Erlang code.

-include("e_record_definition.hrl").

str_to_int_tokens({string, Loc, Str}) ->
  [#e_integer{value = Char, loc = Loc} || Char <- Str].

types_from_vardefs(VarDefs) ->
  [Type || #e_vardef{type = Type} <- VarDefs].

token_value({_, _, Val}) -> Val.

token_symbol({Sym, _}) -> Sym.

token_loc(T) -> element(2, T).

replace_tag({_, Loc, Val}, NewTag) -> {NewTag, Loc, Val};
replace_tag({_, Loc     }, NewTag) -> {NewTag, Loc}.

