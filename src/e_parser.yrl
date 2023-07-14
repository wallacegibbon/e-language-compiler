Nonterminals

root_stmts root_stmt struct_def function_def var_defs var_def params function_stmts function_stmt if_stmt else_stmt while_stmt goto_label
expr call_expr pre_minus_plus_expr sizeof_expr assign_expr type_annotations type_anno op19 op30 op29 op28 op27 op26 op25 op2_with_assignment
pointer_depth atomic_literal_values array_init_expr array_init_elements struct_init_expr struct_init_fields struct_init_assignment reserved_keyword

.

Terminals

%% operators
',' ':' ';' '=' '{' '}' '(' ')' '<' '>' '+' '-' '*' '/' '^' '@' '.' '~' '!' '!=' '==' '>=' '<='

%% keywords
struct 'end' 'fun' 'rem' 'and' 'or' 'band' 'bor' 'bxor' 'bsl' 'bsr' while do 'if' then elif else return sizeof goto as new

%% reserved keywords
'cond' 'case' for break continue

%%
identifier integer float string int_type float_type void_type any_type

.

Rootsymbol root_stmts.

root_stmts -> root_stmt root_stmts			: ['$1' | '$2'].
root_stmts -> root_stmt					: ['$1'].

root_stmt -> struct_def					: '$1'.
root_stmt -> function_def				: '$1'.
root_stmt -> var_def ';'				: '$1'.

%% type annotation inside array or function
type_annotations -> type_anno ',' type_annotations	: ['$1' | '$3'].
type_annotations -> type_anno				: ['$1'].

type_anno -> 'fun' '(' type_annotations ')' ':' type_anno :
	#fn_type{params = '$3', ret = '$6', line = token_line('$1')}.
type_anno -> 'fun' '(' type_annotations ')' :
	#fn_type{params = '$3', ret = e_util:void_type(token_line('$4')), line = token_line('$1')}.
type_anno -> 'fun' '(' ')' ':' type_anno :
	#fn_type{params = [], ret = '$5', line = token_line('$1')}.
type_anno -> 'fun' '(' ')' :
	#fn_type{params = [], ret = e_util:void_type(token_line('$3')), line = token_line('$1')}.
type_anno -> '{' type_anno ',' expr '}' :
	#array_type{elem_type = '$2', length = token_value('$4'), line = token_line('$1')}.
type_anno -> int_type pointer_depth :
	#basic_type{class = integer, p_depth = '$2', tag = token_value('$1'), line = token_line('$1')}.
type_anno -> int_type :
	#basic_type{class = integer, p_depth = 0, tag = token_value('$1'), line = token_line('$1')}.
type_anno -> float_type pointer_depth :
	#basic_type{class = float, p_depth = '$2', tag = token_value('$1'), line = token_line('$1')}.
type_anno -> float_type :
	#basic_type{class = float, p_depth = 0, tag = token_value('$1'), line = token_line('$1')}.
type_anno -> identifier pointer_depth :
	#basic_type{class = struct, p_depth = '$2', tag = token_value('$1'), line = token_line('$1')}.
type_anno -> identifier :
	#basic_type{class = struct, p_depth = 0, tag = token_value('$1'), line = token_line('$1')}.
type_anno -> any_type pointer_depth :
	#basic_type{class = any, p_depth = '$2', tag = void, line = token_line('$1')}.
type_anno -> any_type :
	return_error(token_line('$1'), "type any is not allowed here").
type_anno -> void_type :
	return_error(token_line('$1'), "type void is not allowed here").

%% pointer depth
pointer_depth -> '^' pointer_depth			: '$2' + 1.
pointer_depth -> '^'					: 1.

var_defs -> var_def ',' var_defs			: ['$1' | '$3'].
var_defs -> var_def ','					: ['$1'].
var_defs -> var_def					: ['$1'].

var_def -> identifier ':' type_anno '=' expr :
	#var_def{name = token_value('$1'), type = '$3', init_value = '$5', line = token_line('$1')}.

var_def -> identifier ':' type_anno :
	#var_def{name = token_value('$1'), type = '$3', line = token_line('$1')}.

%% struct definition
struct_def -> struct identifier var_defs 'end' :
	#struct_raw{name = token_value('$2'), fields = '$3', line = token_line('$2')}.

%% function definition
function_def -> 'fun' identifier '(' var_defs ')' ':' type_anno function_stmts 'end' :
	#function_raw{name = token_value('$2'), params = '$4', ret_type = '$7', stmts = '$8', line = token_line('$2')}.
function_def -> 'fun' identifier '(' ')' ':' type_anno function_stmts 'end' :
	#function_raw{name = token_value('$2'), params = [], ret_type = '$6', stmts = '$7', line = token_line('$2')}.
function_def -> 'fun' identifier '(' var_defs ')' function_stmts 'end' :
	#function_raw{name = token_value('$2'), params = '$4', ret_type = e_util:void_type(token_line('$5')), stmts = '$6', line = token_line('$2')}.
function_def -> 'fun' identifier '(' ')' function_stmts 'end' :
	#function_raw{name = token_value('$2'), params = [], ret_type = e_util:void_type(token_line('$4')), stmts = '$5', line = token_line('$2')}.

%% while
while_stmt -> while expr do function_stmts 'end'	: #while_stmt{condi = '$2', stmts = '$4', line = token_line('$1')}.

%% if
if_stmt -> 'if' expr then function_stmts else_stmt	: #if_stmt{condi = '$2', then = '$4', else = '$5', line = token_line('$1')}.

else_stmt -> elif expr then function_stmts else_stmt	: [#if_stmt{condi = '$2', then = '$4', else = '$5', line = token_line('$1')}].
else_stmt -> else function_stmts 'end'			: '$2'.
else_stmt -> 'end'					: [].

%% array_init_expr and struct_init_expr contains similar pattern '{' '}'.
%% make the precedence of array_init_expr higher than struct_init_expr
Unary 2100 array_init_expr.
array_init_expr -> '{' array_init_elements '}'		: #array_init_expr{elements = '$2', line = token_line('$1')}.
array_init_expr -> '{' string '}'			: #array_init_expr{elements = str_to_int_tokens('$2'), line = token_line('$1')}.

array_init_elements -> expr ',' array_init_elements	: ['$1' | '$3'].
array_init_elements -> expr				: ['$1'].

Unary 2000 struct_init_expr.
struct_init_expr -> identifier '{' struct_init_fields '}' :
	#struct_init_raw_expr{name = token_value('$1'), fields = '$3', line = token_line('$1')}.

struct_init_fields -> struct_init_assignment ',' struct_init_fields :
	['$1' | '$3'].
struct_init_fields -> struct_init_assignment :
	['$1'].

struct_init_assignment -> identifier '=' expr :
	#e_expr{tag = '=', data = [#var_ref{name = token_value('$1'), line = token_line('$1')}, '$3'], line = token_line('$2')}.

%% sizeof
sizeof_expr -> sizeof '(' type_anno ')'			: #e_expr{tag = {sizeof, '$3'}, line = token_line('$2')}.

%% function invocation
call_expr -> expr '(' params ')'			: #e_expr{tag = {call, '$1'}, data = '$3', line = token_line('$2')}.
call_expr -> expr '(' ')'				: #e_expr{tag = {call, '$1'}, data = [], line = token_line('$2')}.

assign_expr -> expr op2_with_assignment expr :
	#e_expr{tag = '=', data = ['$1', #e_expr{tag = token_symbol('$2'), data = ['$1', '$3'], line = token_line('$2')}], line = token_line('$2')}.
assign_expr -> expr '=' expr :
	#e_expr{tag = '=', data = ['$1', '$3'], line = token_line('$2')}.

op2_with_assignment -> op29 '='				: '$1'.
op2_with_assignment -> op28 '='				: '$1'.
op2_with_assignment -> op27 '='				: '$1'.

params -> expr ',' params				: ['$1' | '$3'].
params -> expr ','					: ['$1'].
params -> expr						: ['$1'].

atomic_literal_values -> integer			: '$1'.
atomic_literal_values -> float				: '$1'.
atomic_literal_values -> string				: '$1'.

function_stmts -> function_stmt function_stmts		: ['$1' | '$2'].
function_stmts -> function_stmt				: ['$1'].

function_stmt -> expr ';'				: '$1'.
function_stmt -> var_def ';'				: '$1'.
function_stmt -> assign_expr ';'			: '$1'.
function_stmt -> if_stmt				: '$1'.
function_stmt -> while_stmt				: '$1'.
function_stmt -> goto expr ';'				: #goto_stmt{expr = '$2', line = token_line('$1')}.
function_stmt -> return expr ';'			: #return_stmt{expr = '$2', line = token_line('$1')}.
function_stmt -> goto_label				: '$1'.

goto_label -> '@' identifier				: #goto_label{name = token_value('$2'), line = token_line('$2')}.

expr -> reserved_keyword :
	return_error(token_line('$1'), e_util:fmt("~s is reserved keyword", [token_symbol('$1')])).
expr -> expr op30 expr :
	#e_expr{tag = token_symbol('$2'), data = ['$1', '$3'], line=token_line('$2')}.
expr -> expr op29 expr :
	#e_expr{tag = token_symbol('$2'), data = ['$1', '$3'], line=token_line('$2')}.
expr -> expr op28 expr :
	#e_expr{tag = token_symbol('$2'), data = ['$1', '$3'], line = token_line('$2')}.
expr -> expr op27 expr :
	#e_expr{tag = token_symbol('$2'), data = ['$1', '$3'], line = token_line('$2')}.
expr -> expr op26 expr :
	#e_expr{tag = token_symbol('$2'), data = ['$1', '$3'], line = token_line('$2')}.
expr -> expr op25 expr :
	#e_expr{tag = token_symbol('$2'), data = ['$1', '$3'], line = token_line('$2')}.
expr -> expr op19 :
	#e_expr{tag = token_symbol('$2'), data = ['$1'], line = token_line('$2')}.
expr -> identifier :
	#var_ref{name = token_value('$1'), line = token_line('$1')}.
expr -> pre_minus_plus_expr				: '$1'.
expr -> array_init_expr					: '$1'.
expr -> struct_init_expr				: '$1'.
expr -> atomic_literal_values				: '$1'.
expr -> call_expr					: '$1'.
expr -> sizeof_expr					: '$1'.
expr -> '(' expr ')'					: '$2'.
expr -> '(' expr as type_anno ')'			: #type_convert{expr = '$2', type = '$4', line = token_line('$3')}.

reserved_keyword -> new					: '$1'.
reserved_keyword -> 'cond'				: '$1'.
reserved_keyword -> 'case'				: '$1'.
reserved_keyword -> for					: '$1'.
reserved_keyword -> break				: '$1'.
reserved_keyword -> continue				: '$1'.

%% the precedence of 'pre_minus_plus_expr' needs to be
%% higher than "operator +/-"
Unary 300 pre_minus_plus_expr.
pre_minus_plus_expr -> '-' expr				: #e_expr{tag = token_symbol('$1'), data = ['$2'], line = token_line('$1')}.
pre_minus_plus_expr -> '+' expr				: #e_expr{tag = token_symbol('$1'), data = ['$2'], line = token_line('$1')}.

Unary 900 op19.
op19 -> '^'						: '$1'.
op19 -> '@'						: '$1'.
op19 -> '!'						: '$1'.
op19 -> '~'						: '$1'.

Left 1000 op30.
op30 -> '.'						: '$1'.

Left 290 op29.
op29 -> '*'						: '$1'.
op29 -> '/'						: '$1'.
op29 -> 'rem'						: '$1'.

Left 280 op28.
op28 -> '+'						: '$1'.
op28 -> '-'						: '$1'.

Left 270 op27.
op27 -> 'bsl'						: '$1'.
op27 -> 'bsr'						: '$1'.
op27 -> 'band'						: '$1'.
op27 -> 'bor'						: '$1'.
op27 -> 'bxor'						: '$1'.

Nonassoc 260 op26.
op26 -> '=='						: '$1'.
op26 -> '!='						: '$1'.
op26 -> '>='						: '$1'.
op26 -> '<='						: '$1'.
op26 -> '>'						: '$1'.
op26 -> '<'						: '$1'.

Left 250 op25.
op25 -> 'and'						: '$1'.
op25 -> 'or'						: '$1'.


Erlang code.

-include("e_record_definition.hrl").

str_to_int_tokens({string, Line, Str}) -> lists:map(fun (Char) -> {integer, Line, Char} end, Str).

token_value({_, _, Val}) -> Val.

token_symbol({Sym, _}) -> Sym.

token_line(T) -> element(2, T).

