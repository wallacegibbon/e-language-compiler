Nonterminals

root_statements root_statement struct_definition function_definition variable_definitions variable_definition parameters
function_statements function_statement if_statement else_statement while_statement goto_label
expression  call_expr pre_minus_plus_expr sizeof_expression assign_expr op19 op30 op29 op28 op27 op26 op25 op2_with_assignment
type_annotations type_annotation
pointer_depth atomic_literal_values array_init_expr array_init_elements struct_init_expr struct_init_fields struct_init_assignment
reserved_keyword

.

Terminals

%% operators
',' ':' ';' '=' '{' '}' '(' ')' '<' '>' '+' '-' '*' '/' '^' '@' '.' '~' '!' '!=' '==' '>=' '<='
%% keywords
struct 'end' 'fun' 'rem' 'and' 'or' 'band' 'bor' 'bxor' 'bsl' 'bsr' while do 'if' then elif else return sizeof goto as new

%% reserved keywords
'cond' 'case' for break continue

%%
identifier integer float string integerType floatType void_type anyType

.

Rootsymbol root_statements.

root_statements -> root_statement root_statements : ['$1' | '$2'].
root_statements -> root_statement : ['$1'].

root_statement -> struct_definition : '$1'.
root_statement -> function_definition : '$1'.
root_statement -> variable_definition ';' : '$1'.

%% type annotation inside array or function
type_annotations -> type_annotation ',' type_annotations : ['$1' | '$3'].
type_annotations -> type_annotation : ['$1'].

type_annotation -> 'fun' '(' type_annotations ')' ':' type_annotation :
    #function_type{parameters = '$3', ret = '$6', line = token_line('$1')}.
type_annotation -> 'fun' '(' type_annotations ')' :
    #function_type{parameters = '$3', ret = e_util:void_type(token_line('$4')), line = token_line('$1')}.
type_annotation -> 'fun' '(' ')' ':' type_annotation :
    #function_type{parameters = [], ret = '$5', line = token_line('$1')}.
type_annotation -> 'fun' '(' ')' :
    #function_type{parameters = [], ret = e_util:void_type(token_line('$3')), line = token_line('$1')}.
type_annotation -> '{' type_annotation ',' expression '}' :
    #array_type{elemtype = '$2', length = token_value('$4'), line = token_line('$1')}.
type_annotation -> integerType pointer_depth :
    #basic_type{class = integer, pdepth = '$2', tag = token_value('$1'), line = token_line('$1')}.
type_annotation -> integerType :
    #basic_type{class = integer, pdepth = 0, tag = token_value('$1'), line = token_line('$1')}.
type_annotation -> floatType pointer_depth :
    #basic_type{class = float, pdepth = '$2', tag = token_value('$1'), line = token_line('$1')}.
type_annotation -> floatType :
    #basic_type{class = float, pdepth = 0, tag = token_value('$1'), line = token_line('$1')}.
type_annotation -> identifier pointer_depth :
    #basic_type{class = struct, pdepth = '$2', tag = token_value('$1'), line = token_line('$1')}.
type_annotation -> identifier :
    #basic_type{class = struct,pdepth = 0, tag = token_value('$1'), line = token_line('$1')}.
type_annotation -> anyType pointer_depth :
    #basic_type{class = any, pdepth = '$2', tag = void, line = token_line('$1')}.
type_annotation -> anyType :
    return_error(token_line('$1'), "type any is not allowed here").
type_annotation -> void_type :
    return_error(token_line('$1'), "type void is not allowed here").

%% pointer depth
pointer_depth -> '^' pointer_depth : '$2' + 1.
pointer_depth -> '^' : 1.

variable_definitions -> variable_definition ',' variable_definitions : ['$1' | '$3'].
variable_definitions -> variable_definition ',' : ['$1'].
variable_definitions -> variable_definition : ['$1'].

variable_definition -> identifier ':' type_annotation '=' expression :
    #variable_definition{name = token_value('$1'), type = '$3', init_value = '$5', line = token_line('$1')}.

variable_definition -> identifier ':' type_annotation :
    #variable_definition{name = token_value('$1'), type = '$3', line = token_line('$1')}.

%% struct definition
struct_definition -> struct identifier variable_definitions 'end' :
    #struct_raw{name = token_value('$2'), fields = '$3', line = token_line('$2')}.

%% function definition
function_definition -> 'fun' identifier '(' variable_definitions ')' ':' type_annotation function_statements 'end' :
    #function_raw{name = token_value('$2'), parameters = '$4', ret_type = '$7', statements = '$8', line = token_line('$2')}.
function_definition -> 'fun' identifier '(' ')' ':' type_annotation function_statements 'end' :
    #function_raw{name = token_value('$2'), parameters = [], ret_type = '$6', statements = '$7', line = token_line('$2')}.
function_definition -> 'fun' identifier '(' variable_definitions ')' function_statements 'end' :
    #function_raw{name = token_value('$2'), parameters = '$4', ret_type = e_util:void_type(token_line('$5')), statements = '$6', line = token_line('$2')}.
function_definition -> 'fun' identifier '(' ')' function_statements 'end' :
    #function_raw{name = token_value('$2'), parameters = [], ret_type = e_util:void_type(token_line('$4')), statements = '$5', line = token_line('$2')}.

%% while
while_statement -> while expression do function_statements 'end' :
    #while_statement{condition = '$2', statements = '$4', line = token_line('$1')}.

%% if
if_statement -> 'if' expression then function_statements else_statement :
    #if_statement{condition = '$2', then = '$4', else = '$5', line = token_line('$1')}.

else_statement -> elif expression then function_statements else_statement :
    [#if_statement{condition = '$2', then = '$4', else = '$5', line = token_line('$1')}].
else_statement -> else function_statements 'end' :
    '$2'.
else_statement -> 'end' :
    [].

%% array_init_expr and struct_init_expr contains similar pattern '{' '}'.
%% make the precedence of array_init_expr higher than struct_init_expr
Unary 2100 array_init_expr.
array_init_expr -> '{' array_init_elements '}' :
    #array_init_expr{elements = '$2', line = token_line('$1')}.
array_init_expr -> '{' string '}' :
    #array_init_expr{elements = str_to_int_tokens('$2'), line = token_line('$1')}.

array_init_elements -> expression ',' array_init_elements : ['$1' | '$3'].
array_init_elements -> expression : ['$1'].

Unary 2000 struct_init_expr.
struct_init_expr -> identifier '{' struct_init_fields '}' :
    #struct_init_expr_raw{name = token_value('$1'), fields = '$3', line = token_line('$1')}.

struct_init_fields -> struct_init_assignment ',' struct_init_fields : ['$1' | '$3'].
struct_init_fields -> struct_init_assignment : ['$1'].

struct_init_assignment -> identifier '=' expression :
    #operator_expression2{operator = assign, operand1 = #variable_reference{name = token_value('$1'), line = token_line('$1')}, operand2 = '$3', line = token_line('$2')}.

%% sizeof
sizeof_expression -> sizeof '(' type_annotation ')' : #sizeof_expression{type = '$3', line = token_line('$2')}.

%% function invocation
call_expr -> expression '(' parameters ')' : #call_expr{fn = '$1', args = '$3', line = token_line('$2')}.
call_expr -> expression '(' ')' : #call_expr{fn = '$1', args = [], line = token_line('$2')}.

assign_expr -> expression op2_with_assignment expression :
    #operator_expression2{operator = assign, operand1 = '$1', operand2 = #operator_expression2{operator = token_symbol('$2'), operand1 = '$1', operand2 = '$3', line = token_line('$2')}, line = token_line('$2')}.
assign_expr -> expression '=' expression :
    #operator_expression2{operator = assign, operand1 = '$1', operand2 = '$3', line = token_line('$2')}.

op2_with_assignment -> op29 '=' : '$1'.
op2_with_assignment -> op28 '=' : '$1'.
op2_with_assignment -> op27 '=' : '$1'.

parameters -> expression ',' parameters : ['$1' | '$3'].
parameters -> expression ',' : ['$1'].
parameters -> expression : ['$1'].

atomic_literal_values -> integer : '$1'.
atomic_literal_values -> float : '$1'.
atomic_literal_values -> string : '$1'.

function_statements -> function_statement function_statements : ['$1' | '$2'].
function_statements -> function_statement : ['$1'].

function_statement -> expression ';' : '$1'.
function_statement -> variable_definition ';' : '$1'.
function_statement -> assign_expr ';' : '$1'.
function_statement -> if_statement : '$1'.
function_statement -> while_statement : '$1'.
function_statement -> goto expression ';' : #goto_statement{expression = '$2', line = token_line('$1')}.
function_statement -> return expression ';' : #return_statement{expression = '$2', line = token_line('$1')}.
function_statement -> goto_label : '$1'.

goto_label -> '@' identifier : #goto_label{name = token_value('$2'), line = token_line('$2')}.

expression -> reserved_keyword : return_error(token_line('$1'), e_util:fmt("~s is reserved keyword", [token_symbol('$1')])).
expression -> expression op30 expression : #operator_expression2{operator = token_symbol('$2'), operand1 = '$1', operand2 = '$3', line=token_line('$2')}.
expression -> expression op29 expression : #operator_expression2{operator = token_symbol('$2'), operand1 = '$1', operand2 = '$3', line=token_line('$2')}.
expression -> expression op28 expression : #operator_expression2{operator = token_symbol('$2'), operand1 = '$1', operand2 = '$3', line = token_line('$2')}.
expression -> expression op27 expression : #operator_expression2{operator = token_symbol('$2'), operand1 = '$1', operand2 = '$3', line = token_line('$2')}.
expression -> expression op26 expression : #operator_expression2{operator = token_symbol('$2'), operand1 = '$1', operand2 = '$3', line = token_line('$2')}.
expression -> expression op25 expression : #operator_expression2{operator = token_symbol('$2'), operand1 = '$1', operand2 = '$3', line = token_line('$2')}.
expression -> expression op19 : #operator_expression1{operator = token_symbol('$2'), operand = '$1', line = token_line('$2')}.
expression -> identifier : #variable_reference{name = token_value('$1'), line = token_line('$1')}.
expression -> pre_minus_plus_expr : '$1'.
expression -> array_init_expr : '$1'.
expression -> struct_init_expr : '$1'.
expression -> atomic_literal_values : '$1'.
expression -> call_expr : '$1'.
expression -> sizeof_expression : '$1'.
expression -> '(' expression ')' : '$2'.
expression -> '(' expression as type_annotation ')' : #type_convert{expression = '$2', type = '$4', line = token_line('$3')}.

reserved_keyword -> new : '$1'.
reserved_keyword -> 'cond' : '$1'.
reserved_keyword -> 'case' : '$1'.
reserved_keyword -> for : '$1'.
reserved_keyword -> break : '$1'.
reserved_keyword -> continue : '$1'.

%% the precedence of 'pre_minus_plus_expr' needs to be higher than "operator_expression2 +/-"
Unary 300 pre_minus_plus_expr.
pre_minus_plus_expr -> '-' expression : #operator_expression1{operator = token_symbol('$1'), operand = '$2', line = token_line('$1')}.
pre_minus_plus_expr -> '+' expression : #operator_expression1{operator = token_symbol('$1'), operand = '$2', line = token_line('$1')}.

Unary 900 op19.
op19 -> '^' : '$1'.
op19 -> '@' : '$1'.
op19 -> '!' : '$1'.
op19 -> '~' : '$1'.

Left 1000 op30.
op30 -> '.' : '$1'.

Left 290 op29.
op29 -> '*' : '$1'.
op29 -> '/' : '$1'.
op29 -> 'rem' : '$1'.

Left 280 op28.
op28 -> '+' : '$1'.
op28 -> '-' : '$1'.

Left 270 op27.
op27 -> 'bsl' : '$1'.
op27 -> 'bsr' : '$1'.
op27 -> 'band' : '$1'.
op27 -> 'bor' : '$1'.
op27 -> 'bxor' : '$1'.

Nonassoc 260 op26.
op26 -> '==' : '$1'.
op26 -> '!=' : '$1'.
op26 -> '>=' : '$1'.
op26 -> '<=' : '$1'.
op26 -> '>' : '$1'.
op26 -> '<' : '$1'.

Left 250 op25.
op25 -> 'and' : '$1'.
op25 -> 'or' : '$1'.


Erlang code.

-include("e_record_definition.hrl").

str_to_int_tokens({string, Line, Str}) ->
    lists:map(fun (Char) -> {integer, Line, Char} end, Str).

token_value({_, _, Val}) ->
    Val.

token_symbol({Sym, _}) ->
    Sym.

token_line(T) ->
    element(2, T).
