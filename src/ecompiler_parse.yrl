Nonterminals

statements statement defconst defstruct defun defvars defvar params
exprs expr root_expr call_expr if_expr else_expr while_expr preminusplus_expr
return_expr sizeof_expr assign_expr label_expr goto_expr
op19 op30 op29 op28 op27 op26 op25 op2_withassign
typeanno_list typeanno pointer_depth atomic_literal
array_init_expr array_init_elements struct_init_expr struct_init_fields
struct_init_assign reserved_keyword

.

Terminals

%% operators
',' '::' ':' ';' '=' '{' '}' '(' ')' '<' '>' '+' '-' '*' '/' '^' '@' '.' '~'
'!' '!=' '==' '>=' '<='
%% keywords
const struct 'end' 'fun' 'rem' 'and' 'or' 'band' 'bor' 'bxor' 'bsl' 'bsr'
while 'if' elif else return sizeof goto

%% reserved keywords
'cond' 'case' for break continue

%%
identifier integer float string integer_type float_type void_type any_type

.

Rootsymbol statements.

statements -> statement statements : ['$1' | '$2'].
statements -> statement : ['$1'].

statement -> defconst : '$1'.
statement -> defstruct : '$1'.
statement -> defun : '$1'.
statement -> defvar ';' : '$1'.

defconst -> const identifier '=' expr ';' :
    #const{name=tok_val('$2'), val='$4', line=tok_line('$2')}.

%% type annotation inside array or function
typeanno_list -> typeanno ',' typeanno_list : ['$1' | '$3'].
typeanno_list -> typeanno : ['$1'].

typeanno -> 'fun' '(' typeanno_list ')' ':' typeanno :
    #fun_type{params='$3', ret='$6', line=tok_line('$1')}.
typeanno -> 'fun' '(' typeanno_list ')' :
    #fun_type{params='$3', ret=void_type(tok_line('$4')), line=tok_line('$1')}.
typeanno -> 'fun' '(' ')' ':' typeanno :
    #fun_type{params=[], ret='$5', line=tok_line('$1')}.
typeanno -> 'fun' '(' ')' :
    #fun_type{params=[], ret=void_type(tok_line('$3')), line=tok_line('$1')}.
typeanno -> '{' typeanno ',' expr '}' :
    #array_type{elemtype='$2', len='$4', line=tok_line('$1')}.
typeanno -> integer_type pointer_depth :
    #basic_type{class=integer, pdepth='$2', tag=tok_val('$1'),
		line=tok_line('$1')}.
typeanno -> integer_type :
    #basic_type{class=integer, pdepth=0, tag=tok_val('$1'),
		line=tok_line('$1')}.
typeanno -> float_type pointer_depth :
    #basic_type{class=float, pdepth='$2', tag=tok_val('$1'),
		line=tok_line('$1')}.
typeanno -> float_type :
    #basic_type{class=float, pdepth=0, tag=tok_val('$1'),
		line=tok_line('$1')}.
typeanno -> identifier pointer_depth :
    #basic_type{class=struct, pdepth='$2', tag=tok_val('$1'),
		line=tok_line('$1')}.
typeanno -> identifier :
    #basic_type{class=struct, pdepth=0, tag=tok_val('$1'),
		line=tok_line('$1')}.
typeanno -> any_type pointer_depth :
    #basic_type{class=any, pdepth='$2', tag=void, line=tok_line('$1')}.
typeanno -> any_type :
    return_error(tok_line('$1'), "type any is not allowed here").
typeanno -> void_type :
    return_error(tok_line('$1'), "type void is not allowed here").

%% pointer depth
pointer_depth -> '^' pointer_depth : '$2' + 1.
pointer_depth -> '^' : 1.

defvars -> defvar ',' defvars : ['$1' | '$3'].
defvars -> defvar ',' : ['$1'].
defvars -> defvar : ['$1'].

defvar -> identifier ':' typeanno '=' expr :
    #vardef{name=tok_val('$1'), type='$3', initval='$5', line=tok_line('$1')}.

defvar -> identifier ':' typeanno :
    #vardef{name=tok_val('$1'), type='$3', line=tok_line('$1')}.

%% struct definition
defstruct -> struct identifier defvars 'end' :
    #struct_raw{name=tok_val('$2'), fields='$3', line=tok_line('$2')}.

%% function definition
defun -> 'fun' identifier '(' defvars ')' ':' typeanno exprs 'end' :
    #function_raw{name=tok_val('$2'), params='$4', ret='$7', exprs='$8',
		  line=tok_line('$2')}.
defun -> 'fun' identifier '(' ')' ':' typeanno exprs 'end' :
    #function_raw{name=tok_val('$2'), params=[], ret='$6', exprs='$7',
		  line=tok_line('$2')}.
defun -> 'fun' identifier '(' defvars ')' exprs 'end' :
    #function_raw{name=tok_val('$2'), params='$4',
		  ret=void_type(tok_line('$5')),
		  exprs='$6', line=tok_line('$2')}.
defun -> 'fun' identifier '(' ')' exprs 'end' :
    #function_raw{name=tok_val('$2'), params=[],
		  ret=void_type(tok_line('$4')),
		  exprs='$5', line=tok_line('$2')}.

%% while
while_expr -> while expr ':' exprs 'end' :
    #while_expr{condition='$2', exprs='$4', line=tok_line('$1')}.

%% if
if_expr -> 'if' expr ':' exprs else_expr :
    #if_expr{condition='$2', then='$4', else='$5', line=tok_line('$1')}.

else_expr -> elif expr ':' exprs else_expr :
    [#if_expr{condition='$2', then='$4', else='$5', line=tok_line('$1')}].
else_expr -> else ':' exprs 'end' :
    '$3'.
else_expr -> 'end' :
    [].

%% array_init_expr and struct_init_expr contains similar pattern '{' '}'.
%% make the precedence of array_init_expr higher than struct_init_expr
Unary 2100 array_init_expr.
array_init_expr -> '{' array_init_elements '}' :
    #array_init{elements='$2', line=tok_line('$1')}.
array_init_expr -> '{' string '}' :
    #array_init{elements=str_to_inttks('$2'), line=tok_line('$1')}.

array_init_elements -> expr ',' array_init_elements :
    ['$1' | '$3'].
array_init_elements -> expr :
    ['$1'].

Unary 2000 struct_init_expr.
struct_init_expr -> identifier '{' struct_init_fields '}' :
    #struct_init_raw{name=tok_val('$1'), fields='$3', line=tok_line('$1')}.

struct_init_fields -> struct_init_assign ',' struct_init_fields :
    ['$1' | '$3'].
struct_init_fields -> struct_init_assign :
    ['$1'].

struct_init_assign -> identifier '=' expr :
    #op2{operator=assign, op1=#varref{name=tok_val('$1'),
				      line=tok_line('$1')},
	 op2='$3', line=tok_line('$2')}.

%% return
return_expr -> return expr :
    #return{expr='$2', line=tok_line('$1')}.

%% goto
goto_expr -> goto expr :
    #goto{expr='$2', line=tok_line('$1')}.

%% sizeof
sizeof_expr -> sizeof '(' typeanno ')' :
    #sizeof{type='$3', line=tok_line('$2')}.

%% function invocation
call_expr -> expr '(' params ')' :
    #call{fn='$1', args='$3', line=tok_line('$2')}.
call_expr -> expr '(' ')' :
    #call{fn='$1', args=[], line=tok_line('$2')}.

assign_expr -> expr op2_withassign expr :
    #op2{operator=assign, op1='$1', op2=#op2{operator=tok_sym('$2'), op1='$1',
					     op2='$3', line=tok_line('$2')},
	 line=tok_line('$2')}.
assign_expr -> expr '=' expr :
    #op2{operator=assign, op1='$1', op2='$3', line=tok_line('$2')}.

op2_withassign -> op29 '=' : '$1'.
op2_withassign -> op28 '=' : '$1'.
op2_withassign -> op27 '=' : '$1'.

params -> expr ',' params : ['$1' | '$3'].
params -> expr ',' : ['$1'].
params -> expr : ['$1'].

atomic_literal -> integer : '$1'.
atomic_literal -> float : '$1'.
atomic_literal -> string : '$1'.

label_expr -> '@' '@' identifier ':' :
    #label{name=tok_val('$3'), line=tok_line('$3')}.

%% expression
exprs -> root_expr exprs : ['$1' | '$2'].
exprs -> root_expr : ['$1'].

root_expr -> defvar ';' : '$1'.
root_expr -> expr ';' : '$1'.
root_expr -> assign_expr ';' : '$1'.
root_expr -> return_expr ';' : '$1'.
root_expr -> goto_expr ';' : '$1'.
root_expr -> if_expr : '$1'.
root_expr -> while_expr : '$1'.
root_expr -> label_expr : '$1'.

expr -> reserved_keyword :
    return_error(tok_line('$1'), flat_format("~s is reserved keyword",
					     [tok_sym('$1')])).
expr -> expr op30 expr :
    #op2{operator=tok_sym('$2'), op1='$1', op2='$3', line=tok_line('$2')}.
expr -> expr op29 expr :
    #op2{operator=tok_sym('$2'), op1='$1', op2='$3', line=tok_line('$2')}.
expr -> expr op28 expr :
    #op2{operator=tok_sym('$2'), op1='$1', op2='$3', line=tok_line('$2')}.
expr -> expr op27 expr :
    #op2{operator=tok_sym('$2'), op1='$1', op2='$3', line=tok_line('$2')}.
expr -> expr op26 expr :
    #op2{operator=tok_sym('$2'), op1='$1', op2='$3', line=tok_line('$2')}.
expr -> expr op25 expr :
    #op2{operator=tok_sym('$2'), op1='$1', op2='$3', line=tok_line('$2')}.
expr -> expr op19 :
    #op1{operator=tok_sym('$2'), operand='$1', line=tok_line('$2')}.
expr -> identifier :
    #varref{name=tok_val('$1'), line=tok_line('$1')}.
expr -> preminusplus_expr : '$1'.
expr -> array_init_expr : '$1'.
expr -> struct_init_expr : '$1'.
expr -> atomic_literal : '$1'.
expr -> call_expr : '$1'.
expr -> sizeof_expr : '$1'.
expr -> '(' expr ')' : '$2'.

reserved_keyword -> 'cond' : '$1'.
reserved_keyword -> 'case' : '$1'.
reserved_keyword -> for : '$1'.
reserved_keyword -> break : '$1'.
reserved_keyword -> continue : '$1'.

%% the precedence of 'preminusplus_expr' needs to be higher than "op2 -"
Unary 300 preminusplus_expr.
preminusplus_expr -> '-' expr :
    #op1{operator=tok_sym('$1'), operand='$2', line=tok_line('$1')}.
preminusplus_expr -> '+' expr :
    #op1{operator=tok_sym('$1'), operand='$2', line=tok_line('$1')}.

Unary 900 op19.
op19 -> '^' : '$1'.
op19 -> '@' : '$1'.
op19 -> '!' : '$1'.
op19 -> '~' : '$1'.

Left 1000 op30.
op30 -> '.' : '$1'.
op30 -> '::' : '$1'.

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

-include("./ecompiler_frame.hrl").

-import(ecompiler_utils, [void_type/1, flat_format/2]).

str_to_inttks({string, Line, Str}) ->
    lists:map(fun(Char) -> {integer, Line, Char} end, Str).

tok_val({_, _, Val}) -> Val.
tok_sym({Sym, _}) -> Sym.
tok_line(T) -> element(2, T).

