Nonterminals

statements statement defconst defstruct defun defvars defvar varref params
exprs expr call_expr if_expr else_expr while_expr preminusplus_expr
return_expr expr_or_defvar
op19 op30 op29 op28 op27 op26 op25
typeanno_list typeanno pointer_depth general_type atomic_literal constref
.

Terminals

%% operators
',' ':' ';' '=' '<' '>' '(' ')' '+' '-' '*' '/' '^' '@' '.' '~' '!'
'!=' '==' '>=' '<='
%% keywords
const struct 'end' 'fun' 'rem' 'and' 'or' 'band' 'bor' 'bxor' 'bsl' 'bsr'
while 'if' elif else return
%%
identifier integer float string basic_type
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

constref -> atomic_literal : tok_val('$1').
constref -> identifier : {constref, tok_line('$1'), tok_val('$1')}.

atomic_literal -> integer : '$1'.
atomic_literal -> float : '$1'.
atomic_literal -> string : '$1'.

varref -> identifier :
    #varref{name=tok_val('$1'), line=tok_line('$1')}.

%% type annotation inside box or function
typeanno_list -> typeanno ',' typeanno_list : ['$1' | '$3'].
typeanno_list -> typeanno : ['$1'].

typeanno -> 'fun' '(' typeanno_list ')' ':' typeanno :
    #fun_type{params='$3', ret='$6', line=tok_line('$1')}.

typeanno -> '<' typeanno ',' constref '>' :
    #box_type{elemtype='$2', size='$4', line=tok_line('$1')}.

typeanno -> general_type pointer_depth :
    #basic_type{type={tok_val('$1'), '$2'}, line=tok_line('$1')}.

typeanno -> general_type :
    #basic_type{type={tok_val('$1'), 0}, line=tok_line('$1')}.

general_type -> basic_type : '$1'.
general_type -> identifier : '$1'.

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

%% function invocation
call_expr -> identifier '(' params ')' :
    #call{fn=#varref{name=tok_val('$1'), line=tok_line('$1')},
	  args='$3', line=tok_line('$2')}.

params -> expr ',' params : ['$1' | '$3'].
params -> expr ',' : ['$1'].
params -> expr : ['$1'].

%% while
while_expr -> while '(' expr ')' exprs 'end' :
    #while_expr{condition='$3', exprs='$5', line=tok_line('$1')}.

%% if
if_expr -> 'if' '(' expr ')' exprs else_expr :
    #if_expr{condition='$3', then='$5', else='$6', line=tok_line('$1')}.

else_expr -> elif '(' expr ')' exprs else_expr :
    [#if_expr{condition='$3', then='$5', else='$6', line=tok_line('$1')}].
else_expr -> else exprs 'end' :
    '$2'.
else_expr -> 'end' :
    [].

return_expr -> return expr :
    #return{expr='$2', line=tok_line('$1')}.

%% expression
exprs -> expr_or_defvar ';' exprs : ['$1' | '$3'].
exprs -> expr_or_defvar ';' : ['$1'].
exprs -> while_expr exprs : ['$1' | '$2'].
exprs -> if_expr exprs : ['$1' | '$2'].
exprs -> while_expr : ['$1'].
exprs -> if_expr : ['$1'].

expr_or_defvar -> defvar : '$1'.
expr_or_defvar -> expr : '$1'.

expr -> return_expr : '$1'.
expr -> '(' expr ')' : '$2'.
expr -> atomic_literal : '$1'.
expr -> varref : '$1'.
expr -> call_expr : '$1'.
expr -> preminusplus_expr : '$1'.
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
expr -> expr '=' expr :
    #op2{operator=assign, op1='$1', op2='$3', line=tok_line('$2')}.

Unary 800 preminusplus_expr.
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

Right 100 '='.


Erlang code.

-include("./ecompiler_frame.hrl").

tok_val({_, _, Val}) -> Val.
tok_sym({Sym, _}) -> Sym.
tok_line(T) -> element(2, T).

