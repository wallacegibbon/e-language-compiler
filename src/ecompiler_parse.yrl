Nonterminals

vardefs vardef defconst defstruct defun variable args arg
exprs expr call_expr if_expr else_expr while_expr
op1 op29 op28 op27 op26
typeanno pointer_depth general_type atomic_literal constant
.

Terminals

%% operators
',' ':' ';' '=' '<' '>' '(' ')' '+' '-' '*' '/' '^' '@' '.' '~' '!'
'!=' '==' '>=' '<='
%% keywords
const struct 'end' 'fun' 'rem' 'and' 'or' 'band' 'bor' 'bxor' 'bsl' 'bsr'
while 'if' elif else
%%
identifier integer float string single_type
.

%Rootsymbol defstruct.
Rootsymbol defun.

defconst -> const identifier '=' expr :
    #const{name=tok_val('$2'), val='$4', line=tok_line('$2')}.

constant -> atomic_literal : tok_val('$1').
constant -> identifier : {eval, '$1'}.

atomic_literal -> integer : '$1'.
atomic_literal -> float : '$1'.
atomic_literal -> string : '$1'.

variable -> identifier :
    #var{name=tok_val('$1'), line=tok_line('$1')}.

%% type annotation inside box or function
typeanno -> '<' typeanno ',' constant '>' :
    {box_type, tok_line('$2'), '$4', element(3, '$2')}.

typeanno -> general_type pointer_depth :
    {single_type, tok_line('$1'), {tok_val('$1'), '$2'}}.

typeanno -> general_type :
    {single_type, tok_line('$1'), tok_val('$1')}.

general_type -> single_type : '$1'.
general_type -> identifier : '$1'.

%% pointer depth
pointer_depth -> '^' pointer_depth : '$2' + 1.
pointer_depth -> '^' : 1.

vardefs -> vardef ',' vardefs : ['$1' | '$3'].
vardefs -> vardef ',' : ['$1'].
vardefs -> vardef : ['$1'].

vardef -> identifier ':' typeanno '=' expr :
    {tok_val('$1'), '$3', '$5'}.

vardef -> identifier ':' typeanno :
    {tok_val('$1'), '$3'}.

%% struct definition
defstruct -> struct identifier vardefs 'end' :
    #struct{name=tok_val('$2'), fields='$3', line=tok_line('$2')}.

%% function definition
defun -> 'fun' identifier '(' vardefs ')' ':' typeanno exprs 'end' :
    #function{name=tok_val('$2'), args='$4', ret='$7', exprs='$8',
	      line=tok_line('$2')}.

%% function invocation
call_expr -> identifier '(' args ')' :
    {call, '$1', '$3'}.

args -> arg ',' args : ['$1' | '$3'].
args -> arg ',' : ['$1'].
args -> arg : ['$1'].

arg -> variable : '$1'.
arg -> atomic_literal : '$1'.

%% while
while_expr -> while expr exprs 'end' :
    #while_expr{condition='$2', exprs='$3', line=tok_line('$1')}.

%% if
if_expr -> 'if' expr exprs else_expr :
    #if_expr{condition='$2', then='$3', else='$4', line=tok_line('$1')}.

else_expr -> elif expr exprs else_expr :
    [#if_expr{condition='$2', then='$3', else='$4', line=tok_line('$1')}].
else_expr -> else exprs 'end' :
    '$2'.
else_expr -> 'end' :
    [].

%% expression
exprs -> expr ';' exprs : ['$1' | '$3'].
exprs -> while_expr exprs : ['$1' | '$2'].
exprs -> if_expr exprs : ['$1' | '$2'].
exprs -> while_expr : ['$1'].
exprs -> if_expr : ['$1'].
exprs -> expr ';' : ['$1'].

expr -> '(' expr ')' : '$2'.
expr -> atomic_literal : '$1'.
expr -> variable : '$1'.
expr -> call_expr : '$1'.
expr -> expr op29 expr :
    {op, tok_line('$2'), tok_sym('$2'), '$1', '$3'}.
expr -> expr op28 expr :
    {op, tok_line('$2'), tok_sym('$2'), '$1', '$3'}.
expr -> expr op27 expr :
    {op, tok_line('$2'), tok_sym('$2'), '$1', '$3'}.
expr -> expr op26 expr :
    {op, tok_line('$2'), tok_sym('$2'), '$1', '$3'}.
expr -> expr op1 :
    {op, tok_line('$2'), tok_sym('$2'), '$1'}.
expr -> expr ':' typeanno '=' expr :
    {op, tok_line('$4'), vardef, '$1', '$5', '$3'}.
expr -> expr '=' expr :
    {op, tok_line('$2'), assign, '$1', '$3'}.

Unary 900 op1.
op1 -> '^' : '$1'.
op1 -> '@' : '$1'.
op1 -> '.' : '$1'.
op1 -> '!' : '$1'.
op1 -> '~' : '$1'.

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
op27 -> 'and' : '$1'.
op27 -> 'or' : '$1'.

Nonassoc 260 op26.
op26 -> '==' : '$1'.
op26 -> '!=' : '$1'.
op26 -> '>=' : '$1'.
op26 -> '<=' : '$1'.
op26 -> '>' : '$1'.
op26 -> '<' : '$1'.

Right 100 '='.


Erlang code.

-include("./ecompiler_frame.hrl").

tok_val({_, _, Val}) -> Val.
tok_sym({Sym, _}) -> Sym.
tok_line(T) -> element(2, T).

