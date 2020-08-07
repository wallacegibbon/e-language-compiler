Nonterminals

defvars defvar defbox variable expr atomic_literal constant
typeanno type_extra general_type
.

Terminals

identifier kw_defbox kw_as ',' '.' ':' '=' '^' '<' '>'
integer float string single_type custom_type
.

Rootsymbol defbox.

constant -> atomic_literal : tok_val('$1').
constant -> identifier : {eval, '$1'}.

atomic_literal -> integer : '$1'.
atomic_literal -> float : '$1'.
atomic_literal -> string : '$1'.

expr -> atomic_literal : '$1'.
expr -> variable : '$1'.

variable -> identifier : {var, tok_line('$1'), tok_val('$1')}.

%% The type system

%% type annotation inside box or function
typeanno -> '<' typeanno ',' constant '>' :
    {box_type, tok_line('$2'), '$4', element(3, '$2')}.

typeanno -> general_type type_extra :
    {single_type, tok_line('$1'), {tok_val('$1'), '$2'}}.

typeanno -> general_type :
    {single_type, tok_line('$1'), tok_val('$1')}.

general_type -> single_type : '$1'.
general_type -> custom_type : '$1'.

%% pointer depth
type_extra -> '^' type_extra : '$2' + 1.
type_extra -> '^' : 1.

%% "box" is like "struct" in C language
defbox -> kw_defbox custom_type kw_as defvars '.' :
    #box{name=tok_val('$2'), fields='$4', line=tok_line('$2')}.

defvars -> defvar ',' defvars : ['$1' | '$3'].
defvars -> defvar : ['$1'].

defvar -> identifier ':' typeanno '=' expr :
    {tok_val('$1'), '$3', '$5'}.

defvar -> identifier ':' typeanno :
    {tok_val('$1'), '$3'}.


Erlang code.

-include("./ecompiler_frame.hrl").

tok_val({_, _, Val}) -> Val.
tok_line(T) -> element(2, T).

