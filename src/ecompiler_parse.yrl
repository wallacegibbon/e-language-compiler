Nonterminals

struct pairs pair atomic_literal variable single_expression atomic_literal
.

Terminals

identifier kw_struct kw_end newline ',' ':' '=' typeanno expr
integer float string
.

Rootsymbol struct.

struct -> kw_struct identifier newline pairs kw_end :
    #struct{name=tok_val('$2'), fields='$4', line=tok_line('$2')}.

pairs -> pair : ['$1'].
pairs -> pair ',' pairs : ['$1' | '$3'].

pair -> identifier ':' typeanno '=' expr : {tok_val('$1'), '$3', '$5'}.
pair -> identifier ':' typeanno : {tok_val('$1'), '$3'}.

atomic_literal -> integer : #integer{val=tok_val('$1'),
				     line=tok_line('$1')}.

atomic_literal -> float : #float{val=tok_val('$1'),
				 line=tok_line('$1')}.

atomic_literal -> string : #string{val=tok_val('$1'),
				   line=tok_line('$1')}.

variable -> identifier : #var{val=tok_val('$1'),
			      line=tok_line('$1')}.

single_expression -> atomic_literal : '$1'.
single_expression -> variable : '$1'.

Erlang code.

-include("./ecompiler_frame.hrl").

tok_val({_, _, Val}) -> Val.
tok_line(T) -> element(2, T).

