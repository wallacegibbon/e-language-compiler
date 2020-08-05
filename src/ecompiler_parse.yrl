Nonterminals struct pairs pair.

Terminals identifier kw_struct kw_end newline ',' ':' '=' expr typeanno.

Rootsymbol struct.

struct -> kw_struct identifier newline pairs kw_end :
    {struct, element(3, '$2'), '$4'}.

pairs -> pair :
    ['$1'].
pairs -> pair ',' pairs :
    ['$1' | '$3'].

pair -> identifier ':' typeanno :
    {element(3, '$1'), '$3'}.
pair -> identifier ':' typeanno '=' expr :
    {element(3, '$1'), '$3', '$5'}.

Erlang code.

