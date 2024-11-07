Definitions.

Delimiter = [@^.~,;:#+\-*/()\[\]{}?]|>=|<=|==|!=|!|>|<|=
Identifier = [_a-zA-Z][_a-zA-Z0-9]*
StringQuote = "
CharQuote = '
StringUnescapedChar = [^\"\\]
CharUnescapedChar = [^\'\\]
CommonEscapedChar = \\\\|\\b|\\f|\\n|\\r|\\t|\\/
StringEscapedChar = ({CommonEscapedChar}|\\")
CharEscapedChar = ({CommonEscapedChar}|\\')
BinaryDigit = [01_]
OctalDigit = [0-7_]
DecimalDigit = [0-9_]
HexDigit = [0-9A-Fa-f_]
CommentStart = %

Rules.

{StringQuote}{StringQuote} : {token, {string, TokenLoc, ""}}.
{StringQuote}({StringUnescapedChar}|{StringEscapedChar})+{StringQuote} : {token, {string, TokenLoc, fix_str(drop_quotes(TokenChars))}}.
{CharQuote}{CharQuote} : {error, {TokenLoc, "empty char"}}.
{CharQuote}({CharUnescapedChar}|{CharEscapedChar}){CharQuote} : {token, {integer, TokenLoc, fix_char(drop_quotes(TokenChars))}}.
0x{HexDigit}+ : {token, {integer, TokenLoc, str_to_int(TokenChars, 16)}}.
0o{OctalDigit}+ : {token, {integer, TokenLoc, str_to_int(TokenChars, 8)}}.
0b{BinaryDigit}+ : {token, {integer, TokenLoc, str_to_int(TokenChars, 2)}}.
{DecimalDigit}+ : {token, {integer, TokenLoc, list_to_integer(TokenChars)}}.
{DecimalDigit}+\.{DecimalDigit}+ : {token, {float, TokenLoc, list_to_float(TokenChars)}}.
{DecimalDigit}\.{DecimalDigit}+e{DecimalDigit}+ : {token, {float, TokenLoc, list_to_float(TokenChars)}}.
{Delimiter} : {token, {list_to_atom(TokenChars), TokenLoc}}.
struct|end|fn|return|if|then|elif|else|while|do|goto|sizeof|alignof|as|attribute : {token, {list_to_atom(TokenChars), TokenLoc}}.
rem|and|or|not|band|bor|bnot|bxor|bsl|bsr : {token, {list_to_atom(TokenChars), TokenLoc}}.
cond|case|for|break|continue|typeof|new : {token, {list_to_atom(TokenChars), TokenLoc}}.
byte|word : {token, {int_type, TokenLoc, list_to_atom(TokenChars)}}.
float : {token, {float_type, TokenLoc, list_to_atom(TokenChars)}}.
void : {token, {void_type, TokenLoc, void}}.
any : {token, {any_type, TokenLoc, any}}.
{Identifier} : {token, {identifier, TokenLoc, list_to_atom(TokenChars)}}.
[\s\r\t\v\f]* : skip_token.
\n : {token, {newline, TokenLoc}}.
{CommentStart}[^\n]* : skip_token.
. : {error, {TokenLoc, e_util:fmt("invalid char. (code list: ~w)", [TokenChars])}}.

Erlang code.

str_to_int([$0, _ | Chars], Base) ->
	list_to_integer(lists:filter(fun(V) -> V =/= $_ end, Chars), Base).

drop_quotes([_ | QuotedString]) ->
	lists:droplast(QuotedString).

fix_str([$\\, $\\ | Rest]) -> [$\\ | fix_str(Rest)];
fix_str([$\\, $/ | Rest]) -> [$/ | fix_str(Rest)];
fix_str([$\\, $b | Rest]) -> [$\b | fix_str(Rest)];
fix_str([$\\, $f | Rest]) -> [$\f | fix_str(Rest)];
fix_str([$\\, $n | Rest]) -> [$\n | fix_str(Rest)];
fix_str([$\\, $r | Rest]) -> [$\r | fix_str(Rest)];
fix_str([$\\, $t | Rest]) -> [$\t | fix_str(Rest)];
fix_str([$\\, $" | Rest]) -> [$" | fix_str(Rest)];
fix_str([$\\, Any | Rest]) -> [Any | fix_str(Rest)];
fix_str([C | Rest]) -> [C | fix_str(Rest)];
fix_str([]) -> [].

fix_char([$\\, $\\]) -> $\\;
fix_char([$\\, $b]) -> $\b;
fix_char([$\\, $f]) -> $\f;
fix_char([$\\, $n]) -> $\n;
fix_char([$\\, $r]) -> $\r;
fix_char([$\\, $t]) -> $\t;
fix_char([$\\, Any]) -> Any;
fix_char([Any]) -> Any.

