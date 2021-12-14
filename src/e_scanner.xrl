Definitions.

Delim = [@^.~,;:#+\-*/(){}?]|>=|<=|==|!=|!|>|<|=
Identifier = [_a-zA-Z][_a-zA-Z0-9]*
StrQuote = "
StrUnescapedChar = [^\"\\]
CharQuote = '
CharUnescapedChar = [^\'\\]
CommonEscapedChar = \\\\|\\b|\\f|\\n|\\r|\\t|\\/
StrEscapedChar = ({CommonEscapedChar}|\\")
CharEscapedChar = ({CommonEscapedChar}|\\')
BinaryDigit = [01]
OctallDigit = [0-7]
DecimalDigit = [0-9]
HexDigit = [0-9a-f]
CommentStart = %

Rules.

{StrQuote}{StrQuote} : {token, {string, TokenLine, ""}}.
{StrQuote}({StrUnescapedChar}|{StrEscapedChar})+{StrQuote} : {token, {string, TokenLine, fix_str(drop_quotes(TokenChars))}}.
{CharQuote}{CharQuote} : {error, "empty char"}.
{CharQuote}({CharUnescapedChar}|{CharEscapedChar}){CharQuote} : {token, {integer, TokenLine, fix_char(drop_quotes(TokenChars))}}.
0x{HexDigit}+ : {token, {integer, TokenLine, str_to_int(TokenChars, 16)}}.
0o{OctallDigit}+ : {token, {integer, TokenLine, str_to_int(TokenChars, 8)}}.
0b{BinaryDigit}+ : {token, {integer, TokenLine, str_to_int(TokenChars, 2)}}.
{DecimalDigit}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{DecimalDigit}+\.{DecimalDigit}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{DecimalDigit}\.{DecimalDigit}+e{DecimalDigit}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{Delim} : {token, {list_to_atom(TokenChars), TokenLine}}.
struct|end|fun|return|if|then|elif|else|while|do|goto|sizeof|as|new : {token, {list_to_atom(TokenChars), TokenLine}}.
rem|and|or|band|bor|bxor|bsl|bsr : {token, {list_to_atom(TokenChars), TokenLine}}.
cond|case|for|break|continue : {token, {list_to_atom(TokenChars), TokenLine}}.
u8|i8|u16|i16|u32|i32|u64|i64|usize|isize : {token, {integerType, TokenLine, list_to_atom(TokenChars)}}.
f64|f32 : {token, {floatType, TokenLine, list_to_atom(TokenChars)}}.
void : {token, {void_type, TokenLine, void}}.
any : {token, {anyType, TokenLine, any}}.
{Identifier} : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
[\s\r\t\v\f]* : skip_token.
\n : {token, {newline, TokenLine}}.
{CommentStart}[^\n]* : skip_token.

Erlang code.

str_to_int([$0, _ | Chars], Base) ->
    list_to_integer(Chars, Base).

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
