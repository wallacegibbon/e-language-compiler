Definitions.

Delim = [@^.~,;+\-*/(){}]|>=|<=|==|!=|!|>|<|=|::|:
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

{CommentStart}[^\n]* :
    %{token, {comment, TokenLine, tl(TokenChars)}}.
    skip_token.

{StrQuote}{StrQuote} :
    {token, {string, TokenLine, ""}}.

{StrQuote}({StrUnescapedChar}|{StrEscapedChar})+{StrQuote} :
    {token, {string, TokenLine, fixchars(drop_quotes(TokenChars))}}.

{CharQuote}{CharQuote} :
    {error, "empty char"}.

{CharQuote}({CharUnescapedChar}|{CharEscapedChar}){CharQuote} :
    {token, {integer, TokenLine, fixchar(drop_quotes(TokenChars))}}.

0x{HexDigit}+ :
    {token, {integer, TokenLine, str_to_integer(TokenChars, 16)}}.
0o{OctallDigit}+ :
    {token, {integer, TokenLine, str_to_integer(TokenChars, 8)}}.
0b{BinaryDigit}+ :
    {token, {integer, TokenLine, str_to_integer(TokenChars, 2)}}.
{DecimalDigit}+ :
    {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

{DecimalDigit}+\.{DecimalDigit}+ :
    {token, {float, TokenLine, list_to_float(TokenChars)}}.

{Delim} :
    {token, {list_to_atom(TokenChars), TokenLine}}.

const|struct|end|fun|return|if|elif|else|while|rem|and|or|band|bor|bxor|bsl|bsr :
    {token, {list_to_atom(TokenChars), TokenLine}}.

u8|i8|u16|i16|u32|i32|u64|i64|f64|f32 :
    {token, {basic_type, TokenLine, list_to_atom(TokenChars)}}.

void :
    {token, {void_type, TokenLine, void}}.

any :
    {token, {any_type, TokenLine, any}}.

{Identifier} :
    {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.

[\s\r\t\v\f\n]* :
    skip_token.


Erlang code.

str_to_integer([$0, _ | Chars], Base) ->
    list_to_integer(Chars, Base).

drop_quotes([_ | QuotedString]) ->
    lists:droplast(QuotedString).

fixchars([$\\, $\\ | Rest]) ->
    [$\\ | fixchars(Rest)];
fixchars([$\\, $/ | Rest]) ->
    [$/ | fixchars(Rest)];
fixchars([$\\, $b | Rest]) ->
    [$\b | fixchars(Rest)];
fixchars([$\\, $f | Rest]) ->
    [$\f | fixchars(Rest)];
fixchars([$\\, $n | Rest]) ->
    [$\n | fixchars(Rest)];
fixchars([$\\, $r | Rest]) ->
    [$\r | fixchars(Rest)];
fixchars([$\\, $t | Rest]) ->
    [$\t | fixchars(Rest)];
fixchars([$\\, $" | Rest]) ->
    [$" | fixchars(Rest)];
fixchars([$\\, Any | Rest]) ->
    [Any | fixchars(Rest)];
fixchars([C | Rest]) ->
    [C | fixchars(Rest)];
fixchars([]) ->
    [].

fixchar([$\\, $\\]) -> $\\;
fixchar([$\\, $b]) -> $\b;
fixchar([$\\, $f]) -> $\f;
fixchar([$\\, $n]) -> $\n;
fixchar([$\\, $r]) -> $\r;
fixchar([$\\, $t]) -> $\t;
fixchar([$\\, Any]) -> Any;
fixchar([Any]) -> Any.

