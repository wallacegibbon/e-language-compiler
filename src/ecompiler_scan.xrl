Definitions.

StrQuote = "
StrUnescapedChar = [^\"\\]
CharQuote = '
CharUnescapedChar = [^\'\\]
CommonEscapedChar = (\\\\)|(\\b)|(\\f)|(\\n)|(\\r)|(\\t)|(\\/)
StrEscapedChar = {CommonEscapedChar}|(\\")
CharEscapedChar = {CommonEscapedChar}|(\\')
Delim = ([@^.+\-*/~:,])|(>=)|(<=)|(==)|(>)|(<)|(=)
BinaryDigit = [01]
OctallDigit = [0-7]
DecimalDigit = [0-9]
HexDigit = [0-9a-f]


Rules.

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

{Delim} :
    {token, {list_to_atom(TokenChars), TokenLine}}.

\n :
    {token, {newline, TokenLine}}.
[\r\t\s]* :
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
fixchars([$\\, Any | _]) ->
    throw(io_lib:format("unknown escape char ~w", [Any]));
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
fixchar([$\\, $']) -> $\';
fixchar([$\\, Any]) ->
    throw(io_lib:format("unknown escape char ~w", [Any]));
fixchar([Any]) -> Any.


