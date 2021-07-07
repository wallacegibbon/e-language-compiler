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
    %{token,{comment,TokenLine,tl(TokenChars)}}.
    skip_token.

{StrQuote}{StrQuote} :
    {token, {string, TokenLine, ""}}.

{StrQuote}({StrUnescapedChar}|{StrEscapedChar})+{StrQuote} :
    {token, {string, TokenLine, fixCharacters(dropQuotes(TokenChars))}}.

{CharQuote}{CharQuote} :
    {error, "empty char"}.

{CharQuote}({CharUnescapedChar}|{CharEscapedChar}){CharQuote} :
    {token, {integer, TokenLine, fixCharacter(dropQuotes(TokenChars))}}.

0x{HexDigit}+ :
    {token, {integer, TokenLine, stringToInteger(TokenChars, 16)}}.
0o{OctallDigit}+ :
    {token, {integer, TokenLine, stringToInteger(TokenChars, 8)}}.
0b{BinaryDigit}+ :
    {token, {integer, TokenLine, stringToInteger(TokenChars, 2)}}.
{DecimalDigit}+ :
    {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

{DecimalDigit}+\.{DecimalDigit}+ :
    {token, {float, TokenLine, list_to_float(TokenChars)}}.

{Delim} :
    {token, {list_to_atom(TokenChars), TokenLine}}.

const|struct|end|fun|return|if|then|elif|else|while|do|goto|sizeof :
    {token, {list_to_atom(TokenChars), TokenLine}}.

rem|and|or|band|bor|bxor|bsl|bsr :
    {token, {list_to_atom(TokenChars), TokenLine}}.

cond|case|for|break|continue :
    {token, {list_to_atom(TokenChars), TokenLine}}.

u8|i8|u16|i16|u32|i32|u64|i64|usize|isize :
    {token, {integerType, TokenLine, list_to_atom(TokenChars)}}.

f64|f32 :
    {token, {floatType, TokenLine, list_to_atom(TokenChars)}}.

void :
    {token, {voidType, TokenLine, void}}.

any :
    {token, {anyType, TokenLine, any}}.

{Identifier} :
    {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.

[\s\r\t\v\f\n]* :
    skip_token.


Erlang code.

stringToInteger([$0, _ | Chars], Base) -> list_to_integer(Chars, Base).

dropQuotes([_ | QuotedString]) -> lists:droplast(QuotedString).

fixCharacters([$\\, $\\ | Rest]) -> [$\\ | fixCharacters(Rest)];
fixCharacters([$\\, $/ | Rest]) -> [$/ | fixCharacters(Rest)];
fixCharacters([$\\, $b | Rest]) -> [$\b | fixCharacters(Rest)];
fixCharacters([$\\, $f | Rest]) -> [$\f | fixCharacters(Rest)];
fixCharacters([$\\, $n | Rest]) -> [$\n | fixCharacters(Rest)];
fixCharacters([$\\, $r | Rest]) -> [$\r | fixCharacters(Rest)];
fixCharacters([$\\, $t | Rest]) -> [$\t | fixCharacters(Rest)];
fixCharacters([$\\, $" | Rest]) -> [$" | fixCharacters(Rest)];
fixCharacters([$\\, Any | Rest]) -> [Any | fixCharacters(Rest)];
fixCharacters([C | Rest]) -> [C | fixCharacters(Rest)];
fixCharacters([]) -> [].

fixCharacter([$\\, $\\]) -> $\\;
fixCharacter([$\\, $b]) -> $\b;
fixCharacter([$\\, $f]) -> $\f;
fixCharacter([$\\, $n]) -> $\n;
fixCharacter([$\\, $r]) -> $\r;
fixCharacter([$\\, $t]) -> $\t;
fixCharacter([$\\, Any]) -> Any;
fixCharacter([Any]) -> Any.

