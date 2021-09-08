Nonterminals

statements statement constDefinition structDefinition functionDefinition variableDefinitions variableDefinition params exprs expr rootExpression callExpression ifExpression elseExpression whileExpression preMinusPlusExpression
returnExpression sizeofExpression assignExpression labelExpression gotoExpression op19 op30 op29 op28 op27 op26 op25 op2WithAssignment typeanno_list typeAnnotation
pointerDepth atomicLiteralValues arrayInitExpression arrayInitElements structInitExpression structInitFields structInitAssignment reservedKeyword

.

Terminals

%% operators
',' '::' ':' ';' '=' '{' '}' '(' ')' '<' '>' '+' '-' '*' '/' '^' '@' '.' '~' '!' '!=' '==' '>=' '<='
%% keywords
const struct 'end' 'fun' 'rem' 'and' 'or' 'band' 'bor' 'bxor' 'bsl' 'bsr' while do 'if' then elif else return sizeof goto

%% reserved keywords
'cond' 'case' for break continue

%%
identifier integer float string integerType floatType voidType anyType

.

Rootsymbol statements.

statements -> statement statements                  : ['$1'|'$2'].
statements -> statement                             : ['$1'].

statement -> constDefinition                        : '$1'.
statement -> structDefinition                       : '$1'.
statement -> functionDefinition                     : '$1'.
statement -> variableDefinition ';'                 : '$1'.

constDefinition -> const identifier '=' expr ';'    : #const{name = tokenValue('$2'), val = '$4', line = tokenLine('$2')}.

%% type annotation inside array or function
typeanno_list -> typeAnnotation ',' typeanno_list   : ['$1'|'$3'].
typeanno_list -> typeAnnotation                     : ['$1'].

typeAnnotation -> 'fun' '(' typeanno_list ')' ':' typeAnnotation :
    #fun_type{params = '$3', ret = '$6', line = tokenLine('$1')}.
typeAnnotation -> 'fun' '(' typeanno_list ')' :
    #fun_type{params = '$3', ret = ecompilerUtil:voidType(tokenLine('$4')), line = tokenLine('$1')}.
typeAnnotation -> 'fun' '(' ')' ':' typeAnnotation :
    #fun_type{params = [], ret = '$5', line = tokenLine('$1')}.
typeAnnotation -> 'fun' '(' ')' :
    #fun_type{params = [], ret = ecompilerUtil:voidType(tokenLine('$3')), line = tokenLine('$1')}.
typeAnnotation -> '{' typeAnnotation ',' expr '}' :
    #array_type{elemtype = '$2', len = '$4', line = tokenLine('$1')}.
typeAnnotation -> integerType pointerDepth :
    #basic_type{class = integer, pdepth = '$2', tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> integerType :
    #basic_type{class = integer, pdepth = 0, tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> floatType pointerDepth :
    #basic_type{class = float, pdepth = '$2', tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> floatType :
    #basic_type{class = float, pdepth = 0, tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> identifier pointerDepth :
    #basic_type{class = struct, pdepth = '$2', tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> identifier :
    #basic_type{class = struct,pdepth = 0, tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> anyType pointerDepth :
    #basic_type{class = any, pdepth = '$2', tag = void, line = tokenLine('$1')}.
typeAnnotation -> anyType :
    return_error(tokenLine('$1'), "type any is not allowed here").
typeAnnotation -> voidType :
    return_error(tokenLine('$1'), "type void is not allowed here").

%% pointer depth
pointerDepth -> '^' pointerDepth                    : '$2' + 1.
pointerDepth -> '^'                                 : 1.

variableDefinitions -> variableDefinition ',' variableDefinitions       : ['$1'|'$3'].
variableDefinitions -> variableDefinition ','                           : ['$1'].
variableDefinitions -> variableDefinition                               : ['$1'].

variableDefinition -> identifier ':' typeAnnotation '=' expr :
    #vardef{name = tokenValue('$1'), type = '$3', initval = '$5', line = tokenLine('$1')}.

variableDefinition -> identifier ':' typeAnnotation :
    #vardef{name = tokenValue('$1'), type = '$3', line = tokenLine('$1')}.

%% struct definition
structDefinition -> struct identifier variableDefinitions 'end' :
    #struct_raw{name = tokenValue('$2'), fields = '$3', line = tokenLine('$2')}.

%% function definition
functionDefinition -> 'fun' identifier '(' variableDefinitions ')' ':' typeAnnotation exprs 'end' :
    #function_raw{name = tokenValue('$2'), params = '$4', ret = '$7', exprs = '$8', line = tokenLine('$2')}.
functionDefinition -> 'fun' identifier '(' ')' ':' typeAnnotation exprs 'end' :
    #function_raw{name = tokenValue('$2'), params = [], ret = '$6', exprs = '$7', line = tokenLine('$2')}.
functionDefinition -> 'fun' identifier '(' variableDefinitions ')' exprs 'end' :
    #function_raw{name = tokenValue('$2'), params = '$4', ret = ecompilerUtil:voidType(tokenLine('$5')), exprs = '$6', line = tokenLine('$2')}.
functionDefinition -> 'fun' identifier '(' ')' exprs 'end' :
    #function_raw{name = tokenValue('$2'), params = [], ret = ecompilerUtil:voidType(tokenLine('$4')), exprs = '$5', line = tokenLine('$2')}.

%% while
whileExpression -> while expr do exprs 'end'            : #while_expr{condition = '$2', exprs = '$4', line = tokenLine('$1')}.

%% if
ifExpression -> 'if' expr then exprs elseExpression     : #if_expr{condition = '$2', then = '$4', else = '$5', line = tokenLine('$1')}.

elseExpression -> elif expr then exprs elseExpression   : [#if_expr{condition = '$2', then = '$4', else = '$5', line = tokenLine('$1')}].
elseExpression -> else exprs 'end'                      : '$2'.
elseExpression -> 'end'                                 : [].

%% arrayInitExpression and structInitExpression contains similar pattern '{' '}'.
%% make the precedence of arrayInitExpression higher than structInitExpression
Unary 2100 arrayInitExpression.
arrayInitExpression -> '{' arrayInitElements '}'    : #array_init{elements = '$2', line = tokenLine('$1')}.
arrayInitExpression -> '{' string '}'               : #array_init{elements = stringToIntegerTokens('$2'), line = tokenLine('$1')}.

arrayInitElements -> expr ',' arrayInitElements     : ['$1' | '$3'].
arrayInitElements -> expr                           : ['$1'].

Unary 2000 structInitExpression.
structInitExpression -> identifier '{' structInitFields '}'     : #struct_init_raw{name = tokenValue('$1'), fields = '$3', line = tokenLine('$1')}.

structInitFields -> structInitAssignment ',' structInitFields   : ['$1' | '$3'].
structInitFields -> structInitAssignment                        : ['$1'].

structInitAssignment -> identifier '=' expr         : #op2{operator = assign, op1 = #varref{name = tokenValue('$1'), line = tokenLine('$1')}, op2 = '$3', line = tokenLine('$2')}.

%% return
returnExpression -> return expr                     : #return{expr = '$2', line = tokenLine('$1')}.

%% goto
gotoExpression -> goto expr                         : #goto{expr = '$2', line = tokenLine('$1')}.

%% sizeof
sizeofExpression -> sizeof '(' typeAnnotation ')'   : #sizeof{type = '$3', line = tokenLine('$2')}.

%% function invocation
callExpression -> expr '(' params ')'               : #call{fn = '$1', args = '$3', line = tokenLine('$2')}.
callExpression -> expr '(' ')'                      : #call{fn = '$1', args = [], line = tokenLine('$2')}.

assignExpression -> expr op2WithAssignment expr     : #op2{operator = assign, op1 = '$1', line = tokenLine('$2'), op2 = #op2{operator = tokenSymbol('$2'), op1 = '$1', op2 = '$3', line = tokenLine('$2')}}.
assignExpression -> expr '=' expr                   : #op2{operator = assign, op1 = '$1', op2 = '$3', line = tokenLine('$2')}.

op2WithAssignment -> op29 '='                       : '$1'.
op2WithAssignment -> op28 '='                       : '$1'.
op2WithAssignment -> op27 '='                       : '$1'.

params -> expr ',' params                           : ['$1' | '$3'].
params -> expr ','                                  : ['$1'].
params -> expr                                      : ['$1'].

atomicLiteralValues -> integer                      : '$1'.
atomicLiteralValues -> float                        : '$1'.
atomicLiteralValues -> string                       : '$1'.

labelExpression -> '@' '@' identifier ':'           : #label{name = tokenValue('$3'), line = tokenLine('$3')}.

%% expression
exprs -> rootExpression exprs                       : ['$1'|'$2'].
exprs -> rootExpression                             : ['$1'].

rootExpression -> variableDefinition ';'            : '$1'.
rootExpression -> expr ';'                          : '$1'.
rootExpression -> assignExpression ';'              : '$1'.
rootExpression -> returnExpression ';'              : '$1'.
rootExpression -> gotoExpression ';'                : '$1'.
rootExpression -> ifExpression                      : '$1'.
rootExpression -> whileExpression                   : '$1'.
rootExpression -> labelExpression                   : '$1'.

expr -> reservedKeyword                             : return_error(tokenLine('$1'), ecompilerUtil:flatfmt("~s is reserved keyword", [tokenSymbol('$1')])).
expr -> expr op30 expr                              : #op2{operator = tokenSymbol('$2'), op1 = '$1', op2 = '$3', line=tokenLine('$2')}.
expr -> expr op29 expr                              : #op2{operator = tokenSymbol('$2'), op1 = '$1', op2 = '$3', line=tokenLine('$2')}.
expr -> expr op28 expr                              : #op2{operator = tokenSymbol('$2'), op1 = '$1', op2 = '$3', line = tokenLine('$2')}.
expr -> expr op27 expr                              : #op2{operator = tokenSymbol('$2'), op1 = '$1', op2 = '$3', line = tokenLine('$2')}.
expr -> expr op26 expr                              : #op2{operator = tokenSymbol('$2'), op1 = '$1', op2 = '$3', line = tokenLine('$2')}.
expr -> expr op25 expr                              : #op2{operator = tokenSymbol('$2'), op1 = '$1', op2 = '$3', line = tokenLine('$2')}.
expr -> expr op19                                   : #op1{operator = tokenSymbol('$2'), operand = '$1', line = tokenLine('$2')}.
expr -> identifier                                  : #varref{name = tokenValue('$1'), line = tokenLine('$1')}.
expr -> preMinusPlusExpression                      : '$1'.
expr -> arrayInitExpression                         : '$1'.
expr -> structInitExpression                        : '$1'.
expr -> atomicLiteralValues                         : '$1'.
expr -> callExpression                              : '$1'.
expr -> sizeofExpression                            : '$1'.
expr -> '(' expr ')'                                : '$2'.

reservedKeyword -> 'cond'                           : '$1'.
reservedKeyword -> 'case'                           : '$1'.
reservedKeyword -> for                              : '$1'.
reservedKeyword -> break                            : '$1'.
reservedKeyword -> continue                         : '$1'.

%% the precedence of 'preMinusPlusExpression' needs to be higher than "op2 -"
Unary 300 preMinusPlusExpression.
preMinusPlusExpression -> '-' expr                  : #op1{operator = tokenSymbol('$1'), operand = '$2', line = tokenLine('$1')}.
preMinusPlusExpression -> '+' expr                  : #op1{operator = tokenSymbol('$1'), operand = '$2', line = tokenLine('$1')}.

Unary 900 op19.
op19 -> '^'                                         : '$1'.
op19 -> '@'                                         : '$1'.
op19 -> '!'                                         : '$1'.
op19 -> '~'                                         : '$1'.

Left 1000 op30.
op30 -> '.'                                         : '$1'.
op30 -> '::'                                        : '$1'.

Left 290 op29.
op29 -> '*'                                         : '$1'.
op29 -> '/'                                         : '$1'.
op29 -> 'rem'                                       : '$1'.

Left 280 op28.
op28 -> '+'                                         : '$1'.
op28 -> '-'                                         : '$1'.

Left 270 op27.
op27 -> 'bsl'                                       : '$1'.
op27 -> 'bsr'                                       : '$1'.
op27 -> 'band'                                      : '$1'.
op27 -> 'bor'                                       : '$1'.
op27 -> 'bxor'                                      : '$1'.

Nonassoc 260 op26.
op26 -> '=='                                        : '$1'.
op26 -> '!='                                        : '$1'.
op26 -> '>='                                        : '$1'.
op26 -> '<='                                        : '$1'.
op26 -> '>'                                         : '$1'.
op26 -> '<'                                         : '$1'.

Left 250 op25.
op25 -> 'and'                                       : '$1'.
op25 -> 'or'                                        : '$1'.


Erlang code.

-include("ecompilerFrameDef.hrl").

stringToIntegerTokens({string, Line, Str}) ->
    lists:map(fun (Char) -> {integer, Line, Char} end, Str).

tokenValue({_, _, Val}) ->
    Val.

tokenSymbol({Sym, _}) ->
    Sym.

tokenLine(T) ->
    element(2, T).
