Nonterminals

rootStatementList rootStatement structDefinition functionDefinition variableDefinitionList variableDefinition parameters
functionStatementList functionStatement ifStatement elseStatement whileStatement
expression  callExpression preMinusPlusExpression sizeofExpression assignExpression op19 op30 op29 op28 op27 op26 op25 op2WithAssignment
typeAnnotationList typeAnnotation
pointerDepth atomicLiteralValues arrayInitExpression arrayInitElements structInitExpression structInitFields structInitAssignment
reservedKeyword

.

Terminals

%% operators
',' '::' ':' ';' '=' '{' '}' '(' ')' '<' '>' '+' '-' '*' '/' '^' '@' '.' '~' '!' '!=' '==' '>=' '<='
%% keywords
struct 'end' 'fun' 'rem' 'and' 'or' 'band' 'bor' 'bxor' 'bsl' 'bsr' while do 'if' then elif else return sizeof goto

%% reserved keywords
'cond' 'case' for break continue

%%
identifier integer float string integerType floatType voidType anyType

.

Rootsymbol rootStatementList.

rootStatementList -> rootStatement rootStatementList : ['$1' | '$2'].
rootStatementList -> rootStatement : ['$1'].

rootStatement -> structDefinition : '$1'.
rootStatement -> functionDefinition : '$1'.
rootStatement -> variableDefinition ';' : '$1'.

%% type annotation inside array or function
typeAnnotationList -> typeAnnotation ',' typeAnnotationList : ['$1' | '$3'].
typeAnnotationList -> typeAnnotation : ['$1'].

typeAnnotation -> 'fun' '(' typeAnnotationList ')' ':' typeAnnotation :
    #functionType{parameters = '$3', ret = '$6', line = tokenLine('$1')}.
typeAnnotation -> 'fun' '(' typeAnnotationList ')' :
    #functionType{parameters = '$3', ret = ecompilerUtil:voidType(tokenLine('$4')), line = tokenLine('$1')}.
typeAnnotation -> 'fun' '(' ')' ':' typeAnnotation :
    #functionType{parameters = [], ret = '$5', line = tokenLine('$1')}.
typeAnnotation -> 'fun' '(' ')' :
    #functionType{parameters = [], ret = ecompilerUtil:voidType(tokenLine('$3')), line = tokenLine('$1')}.
typeAnnotation -> '{' typeAnnotation ',' expression '}' :
    #arrayType{elemtype = '$2', length = tokenValue('$4'), line = tokenLine('$1')}.
typeAnnotation -> integerType pointerDepth :
    #basicType{class = integer, pdepth = '$2', tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> integerType :
    #basicType{class = integer, pdepth = 0, tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> floatType pointerDepth :
    #basicType{class = float, pdepth = '$2', tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> floatType :
    #basicType{class = float, pdepth = 0, tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> identifier pointerDepth :
    #basicType{class = struct, pdepth = '$2', tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> identifier :
    #basicType{class = struct,pdepth = 0, tag = tokenValue('$1'), line = tokenLine('$1')}.
typeAnnotation -> anyType pointerDepth :
    #basicType{class = any, pdepth = '$2', tag = void, line = tokenLine('$1')}.
typeAnnotation -> anyType :
    return_error(tokenLine('$1'), "type any is not allowed here").
typeAnnotation -> voidType :
    return_error(tokenLine('$1'), "type void is not allowed here").

%% pointer depth
pointerDepth -> '^' pointerDepth : '$2' + 1.
pointerDepth -> '^' : 1.

variableDefinitionList -> variableDefinition ',' variableDefinitionList : ['$1' | '$3'].
variableDefinitionList -> variableDefinition ',' : ['$1'].
variableDefinitionList -> variableDefinition : ['$1'].

variableDefinition -> identifier ':' typeAnnotation '=' expression :
    #variableDefinition{name = tokenValue('$1'), type = '$3', initialValue = '$5', line = tokenLine('$1')}.

variableDefinition -> identifier ':' typeAnnotation :
    #variableDefinition{name = tokenValue('$1'), type = '$3', line = tokenLine('$1')}.

%% struct definition
structDefinition -> struct identifier variableDefinitionList 'end' :
    #structRaw{name = tokenValue('$2'), fields = '$3', line = tokenLine('$2')}.

%% function definition
functionDefinition -> 'fun' identifier '(' variableDefinitionList ')' ':' typeAnnotation functionStatementList 'end' :
    #functionRaw{name = tokenValue('$2'), parameters = '$4', returnType = '$7', statements = '$8', line = tokenLine('$2')}.
functionDefinition -> 'fun' identifier '(' ')' ':' typeAnnotation functionStatementList 'end' :
    #functionRaw{name = tokenValue('$2'), parameters = [], returnType = '$6', statements = '$7', line = tokenLine('$2')}.
functionDefinition -> 'fun' identifier '(' variableDefinitionList ')' functionStatementList 'end' :
    #functionRaw{name = tokenValue('$2'), parameters = '$4', returnType = ecompilerUtil:voidType(tokenLine('$5')), statements = '$6', line = tokenLine('$2')}.
functionDefinition -> 'fun' identifier '(' ')' functionStatementList 'end' :
    #functionRaw{name = tokenValue('$2'), parameters = [], returnType = ecompilerUtil:voidType(tokenLine('$4')), statements = '$5', line = tokenLine('$2')}.

%% while
whileStatement -> while expression do functionStatementList 'end' :
    #whileStatement{condition = '$2', statements = '$4', line = tokenLine('$1')}.

%% if
ifStatement -> 'if' expression then functionStatementList elseStatement :
    #ifStatement{condition = '$2', then = '$4', else = '$5', line = tokenLine('$1')}.

elseStatement -> elif expression then functionStatementList elseStatement :
    [#ifStatement{condition = '$2', then = '$4', else = '$5', line = tokenLine('$1')}].
elseStatement -> else functionStatementList 'end' :
    '$2'.
elseStatement -> 'end' :
    [].

%% arrayInitExpression and structInitExpression contains similar pattern '{' '}'.
%% make the precedence of arrayInitExpression higher than structInitExpression
Unary 2100 arrayInitExpression.
arrayInitExpression -> '{' arrayInitElements '}' :
    #arrayInitializeExpression{elements = '$2', line = tokenLine('$1')}.
arrayInitExpression -> '{' string '}' :
    #arrayInitializeExpression{elements = stringToIntegerTokens('$2'), line = tokenLine('$1')}.

arrayInitElements -> expression ',' arrayInitElements : ['$1' | '$3'].
arrayInitElements -> expression : ['$1'].

Unary 2000 structInitExpression.
structInitExpression -> identifier '{' structInitFields '}' :
    #structInitializeExpressionRaw{name = tokenValue('$1'), fields = '$3', line = tokenLine('$1')}.

structInitFields -> structInitAssignment ',' structInitFields : ['$1' | '$3'].
structInitFields -> structInitAssignment : ['$1'].

structInitAssignment -> identifier '=' expression :
    #operatorExpression2{operator = assign, operand1 = #variableReference{name = tokenValue('$1'), line = tokenLine('$1')}, operand2 = '$3', line = tokenLine('$2')}.

%% sizeof
sizeofExpression -> sizeof '(' typeAnnotation ')' : #sizeofExpression{type = '$3', line = tokenLine('$2')}.

%% function invocation
callExpression -> expression '(' parameters ')' : #callExpression{fn = '$1', args = '$3', line = tokenLine('$2')}.
callExpression -> expression '(' ')' : #callExpression{fn = '$1', args = [], line = tokenLine('$2')}.

assignExpression -> expression op2WithAssignment expression :
    #operatorExpression2{operator = assign, operand1 = '$1', operand2 = #operatorExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}, line = tokenLine('$2')}.
assignExpression -> expression '=' expression :
    #operatorExpression2{operator = assign, operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.

op2WithAssignment -> op29 '=' : '$1'.
op2WithAssignment -> op28 '=' : '$1'.
op2WithAssignment -> op27 '=' : '$1'.

parameters -> expression ',' parameters : ['$1' | '$3'].
parameters -> expression ',' : ['$1'].
parameters -> expression : ['$1'].

atomicLiteralValues -> integer : '$1'.
atomicLiteralValues -> float : '$1'.
atomicLiteralValues -> string : '$1'.

functionStatementList -> functionStatement functionStatementList : ['$1' | '$2'].
functionStatementList -> functionStatement : ['$1'].

functionStatement -> expression ';' : '$1'.
functionStatement -> variableDefinition ';' : '$1'.
functionStatement -> assignExpression ';' : '$1'.
functionStatement -> ifStatement : '$1'.
functionStatement -> whileStatement : '$1'.
functionStatement -> goto expression ';' : #gotoStatement{expression = '$2', line = tokenLine('$1')}.
functionStatement -> return expression ';' : #returnStatement{expression = '$2', line = tokenLine('$1')}.
functionStatement -> '@' '@' identifier ':' : #gotoLabel{name = tokenValue('$3'), line = tokenLine('$3')}.

expression -> reservedKeyword : return_error(tokenLine('$1'), ecompilerUtil:fmt("~s is reserved keyword", [tokenSymbol('$1')])).
expression -> expression op30 expression : #operatorExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line=tokenLine('$2')}.
expression -> expression op29 expression : #operatorExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line=tokenLine('$2')}.
expression -> expression op28 expression : #operatorExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.
expression -> expression op27 expression : #operatorExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.
expression -> expression op26 expression : #operatorExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.
expression -> expression op25 expression : #operatorExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.
expression -> expression op19 : #operatorExpression1{operator = tokenSymbol('$2'), operand = '$1', line = tokenLine('$2')}.
expression -> identifier : #variableReference{name = tokenValue('$1'), line = tokenLine('$1')}.
expression -> preMinusPlusExpression : '$1'.
expression -> arrayInitExpression : '$1'.
expression -> structInitExpression : '$1'.
expression -> atomicLiteralValues : '$1'.
expression -> callExpression : '$1'.
expression -> sizeofExpression : '$1'.
expression -> '(' expression ')' : '$2'.

reservedKeyword -> 'cond' : '$1'.
reservedKeyword -> 'case' : '$1'.
reservedKeyword -> for : '$1'.
reservedKeyword -> break : '$1'.
reservedKeyword -> continue : '$1'.

%% the precedence of 'preMinusPlusExpression' needs to be higher than "operand2 -"
Unary 300 preMinusPlusExpression.
preMinusPlusExpression -> '-' expression : #operatorExpression1{operator = tokenSymbol('$1'), operand = '$2', line = tokenLine('$1')}.
preMinusPlusExpression -> '+' expression : #operatorExpression1{operator = tokenSymbol('$1'), operand = '$2', line = tokenLine('$1')}.

Unary 900 op19.
op19 -> '^' : '$1'.
op19 -> '@' : '$1'.
op19 -> '!' : '$1'.
op19 -> '~' : '$1'.

Left 1000 op30.
op30 -> '.' : '$1'.
op30 -> '::' : '$1'.

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

Nonassoc 260 op26.
op26 -> '==' : '$1'.
op26 -> '!=' : '$1'.
op26 -> '>=' : '$1'.
op26 -> '<=' : '$1'.
op26 -> '>' : '$1'.
op26 -> '<' : '$1'.

Left 250 op25.
op25 -> 'and' : '$1'.
op25 -> 'or' : '$1'.


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
