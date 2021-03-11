grammar pycj;

prog:
    (func NEWLINE+)*
    ;

//TYPE
variableType: INT | FLOAT | BOOL ;

//NAME
variableName: LOWERCASE+ ((LOWERCASE | ZERO | NATURAL)* MINUS? LOWERCASE+ (LOWERCASE | ZERO | NATURAL)*)* ;

//VALUE
value: intValue | floatValue | boolValue ;

intValue: ZERO | MINUS? NATURAL+ (ZERO | NATURAL)* ;
floatValue: MINUS? (ZERO | NATURAL (ZERO | NATURAL)*) POINT (ZERO | NATURAL)+;
boolValue: TRUE | FALSE;

//FUNCTION
signature: variableName LEFT_ROUND_BRACKET attribute* RIGHT_ROUND_BRACKET;
attribute: (variableDeclare COMMA SPACE*)* variableDeclare;
return: RETURN SPACE (variableName | value) ;

func:
    DEF SPACE (variableType | VOID)  SPACE signature SPACE LEFT_CURVY_BRACKET
    NEWLINE+
    bodyContent*
    TAB*
    (return NEWLINE+
    TAB*)?
    RIGHT_CURVY_BRACKET
    ;

bodyContent:
     TAB*
     (variableDeclare
     | variableInit
     | variableAssignment
     | variableCast
     | ifCondition
     | print
     | forCycle
     | whileCycle)
     NEWLINE+
    ;

//DECLARATION
variableDeclare: variableType SPACE variableName;

//INITIALIZATION deprecated
variableInit: (INT | FLOAT | BOOL) SPACE variableName SPACE* ASSIGN SPACE* (intValue | floatValue | boolValue | ariphmeticExpression | variableName) ;

//ASSIGNMENT
variableAssignment: variableName SPACE* ASSIGN SPACE* (value | variableName | ariphmeticExpression) ;

//CASTING
castExpr: LEFT_ROUND_BRACKET SPACE* (INT | FLOAT | BOOL) SPACE* RIGHT_ROUND_BRACKET ;

variableCast: (variableType SPACE)* variableName SPACE ASSIGN SPACE* castExpr SPACE (variableName | value | ariphmeticExpression) ;

//IF-THEN-ELSE STATEMENT
comparisonOperator: EQUAL | GREATER | GREATER_OR_EQUAL | LESS | LESS_OR_EQUAL | NOT_EQUAL ;
logicalComparisonOperator: LOGICAL_AND | LOGICAL_OR ;
comparisonAtom: variableName | value | signature | TRUE | FALSE | ariphmeticExpression ;
comparisonStatement: comparisonAtom (SPACE* comparisonOperator SPACE* comparisonAtom)*;
comparisonLogicalStatement: LEFT_ROUND_BRACKET comparisonStatement (SPACE* logicalComparisonOperator SPACE* comparisonStatement)* RIGHT_ROUND_BRACKET;

ifCondition: IF SPACE comparisonLogicalStatement SPACE LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    TAB*
    RIGHT_CURVY_BRACKET
    (NEWLINE TAB* ELSE SPACE* ifCondition)*
    (NEWLINE TAB* ELSE SPACE* LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    TAB*
    RIGHT_CURVY_BRACKET)?
    ;

//ARIPHMETICS
ariphmeticOperator: PLUS | MINUS | MULTIPLY | DIVISION | MOD | POW ;
ariphmeticAtom: (variableName | value | ariphmeticAtomWithBrackets) (SPACE* ariphmeticOperator SPACE* (variableName | value | ariphmeticAtomWithBrackets))+;
ariphmeticAtomWithBrackets: LEFT_ROUND_BRACKET ariphmeticAtom+  RIGHT_ROUND_BRACKET;
ariphmeticExpression: (ariphmeticAtomWithBrackets | ariphmeticAtom) (SPACE* ariphmeticOperator SPACE* (ariphmeticAtom | ariphmeticAtomWithBrackets))*;

//PRINT
print: PRINT LEFT_ROUND_BRACKET (variableName | value | ariphmeticExpression) RIGHT_ROUND_BRACKET;

//CYCLES
cycleStart: variableInit | variableAssignment | variableCast ;
cycleEnd: variableAssignment | variableCast;
cycleStep: value | variableName;
cycleHead: LEFT_ROUND_BRACKET cycleStart SEMI SPACE* cycleEnd SEMI SPACE* cycleStep RIGHT_ROUND_BRACKET ;

whileCycle: WHILE SPACE* comparisonLogicalStatement SPACE* LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    TAB*
    RIGHT_CURVY_BRACKET
    ;

forCycle: FOR SPACE* cycleHead SPACE* LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    TAB*
    RIGHT_CURVY_BRACKET
    ;


//LEXEMAS
INT: 'int' ;
FLOAT: 'float' ;
BOOL: 'bool' ;
ASSIGN: '=' ;
PLUS: '+' ;
MULTIPLY: '*' ;
DIVISION: '/' ;
MOD: '%' ;
POW: '^' ;
MINUS : '-' ;
POINT : '.' ;
COMMA : ',' ;
LEFT_SQUARE_BRACKET: '[' ;
RIGHT_SQUARE_BRACKET: ']' ;
LEFT_ROUND_BRACKET: '(' ;
RIGHT_ROUND_BRACKET: ')' ;
LEFT_CURVY_BRACKET: '{' ;
RIGHT_CURVY_BRACKET: '}' ;
LOWERCASE : [a-z] ;
ZERO : [0] ;
NATURAL : [1-9] ;
SPACE : ' '+ ;
NEWLINE : [ \n\r]+ ;
TRUE: 'true' ;
FALSE: 'false' ;
DEF: 'def' ;
IF: 'if' ;
ELSE: 'else' ;
EQUAL:  '==' ;
GREATER: '>' ;
LESS: '<' ;
GREATER_OR_EQUAL: '>=' ;
LESS_OR_EQUAL: '<=' ;
NOT_EQUAL: '!=' ;
LOGICAL_AND: '&&' ;
LOGICAL_OR: '||' ;
TAB: [ \t]+ ;
RETURN: 'return' ;
PRINT: 'print' ;
VOID: 'void' ;
FOR: 'for' ;
WHILE: 'while' ;
SEMI: ';' ;