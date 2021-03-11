grammar pycj;

prog:
    func*
    ;

func:
    (returnFunc NEWLINE+)+
    | (voidFunc NEWLINE+)+
    ;

declare:
    (declareValue NEWLINE+)+
;

assign:
    (variableToValueAssignment NEWLINE+)+
    | (variableToVariableAssignment NEWLINE+)+
    | (variableToExpressionAssignment NEWLINE+)+
    ;

cast:
    (variableToVariableCast NEWLINE+)+
    | (variableToValueCast NEWLINE+)+
    | (variableToExpressionCast NEWLINE+)+
    ;

//TYPE
type: INT | FLOAT | BOOL ;

//NAME
varName: LOWERCASE+ ((LOWERCASE | ZERO | NATURAL)* MINUS? LOWERCASE+ (LOWERCASE | ZERO | NATURAL)*)* ;

//VALUE
value: intValue | floatValue | boolValue ;

intValue: ZERO | MINUS? NATURAL+ (ZERO | NATURAL)* ;
floatValue: MINUS? (ZERO | NATURAL (ZERO | NATURAL)*) POINT (ZERO | NATURAL)+;
boolValue: TRUE | FALSE;

//FUNCTION
signature: varName LEFT_ROUND_BRACKET attribute* RIGHT_ROUND_BRACKET;
attribute: (declareValue COMMA)* declareValue;
return: RETURN SPACE (varName | value) ;

returnFunc:
    DEF SPACE type SPACE signature SPACE LEFT_CURVY_BRACKET
    NEWLINE+
    bodyContent*
    TAB*
    return NEWLINE+
    TAB*
    RIGHT_CURVY_BRACKET
    ;

voidFunc:
    DEF SPACE VOID SPACE signature SPACE LEFT_CURVY_BRACKET
    NEWLINE+
    bodyContent*
    TAB*
    RIGHT_CURVY_BRACKET
    ;

bodyContent:
     TAB*
     (declare
     | initValue
     | assign
     | cast
     | condition
     | print
     | forCycle
     | whileCycle)
     NEWLINE*
    ;

//DECLARATION
declareValue: type SPACE varName;

//INITIALIZATION deprecated
initValue: initInt | initFloat | initBool ;

initInt: INT SPACE varName SPACE ASSIGN SPACE (intValue | ariphmeticExpression) ;
initFloat: FLOAT SPACE varName SPACE ASSIGN SPACE (floatValue | ariphmeticExpression) ;
initBool: BOOL SPACE varName SPACE ASSIGN SPACE (boolValue | ariphmeticExpression) ;

//ASSIGNMENT
variableToValueAssignment: varName SPACE ASSIGN SPACE value ;
variableToVariableAssignment: varName SPACE ASSIGN SPACE varName ;
variableToExpressionAssignment: varName SPACE ASSIGN SPACE ariphmeticExpression ;

//CASTING
castExpr: LEFT_ROUND_BRACKET (INT | FLOAT | BOOL) RIGHT_ROUND_BRACKET ;

variableToVariableCast: (type SPACE)* varName SPACE ASSIGN SPACE castExpr SPACE varName ;
variableToValueCast: (type SPACE)* varName SPACE ASSIGN SPACE castExpr SPACE value ;
variableToExpressionCast: (type SPACE)* varName SPACE ASSIGN SPACE castExpr SPACE ariphmeticExpression ;

//IF-THEN-ELSE STATEMENT
comparisonOperator: EQUAL | GREATER | GREATER_OR_EQUAL | LESS | LESS_OR_EQUAL | NOT_EQUAL ;
logicalComparisonOperator: LOGICAL_AND | LOGICAL_OR ;
comparisonAtom: varName | value | signature | TRUE | FALSE | ariphmeticExpression ;
comparisonStatement: comparisonAtom (SPACE comparisonOperator SPACE comparisonAtom)*;
comparisonLogicalStatement: LEFT_ROUND_BRACKET comparisonStatement (SPACE logicalComparisonOperator SPACE comparisonStatement)* RIGHT_ROUND_BRACKET;

ifCondition: IF SPACE comparisonLogicalStatement SPACE LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    TAB*
    RIGHT_CURVY_BRACKET
    (NEWLINE TAB* ELSE SPACE ifCondition)*
    (NEWLINE TAB* ELSE SPACE LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    TAB*
    RIGHT_CURVY_BRACKET)?
    ;

condition: ifCondition NEWLINE;

//ARIPHMETICS
ariphmeticOperator: PLUS | MINUS | MULTIPLY | DIVISION | MOD | POW ;
ariphmeticAtom: (varName | value | ariphmeticAtomWithBrackets) (SPACE ariphmeticOperator SPACE (varName | value | ariphmeticAtomWithBrackets))+;
ariphmeticAtomWithBrackets: LEFT_ROUND_BRACKET ariphmeticAtom+  RIGHT_ROUND_BRACKET;
ariphmeticExpression: (ariphmeticAtomWithBrackets | ariphmeticAtom) (SPACE ariphmeticOperator SPACE (ariphmeticAtom | ariphmeticAtomWithBrackets))*;

//PRINT
print: PRINT LEFT_ROUND_BRACKET (varName | value | ariphmeticExpression) RIGHT_ROUND_BRACKET;

//CYCLES
cycleStart: initValue | variableToValueAssignment | variableToExpressionAssignment | variableToVariableAssignment | variableToExpressionCast | variableToExpressionCast | variableToValueCast ;
cycleEnd: variableToValueAssignment | variableToExpressionAssignment | variableToVariableAssignment | variableToExpressionCast | variableToExpressionCast | variableToValueCast;
cycleStep: value | varName;
cycleHead: LEFT_ROUND_BRACKET cycleStart SEMI SPACE cycleEnd SEMI SPACE cycleStep RIGHT_ROUND_BRACKET ;

whileCycle: WHILE SPACE comparisonLogicalStatement SPACE LEFT_CURVY_BRACKET
    NEWLINE
    bodyContent*
    TAB*
    RIGHT_CURVY_BRACKET
    ;

forCycle: FOR SPACE cycleHead SPACE LEFT_CURVY_BRACKET
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
SPACE : ' ' ;
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