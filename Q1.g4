/**
 * Student name:
 * Student ID:
 */
grammar Q1;

@lexer::header{
	package q1.parser;
}

@parser::header{
	package q1.parser;
}

options{
	language=Java;
}

prog: stmt+;
stmt: ID '=' exp ';';
exp: term (ADDOP term)*;
term: term MULOP fact|fact;
fact : ID|INTLIT|FLOATLIT|'(' exp ')'; 
INTLIT: [0-9]+;
FLOATLIT: [0-9]+'.'[0-9]+;
MULOP: '*'|'/';
ADDOP: '+'|'-';
ID: [a-z]+;
WS: [ \t\r\n]+ -> skip;