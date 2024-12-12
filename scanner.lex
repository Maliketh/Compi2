%{
#include "output.hpp"
#include "parser.tab.h"
%}

%option yylineno
%option noyywrap

whitespace           [ \t\n\r]


printable_ascii    [\x20-\x7E]

illegal_string      ({string}|{illegal_escape}|{illegal_chars})

%%

void                    return VOID;
int                     return INT;
byte                    return BYTE;
bool                    return BOOL;
and                     return AND;
or                      return OR;
not                     return NOT;
true                    return TRUE;
false                   return FALSE;
return                  return RETURN;
if                      return IF;
else                    return ELSE;
while                   return WHILE;
break                   return BREAK;
continue                return CONTINUE;

;                       return SC;
,                       return COMMA;
\(                      return LPAREN;
\)                      return RPAREN;
\{                      return LBRACE;
\}                      return RBRACE;
=                       return ASSIGN;

(==|!=|<|>|<=|>=)      return RELOP;
[\+\-]                 return BINOP_ADD;
[\*\/]                 return BINOP_MUL;

\/\/[^\n\r]*           return COMMENT;

[a-zA-Z][a-zA-Z0-9]*    return ID;
(0|[1-9][0-9]*)         return NUM;
(0|[1-9][0-9]*)+b       return NUM_B;

\"([^"\\]|\\.)*\"        return STRING;
\"{printable_ascii}*\"   return STRING;

{whitespace}            ;
.                       return ERR_GENERAL;

\"[^\"]*                return ERR_UNCLOSED_STR;
"([^"\\]*(\\.[^"\\]*)*  return ERR_UNCLOSED_STR;



%%
