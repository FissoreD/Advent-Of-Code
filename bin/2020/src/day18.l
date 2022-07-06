%{
  #include "y.tab.h"
%}

%option noyywrap
NB [0-9]+
OP [+*]
PR [()]
%%
{NB} {yylval.i = atoi(yytext); return NB;}
{OP} {return yytext[0];}
{PR} {return yytext[0];}
.    ;
\n {return '\n';}
%%