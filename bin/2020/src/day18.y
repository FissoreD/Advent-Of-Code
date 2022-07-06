%{
  #include <stdlib.h>
  #include <stdio.h>
  int yylex();
  void yyerror();
%}

%union {
  long int i;
  char c;
}

%token <i>NB PR 
%type <i>start1 oper

// day18_1
%left '*'
%left '+'

// day18_2
//%left '+' '*'

%%

start : start1 {printf("%ld", $1);}

start1 : 
  start1 '\n' oper {$$ = $1 + $3;}
  | oper            {$$ = $1;}
;

oper :
  '(' oper ')'    {$$ = $2;}
  | oper '+' oper  {$$ = $1 + $3;}
  | oper '*' oper  {$$ = $1 * $3;}
  | NB            {$$=$1;}
;

%%
void main (){
  yyparse();
  printf("\n");
}

void yyerror(){
  return;
}