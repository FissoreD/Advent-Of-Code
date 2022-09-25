%token <Pos.Dir.dirs> DIR
%token OR, OPEN_P, CLOSE_P, EOF

%start f
%type <Day20_types.regex> prefix, in_par, f

%%
f : e = e; EOF { e }

e :  
  | e = prefix { e }
  | e1 = prefix; e2 = e { Cat (e1, e2) }

prefix :
  | d = DIR { Symbol (d) }  
  | OPEN_P; e = in_par; CLOSE_P { e }

in_par :
  | e1 = e; OR; e2 = e { Choice (e1, e2) }
  | e1 = e; OR; e2 = in_par { Choice (e1, e2) }
  | e1 = e; OR { Choice (e1, Empty) }
