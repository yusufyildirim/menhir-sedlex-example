%token <int> INT
%token PLUS MINUS TIMES DIVIDE
%token LPARAN RPARAN
%token EOL EOF

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%start <int> calculate
%%

calculate:
| expr EOL | expr EOF { $1 }
;

expr:
| i = INT { i }
| MINUS e = expr %prec UMINUS { -e }
| LPARAN; e = expr; RPARAN { e }
| e1 = expr; PLUS; e2 = expr; { e1 + e2 }
| e1 = expr; MINUS; e2 = expr; { e1 - e2 }
| e1 = expr; TIMES; e2 = expr; { e1 * e2 }
| e1 = expr; DIVIDE; e2 = expr; { e1 / e2 }
;