
/* Parsing for mini-Turtle */

%{
  open Ast
  open Turtle
%}

/* Declaration of tokens */

%token EOF
%token <int> INT
%token <string> IDENT
%token PLUS MINUS TIMES DIV
%token LP RP LBR RBR
%token FORWARD PU PD TL TR
%token IF ELSE REP COMMA DEF
%token CLR BLACK WHITE RED GREEN BLUE



/* To be completed */

/* Priorities and associativity of tokens */
%left PLUS MINUS 
%left TIMES DIV
%nonassoc IF
%nonassoc ELSE

%nonassoc neg


/* To be completed */

/* Axiom of the grammar */
%start prog

/* Type of values ​​returned by the parser */
%type <Ast.program> prog
%type <Turtle.color> color
%type <Ast.def> def
%type <Ast.expr> expr
%type <Ast.stmt> stmt
%type <Ast.def list> list(def)
%type <Ast.stmt list> list(stmt)
%type <string list> loption(separated_nonempty_list(COMMA,IDENT))
%type <Ast.expr list> loption(separated_nonempty_list(COMMA,expr))
%type <string list> separated_nonempty_list(COMMA,IDENT)
%type <Ast.expr list> separated_nonempty_list(COMMA,expr)
%%

/* Production rules of the grammar */

color: 
| BLACK 
  { Turtle.black }
| WHITE
  { Turtle.white }
| RED
  { Turtle.red }
| GREEN
  { Turtle.green }
| BLUE
  { Turtle.blue }
;
expr:
| i = INT 
  { Econst i }
| id = IDENT 
  { Evar id }
| MINUS e1 = expr %prec neg
  { Ebinop (Mul, e1, Econst(-1))}
| e1 = expr PLUS e2 = expr
  { Ebinop (Add, e1, e2)}
| e1 = expr MINUS e2 = expr
  { Ebinop (Sub, e1, e2)}
| e1 = expr TIMES e2 = expr
  { Ebinop (Mul, e1, e2)}
| e1 = expr DIV e2 = expr
  { Ebinop (Div, e1, e2)}
| LP e = expr RP
  { e }

;
stmt:
| FORWARD e = expr
  { Sforward e }
| PU 
  { Spenup }
| PD 
  { Spendown }
| TL e = expr
  { Sturn e }
| TR e = expr 
  { Sturn (Ebinop(Mul, e, Econst(-1))) }
| CLR clr = color
  { Scolor clr }
| f = IDENT LP sl = separated_list(COMMA, expr) RP
  { Scall (f, sl) }
| IF e = expr s = stmt 
  { Sif (e, s, Sblock []) } 
| IF e = expr s1 = stmt ELSE s2 = stmt
  { Sif (e, s1, s2) }
| REP e = expr s = stmt
  { Srepeat (e, s) }
| LBR sl = list(stmt) RBR
  { Sblock sl }
;
def:
| DEF id = IDENT LP params = separated_list(COMMA, IDENT) RP s = stmt
  { { name = id; formals = params; body = s } }

prog:
| dl = list(def) sl = list(stmt) EOF
    { { defs = dl; main = Sblock(sl) } }
;
