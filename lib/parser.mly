%{


%}

/* Constants */
%token <int> INT
%token <bool> BOOL
%token <string> STRING

%token EOF
%token LET
%token SLET
%token END
%token FUN
%token ARROW
%token <string> IDENT

%token IN
%token COLON
%token LPAREN
%token RPAREN
%token EQ
%token STATIC
%token DYNAMIC
%token LNOT
%token PLUS
%token SPLUS
%token MINUS
%token SMINUS
%token LAND
%token SLAND
%token SLNOT
%token SFUN
%token LOR
%token SLOR
%token DOLLAR
%token LIFT

%left PLUS

%left LAND

%nonassoc LNOT

%left DOLLAR

%type <Expr1.expr> program1
%type <Expr2.expr> program2


/* Start symbols */
%start program1 program2
%%


program1: e=expr1 EOF { e }
    
constant: 
    | i = INT { CInt i }
    | b = BOOL { CBool b } ;

expr1:
    | c=constant { EConst c }
    | v=IDENT { EVar v }
    | LET x=IDENT EQ e1=expr1 IN e2=expr1 { ELet (x, e1, e2) }
    | func=expr1 arg=expr1 { EApp (func, arg) }
    | FUN x=IDENT ARROW e=expr1 { ELam (x, e) }
    | e=expr1 COLON a=ann { EAnn (e, a) }
    | e0=expr1 PLUS e1=expr1 %prec PLUS { EOp (OAdd, [e0; e1]) }
    | e0=expr1 LAND e1=expr1 %prec LAND { EOp (OAnd, [e0; e1]) }
    | LNOT e0=expr1 %prec LNOT { EOp (ONot, [e0]) }
    | LPAREN e=expr1 RPAREN { e }
    ;

ann:
    | STATIC { Ann.S }
    | DYNAMIC { Ann.D }
    | arg=ann ARROW ret=ann { Ann.Func (arg, ret) }
    | LPAREN a=ann RPAREN { a }


program2: e=expr2 EOF { e }

expr2:
    | v=IDENT { Var v }
    (* expressions evaluates in pe-time *)
    | c=constant { SConst c }
    | SLET x=IDENT EQ e1=expr2 IN e2=expr2 { SLet (x, e1, e2) }
    | func=expr2 DOLLAR arg=expr2 %prec DOLLAR { SApp (func, arg) }
    | SFUN x=IDENT ARROW e=expr2 { SLam (x, e) }
    | e0=expr2 SPLUS e1=expr2 %prec PLUS { SOp (OAdd, [e0; e1]) }
    | e0=expr2 SLAND e1=expr2 %prec LAND { SOp (OAnd, [e0; e1]) }
    | SLNOT e0=expr2 %prec LNOT { SOp (ONot, [e0]) }
    | LPAREN e=expr2 RPAREN { e }
    (* expressions evaluates in run-time *)
    | LET x=IDENT EQ e1=expr2 IN e2=expr2 { DLet (x, e1, e2) }
    | func=expr2 arg=expr2 { DApp (func, arg) }
    | FUN x=IDENT ARROW e=expr2 { DLam (x, e) }
    | e0=expr2 PLUS e1=expr2 %prec PLUS { DOp (OAdd, [e0; e1]) }
    | e0=expr2 LAND e1=expr2 %prec LAND { DOp (OAnd, [e0; e1]) }
    | LNOT e0=expr2 %prec LNOT { DOp (ONot, [e0]) }
    | LPAREN e=expr2 RPAREN { e }
    | e = expr2 LIFT { DLift e }
    ;

