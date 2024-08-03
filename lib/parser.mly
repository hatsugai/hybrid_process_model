%{
open Id
open S
%}

%token EOF
%token QUESTION
%token EXCLAMATION
%token AMPERSAND
%token VBAR
%token SEMICOLON
%token COLON
%token PERIOD
%token COMMA
%token LPAR
%token RPAR
%token LCUR
%token RCUR
%token LBRA
%token RBRA
%token PLUS
%token MINUS
%token ASTERISK
%token DIV
%token MOD
%token NOT
%token AND
%token OR
%token EQ
%token NE
%token LT
%token LE
%token GT
%token GE
%token ARROW
%token DARROW
%token RECORD_UPDATE
%token FUN
%token CALL
%token FALSE
%token TRUE
%token SKIP
%token BECOMES
%token LET
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token RETURN
%token BREAK
%token CONTINUE
%token SELECT
%token END
%token CONST
%token VAR
%token <int> INT
%token <Id.t> ID

%type <S.expr> expr
%type <S.stmt> stmt
%type <S.definition list> compile_unit
%start stmt
%start expr
%start compile_unit

%right IMP
%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left ASTERISK DIV MOD

%%

compile_unit:
  global_definition_list { $1 }
;

global_definition_list:
/* empty */ { [] }
| global_definition_list global_definition { $1 @ [$2] }
;

global_definition:
  CONST ID EQ expr { Constant ($2, $4) }
| VAR ID EQ expr { Variable ($2, $4) }
| ID LPAR parameter_list RPAR stmt { Function ($1, $3, $5) }
;

parameter_list:
  /* empty */ { [] }
| ID { [$1] }
| parameter_list COMMA ID { $1 @ [$3] }
;

stmt:
  SKIP                         { Skip }
| ID BECOMES expr              { Assign ($1, $3) }
| ASTERISK expr BECOMES expr   { RAssign ($2, $4) }
| LET ID EQ expr               { Let ($2, $4) }
| LCUR stmt_list RCUR          { Seq $2 }
| IF expr THEN stmt ELSE stmt  { If ($2, $4, $6) }
| IF expr THEN stmt            { If ($2, $4, Skip) }
| WHILE expr DO stmt           { While ($2, $4) }
| RETURN expr                  { Return $2 }
| BREAK                        { Break }
| CONTINUE                     { Continue }
| syncterm                     { Select [$1] }
| SELECT syncterm_list END     { Select $2 }
;

stmt_list:
  /* empty */                  { [] }
| stmt                         { [$1] }
| stmt_list SEMICOLON stmt     { $1 @ [$3] }
| stmt_list SEMICOLON          { $1 }
;

syncterm_list:
  syncterm                     { [$1] }
| syncterm_list VBAR syncterm  { $1 @ [$3] }
;

syncterm:
  expr ARROW stmt                   { Event ($1, $3) }
| expr EXCLAMATION expr ARROW stmt  { Event (Apply ($1, [$3]), $5) }
| expr QUESTION ID ARROW stmt       { Receive ($1, $3, $5) }
;

expr:
  FALSE { Bool false }
| TRUE { Bool true }
| INT
	{ Int $1 }
| LBRA expr_list RBRA
	{
	  List.fold_right
        (fun e x -> Apply (Var (Id.make "Cons"), [e; x]))
       $2 (Var (Id.make "Nil"))
	}
| ID
	{ Var $1 }
| IF expr THEN expr ELSE expr
    { IfExpr($2, $4, $6) }
| expr PLUS expr
	{ Apply (Var (Id.make "+"), [$1; $3]) }
| expr MINUS expr
	{ Apply (Var (Id.make "-"), [$1; $3]) }
| expr ASTERISK expr
	{ Apply (Var (Id.make "*"), [$1; $3]) }
| expr DIV expr
	{ Apply (Var (Id.make "/"), [$1; $3]) }
| expr EQ expr
	{ Apply (Var (Id.make "="), [$1; $3]) }
| expr LT expr
	{ Apply (Var (Id.make "<"), [$1; $3]) }
| expr LE expr
	{ Apply (Var (Id.make "<="), [$1; $3]) }
| expr GT expr
	{ Apply (Var (Id.make "<"), [$3; $1]) }
| expr GE expr
	{ Apply (Var (Id.make "<="), [$3; $1]) }
| LPAR expr RPAR
	{ $2 }
| expr LPAR expr_list RPAR
    { Apply ($1, $3) }
| AMPERSAND expr
    { Ref $2 }
| ASTERISK expr
    { Deref $2 }
| expr PERIOD ID
    { RecordRef ($1, $3) }
| LCUR field_def_list RCUR
    { RecordCons $2 }
| expr RECORD_UPDATE LCUR field_def_list RCUR
    { RecordUpdate ($1, $4) }
| FUN LPAR id_list RPAR stmt
    { Fun ((Id.make ""), $3, $5) }
| FUN ID LPAR id_list RPAR stmt
    { Fun ($2, $4, $6) }
;

expr_list:
  /* empty */           { [] }
| expr                  { [$1] }
| expr_list COMMA expr  { $1 @ [$3] }
;

id_list:
  /* empty */       { [] }
| ID                { [$1] }
| id_list COMMA ID  { $1 @ [$3] }
;

field_def_list:
  field_def { [$1] }
| field_def_list SEMICOLON field_def
  { List.append $1 [$3] }
;

field_def:
  ID EQ expr { ($1, $3) }
;
