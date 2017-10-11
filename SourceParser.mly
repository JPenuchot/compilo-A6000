%{

  open SourceAst

%}

(* Keywords *)
%token VAR
%token SEMI COMMA
%token PRINT
%token EOF
%token MAIN
%token BEGIN END

(* Loops *)
%token FOR WHILE IF THEN ELSE

(* Typenames *)
%token INT BOOL

(* Names & values *)
%token <string> IDENT
%token <bool> BOOLVAL
%token <int> INTVAL

(* Affectation *)
%token AFFECT

(* Binops *)
%token AND OR
%token EQ LT LE NEQ
%token MULT DIV
%token ADD SUB

%left AND OR
%left EQ LT LE NEQ
%left ADD SUB
%left MULT DIV

%start main
%type <SourceAst.main> main

%%

main:
| MAIN; BEGIN; INT; x=IDENT; END;
  BEGIN; vds=var_decls; is=instructions; END; EOF 
  {
    let infox = { typ = TypInteger; kind = FormalX } in
    let init  = Symb_Tbl.singleton x infox in
    let union_vars = fun _ _ v -> Some v in
    let locals = Symb_Tbl.union union_vars init vds in
    { locals = locals; code = is }
  }
;

var_decls:
| (* empty *)                             { Symb_Tbl.empty }

| VAR; t=typename; id=IDENT; SEMI; vtbl=var_decls
  { Symb_Tbl.add id { typ = t; kind = Local } vtbl }
;

instructions:
| (* empty *)                             { [] }
| i=instruction; SEMI; is=instructions    { i :: is }

(* for ins, comp, incr ( blk ) *)
| FOR;
    i_start=instruction; COMMA; e=expression; COMMA; i_it=instruction;
  BEGIN; blk=instructions; END; is=instructions
                                          {
                                            i_start ::
                                            [ While(e, blk @ [i_it]) ] @
                                            is
                                          }
;

instruction:
| PRINT; BEGIN; e=expression; END         { Print(e) }

| WHILE; e=expression;
  BEGIN; i=instructions; END              { While(e, i) }

| IF; e=expression;
  THEN; BEGIN; i1=instructions; END
  ELSE; BEGIN; i2=instructions; END       { If(e, i1, i2) }

| l=location; AFFECT; e=expression        { Set(l, e) }
| l=location; b=binop;AFFECT;e=expression { Set(l, Binop(b, Location(l), e)) }
;

expression:
| i=INTVAL;                               { Literal(Int(i)) }
| b=BOOLVAL;                              { Literal(Bool(b)) }
| loc=location                            { Location(loc) }
| e1=expression; b=binop; e2=expression   { Binop(b, e1, e2) }
;

%inline binop:
| ADD;      { Add }
| SUB;      { Sub }
| MULT;     { Mult }
| DIV;      { Div }

| EQ;       { Eq }
| NEQ;      { Neq }
| LT;       { Lt }
| LE;       { Le }

| AND;      { And }
| OR;       { Or }
;

typename:
| INT                                     { TypInteger }
| BOOL                                    { TypBoolean }
;

location:
| id=IDENT  { Identifier id }
;
