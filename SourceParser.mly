%{

  open SourceAst

%}

(* Names & values *)
%token <string> IDENT
%token <bool> BOOLVAL
%token <int> INTVAL

(* Keywords *)
%token BEGIN END
%token SEMI
%token PRINT
%token EOF
%token MAIN
%token VAR

(* Typenames *)
%token INT
%token BOOL

(* Loops *)
%token FOR
%token WHILE
%token IF
%token THEN
%token ELSE

(* Binops *)
%token ADD
%token SUB
%token MULT
%token DIV

%token EQ
%token LT
%token LE
%token NEQ

%token AND
%token OR

%token AFFECT

%start main
%type <SourceAst.main> main

%%

main:
| MAIN; BEGIN; INT; x=IDENT; END;
  BEGIN; vds=var_decls; is=instructions; END; EOF  {
    let infox = { typ=TypInteger; kind=FormalX } in
    let init  = Symb_Tbl.singleton x infox in
    let union_vars = fun _ _ v -> Some v in
    let locals = Symb_Tbl.union union_vars init vds in
    {locals = locals; code=is} }
;

var_decls:
| (* empty *)                             { Symb_Tbl.empty }

| VAR; t=typename; id=IDENT; SEMI; vtbl=var_decls
                                          {
                                            Symb_Tbl.add id
                                              { typ = t; kind = Local }
                                              vtbl
                                          }
;

typename:
| INT                                     { TypInteger }
| BOOL                                    { TypBoolean }
;

instructions:
| (* empty *)                             { []                }
| i=instruction; SEMI; is=instructions    { i :: is           }

(* for(ins; comp; incr) ( blk ) *)
| FOR; BEGIN;
  i_start=instruction; SEMI;
  e=expression; SEMI;
  i_it=instruction; END
  BEGIN; blk=instructions; END            {
                                            i_start ::
                                            [ While(e, blk @ [i_it]) ]
                                          }
;

instruction:
| PRINT; BEGIN; e=expression; END         { Print(e)          }

| WHILE; e=expression;
  BEGIN; i=instructions; END              { While(e, i) }

| IF; e=expression;
  THEN; BEGIN; i1=instructions; END
  ELSE; BEGIN; i2=instructions; END       { If(e, i1, i2) }

| l=location; AFFECT; e=expression; END   { Set(l, e) }
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

location:
| id=IDENT  { Identifier id }
;
