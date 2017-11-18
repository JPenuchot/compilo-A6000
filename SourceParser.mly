%{

  open SourceAst

%}

(* Keywords *)
%token VAR
%token SEMI COMMA
%token PRINT
%token EOF
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

%start prog
%type <SourceAst.program> prog

%%

(* Program structure *)

prog:
| EOF                                     { Symb_Tbl.empty }
| fd = fun_decl; p = prog;
  {
    let (fd, id) = fd in
      Symb_Tbl.add id fd p
  }

(* Function declaration syntax *)

fun_decl:
(* Function *)
| t=typename; id=IDENT;
  BEGIN; p=parameters; END;
  BEGIN; vds=var_decls; is=instructions; END;
  {
    let (form, pl, _) = p in
    (
      { return = None;
        locals = Symb_Tbl.union
        (fun _ _ v -> Some v)
        vds
        (Symb_Tbl.add "return" {typ = t; kind = Return} form);
        formals = pl;
        code = is
      },
      id
    )
  }

(* Procedure *)
| id=IDENT;
  BEGIN; p=parameters; END;
  BEGIN; vds=var_decls; is=instructions; END;
  {
    (
      let (form, pl, _) = p in
      { 
        return = None;
        locals = Symb_Tbl.union
        (fun _ _ v -> Some v)
         vds form;
        formals = pl;
        code = is
      },
      id
    )
  }
;

parameters:
| (* Empty *)                             { (Symb_Tbl.empty, [], 0) }
| p=params                                { p }

params:
| tp=typename; id=IDENT
  {
    (Symb_Tbl.singleton id { typ = tp; kind = Formal(1)}, [ tp ], 2)
  }
| p=parameters; COMMA;tp=typename; id=IDENT;
  { 
    let (st, pl, num) = p in
    (Symb_Tbl.add id { typ = tp; kind = Formal(num)} st, tp :: pl, num + 1)
  }
;


var_decls:
| (* empty *)                             { Symb_Tbl.empty }

| VAR; t=typename; id=IDENT; SEMI; vtbl=var_decls
  {
    Symb_Tbl.add id { typ = t; kind = Local } vtbl
  }
;

(* Instruction syntax *)

instructions:
| (* empty *)                             { [] }
| i=instruction; SEMI; is=instructions    { i :: is }

(* for ins, comp, incr ( blk ) *)
| FOR;
  BEGIN; i_st=instruction;
    COMMA; e=expression;
    COMMA; i_it=instruction;
  END;
  BEGIN; blk=instructions; END; is=instructions
  {
    i_st ::
    [ While(e, blk @ [i_it]) ] @
    is
  }
;

instruction:
| PRINT; BEGIN; e=expression; END         { Print(e) }

| WHILE; BEGIN; e=expression; END;
  BEGIN; i=instructions; END              { While(e, i) }

| IF; e=expression;
  THEN; BEGIN; i1=instructions; END;
  ELSE; BEGIN; i2=instructions; END       { If(e, i1, i2) }

| l=location; AFFECT; e=expression        { Set(l, e) }
| l=location; b=binop;AFFECT;e=expression { Set(l, Binop(b, Location(l), e)) }
| c=call                                  { CallIns(c) }
;

(* Expression syntax *)

expression:
| i=INTVAL;                               { Literal(Int(i)) }
| b=BOOLVAL;                              { Literal(Bool(b)) }
| loc=location                            { Location(loc) }
| e1=expression; b=binop; e2=expression   { Binop(b, e1, e2) }
| c=call                                  { CallExp(c) }
;

(* Function calls *)

call:
| i=IDENT; BEGIN; a=arguments; END        { (i, a) }
;

arguments:
| (* empty *)                             { [] }
| elist=arguments; COMMA; e=expression    { e :: elist }
;

(* Types *)

typename:
| INT                                     { TypInteger }
| BOOL                                    { TypBoolean }
;

location:
| id=IDENT  { Identifier id }
;

(* Binops *)

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
