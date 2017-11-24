%{

  open SourceAst
  open Lexing
  
%}

%token <int> CONST_INT
%token PLUS MINUS STAR
%token <bool> CONST_BOOL
%token AND OR
%token EQUAL NEQ LT LE
%token <string> IDENT
%token BEGIN END
%token IF THEN ELSE
%token WHILE
%token SEMI
%token SET
%token VAR
%token INT BOOL
%token PRINT
%token EOF
%token COMMA
%token OB CB

%left AND OR
%left LE LT EQUAL NEQ
%left PLUS MINUS
%left STAR
%left OB
%left IDENT
%left BEGIN

%start program
%type <SourceAst.program> program

%%

program:
| fds=fun_decls; EOF  { fds }
;
 
fun_decls:
| (* empty *)                { []      }
| fd=fun_decl; fds=fun_decls { fd::fds };
    
fun_decl:
| ty=option(typ) id=IDENT BEGIN args=formals END
    BEGIN vds=var_decls is=instructions END {
      (* Si la fonction a un type de retour, initialiser la table des
	 symboles avec la variables spéciale "result" *)
      let base_env = match ty with
	| None -> Symb_Tbl.empty
	| Some ty -> Symb_Tbl.singleton "result" { typ=ty; kind=Return }
      in
      (* Calculer et ajouter les informations pour les paramètres formels *)
      let info_params = List.mapi (fun i (id, ty) ->
	id, { typ=ty; kind=Formal (i+1) }
      ) args
      in
      let param_env = List.fold_left (fun acc (id, info) ->
	Symb_Tbl.add id info acc
      ) base_env info_params
      in
      (* Ajouter enfin les variables locales *)
      let local_env = List.fold_left (fun acc (id, ty) ->
	Symb_Tbl.add id { typ=ty; kind=Local } acc
      ) param_env vds
      in
      id, { return=ty; formals=args; locals=local_env; code=is }
    }
;

formals:
| pl=separated_list(COMMA, typed_ident) { pl }
;

typed_ident:
| t=typ; id=IDENT  { (id, t) }
;
  
var_decls:
| vds=list(var_decl) { vds }
;

var_decl:
| VAR; vd=typed_ident; SEMI  { vd  }
;

typ:
| INT          { TypInteger    }
| BOOL         { TypBoolean    }
| OB CB ty=typ { TypArray(ty)  }
;

instructions:
| (* empty *)                          { []      }
| i=instruction; SEMI; is=instructions { i :: is }
;

instruction:
| l=location; SET; e=expression                     { Set(l, e)     }
| WHILE; e=expression; b=block                      { While(e, b)   }
| IF; e=expression; THEN; b1=block; ELSE; b2=block  { If(e, b1, b2) }
| PRINT; BEGIN; e=expression; END                   { Print(e)      }
| c=call                                            { ProcCall(c)   }
;

block:
| BEGIN; is=instructions; END        { is }
;

expression:
| lit=literal                             { Literal(lit)      }
| loc=location                            { Location(loc)     }
| BEGIN; e=expression; END                { e                 }
| e1=expression; op=binop; e2=expression  { Binop(op, e1, e2) }
| c=call                                  { FunCall(c)        }
| OB; e=expression; CB; ty=typ            { NewArray(e, ty)   }
;

call:
| id=IDENT BEGIN args=actuals END         { (id, args)        }
;

actuals:
| args=separated_list(COMMA, expression)  { args              }
;

literal:
| i=CONST_INT   { Int i  }
| b=CONST_BOOL  { Bool b }
;

location:
| id=IDENT                             { Identifier(id)      }
| e1=expression; OB; e2=expression; CB { ArrayAccess(e1, e2) }
;

%inline binop:
| PLUS   { Add  }
| MINUS  { Sub  }
| STAR   { Mult }
| EQUAL  { Eq   }
| NEQ    { Neq  }
| LT     { Lt   }
| LE     { Le   }
| AND    { And  }
| OR     { Or   }
;
