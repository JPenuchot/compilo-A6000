(* Syntaxe abstraite de la représentation intermédiaire *)
(* Les expressions sont aplaties, dans un style 'code trois adresses' *)

module Symb_Tbl = GotoAst.Symb_Tbl

type label           = GotoAst.label
type literal         = GotoAst.literal
type identifier_info = GotoAst.identifier_info
type binop           = GotoAst.binop
    
type program = (string * function_info) list
and function_info = {
  locals:  identifier_info Symb_Tbl.t;
  formals: int;
  code:    block
}

and block = (label * instruction) list
and instruction =
  | Value    of identifier * value                 (* Chargement d'une valeur *)
  | Binop    of identifier * binop * value * value (* Opération binaire       *)
  | Print    of value                              (* Affichage               *)
  | Label    of label                              (* Point de saut           *)
  | Goto     of label                              (* Saut                    *)
  | CondGoto of value * label                      (* Saut conditionnel       *)
  | Comment  of string                             (* Commentaire             *)
  | FunCall  of identifier * call                  (* Appel avec résultat     *)
  | ProcCall of call                               (* Appel sans résultat     *)
  | Load     of identifier * access                (* Lecture dans le tas     *)
  | Store    of access     * value                 (* Écriture dans le tas    *)
  | New      of identifier * value                 (* Création d'un bloc      *)

and call = string * value list
and access = value * value
    
and identifier = string (* Identifiant d'un registre virtuel *)
    
and value =
  | Literal    of literal    (* Valeur immédiate *)
  | Identifier of identifier (* Registre virtuel *)


open Printf
      
let rec print_block = function
  | []          -> "\n"
  | (l, i) :: b -> sprintf "%s: %s\n%s" l (print_instruction i) (print_block b)

and print_instruction = function
  | Value(dest, v)   -> sprintf "%s <- %s" dest (print_value v)
  | Binop(dest, op, v1, v2) -> sprintf "%s <- %s %s %s"
    dest (print_value v1) (SourceAst.print_binop op) (print_value v2)
  | Print(v)         -> sprintf "print(%s)" (print_value v)
  | Label(lab)       -> lab
  | Goto(lab)        -> sprintf "goto %s" lab
  | CondGoto(v, lab) -> sprintf "goto %s when %s" lab (print_value v)
  | Comment(c)       -> sprintf "# %s" c
  | FunCall(dest, c) -> sprintf "%s <- %s" dest (print_call c)
  | ProcCall(c)      -> print_call c
  | Load(dest, a)    -> sprintf "%s <- %s" dest (print_access a)
  | Store(a, v)      -> sprintf "%s <- %s" (print_access a) (print_value v)
  | New(dest, v)     -> sprintf "%s <- [%s]" dest (print_value v)
    
and print_value = function
  | Literal(lit)   -> SourceAst.print_literal lit
  | Identifier(id) -> id

and print_call (id, args) =
  sprintf "%s(%s)" id (print_args args)
and print_args = function
  | []      -> ""
  | [v]     -> print_value v
  | v::args -> sprintf "%s, %s" (print_value v) (print_args args)

and print_access (v1, v2) =
  sprintf "%s[%s]" (print_value v1) (print_value v2)
