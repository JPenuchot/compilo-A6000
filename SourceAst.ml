(* Syntaxe abstraite typée *)

module Symb_Tbl = Map.Make(String)

(* La table des symboles contient, pour chaque variable :
   - sa nature  : variable locale ou paramètre formel
   - son type : entier ou booléen
*)

type program = function_info Symb_Tbl.t

and call = string * expression list

and function_info = { 
  return:  typ option;
  formals: typ list;
  locals:  identifier_info Symb_Tbl.t;
  code:    block
}
and identifier_kind =
  | Local           (* Variable locale    *)
  | Formal of int   (* Paramètre formel x *)
  | Return          (* Variable de retour *)
and typ =
  | TypInteger
  | TypBoolean
and identifier_info = { typ: typ; kind: identifier_kind }

(* Un bloc de code est une liste d'instructions *)
and block = instruction list
and instruction =
  | Set   of location   * expression    (* Affectation *)
  | While of expression * block         (* Boucle      *)
  | If    of expression * block * block (* Branchement *)
  | PCall of call
  | Print of expression                 (* Affichage   *)

and expression =
  | Literal   of literal                          (* Valeur immédiate   *)
  | Location  of location                         (* Valeur en mémoire  *)
  | Binop     of binop * expression * expression  (* Opération binaire  *)
  | Call      of call                             (* Appel de fonction  *)

and literal =
  | Int  of int  (* Constante entière   *)
  | Bool of bool (* Constante booléenne *)

and location =
  | Identifier of string (* Variable en mémoire *)

and binop =
  | Add (* +  *) | Mult (* *  *) | Sub (* - *)
  | Eq  (* == *) | Neq  (* != *) | Div (* / *)
  | Lt  (* <  *) | Le   (* <= *)
  | And (* && *) | Or   (* || *)

(* Cadeau pour le débogage : un afficheur.
   [print_main m] produit une chaîne de caractère représentant le programme
*)
open Printf

let print_typ = function
  | TypInteger -> "integer"
  | TypBoolean -> "boolean"
let print_identifier_info i = print_typ i.typ

let print_symb_tbl tbl =
  Symb_Tbl.fold (fun v i s ->
      (sprintf "  var %s %s;\n" (print_identifier_info i) v) ^ s
    ) tbl ""

let print_literal = function
  | Int i -> sprintf "%d" i
  | Bool b -> if b then "true" else "false"
let print_location = function
  | Identifier x -> x
let print_binop = function
  | Add  -> "+"
  | Mult -> "*"
  | Sub  -> "-"
  | Eq   -> "=="
  | Neq  -> "!="
  | Lt   -> "<"
  | Le   -> "<="
  | And  -> "&&"
  | Or   -> "||"
  | Div  -> "/"
let rec print_expression = function
  | Literal lit -> print_literal lit
  | Location id -> print_location id
  | Binop(op, e1, e2) ->
    sprintf "( %s %s %s )"
      (print_expression e1) (print_binop op) (print_expression e2)
  | Call(c) ->
    print_call c
and print_call (name, elist) =
  sprintf "%s ( " name ^
  List.fold_left (fun acc expr -> acc ^ (print_expression expr)) "" elist ^
  sprintf " )"

and offset o = String.make (2*o) ' '
and print_block o = function
  | [] -> ""
  | i::b -> (offset o) ^ (print_instruction o i) ^ ";\n" ^ (print_block o b)
and print_instruction o = function
  | Set(id, e) -> sprintf "%s := %s" (print_location id) (print_expression e)
  | While(e, b) ->
    sprintf "while %s (\n%s%s)"
      (print_expression e)
      (print_block (o+1) b) (offset o)
  | If(e, b1, b2) ->
    sprintf "if %s then (\n%s%s) else (\n%s%s)"
      (print_expression e)
      (print_block (o+1) b1) (offset o)
      (print_block (o+1) b2) (offset o)
  | Print(e) -> sprintf "print(%s)" (print_expression e)
  | PCall(c) -> print_call c

let print_main m =
  sprintf "main(int x) (\n%s%s)\n"
    (print_symb_tbl m.locals) (print_block 1 m.code)