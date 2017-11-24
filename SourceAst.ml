(* Syntaxe abstraite typée *)

module Symb_Tbl = Map.Make(String)

(* On programme est constitué de plusieurs fonctions, et on donne pour
   chacune son nom et sa définition. On pourrait aussi utiliser
   le type [function_info Symb_Tbl.t] *)
type program = (string * function_info) list

and function_info = {
  (* Les mentions du type de retour et des types des paramètres seront
     utiles pour la vérification des types *)
  return:  typ option;
  formals: (string * typ) list;
  locals:  identifier_info Symb_Tbl.t;
  code:    block;
}

(* Différentes sortes de variables *)
and identifier_kind =
  | Local         (* Variable locale  *)
  | Formal of int (* Paramètre formel *)
  | Return        (* Résultat d'une fonction *)
and identifier_info = { typ: typ; kind: identifier_kind }
and typ =
  | TypInteger
  | TypBoolean
  | TypArray  of typ
      
(* Un bloc de code est une liste d'instructions *)
and block = instruction list
and instruction =
  | Set   of location   * expression    (* Affectation       *)
  | While of expression * block         (* Boucle            *)
  | If    of expression * block * block (* Branchement       *)
  | Print of expression                 (* Affichage         *)
  | ProcCall of call                    (* Appel de fonction *)

and expression =
  | Literal   of literal                         (* Valeur immédiate    *)
  | Location  of location                        (* Valeur en mémoire   *)
  | Binop     of binop * expression * expression (* Opération binaire   *)
  | FunCall   of call                            (* Appel de fonction   *)
  | NewArray  of expression * typ                (* Création de tableau *)
      
and call = string * expression list (* Appel de fonction *)
  
and literal =
  | Int  of int  (* Constante entière   *)
  | Bool of bool (* Constante booléenne *)

and location =
  | Identifier  of string   (* Variable en mémoire *)
  | ArrayAccess of a_access (* Case d'un tableau   *)
and a_access = expression * expression
      
and binop =
  | Add (* +  *) | Mult (* *  *) | Sub (* - *)
  | Eq  (* == *) | Neq  (* != *)
  | Lt  (* <  *) | Le   (* <= *)
  | And (* && *) | Or   (* || *)
    
(* Cadeau pour le débogage : un afficheur. *)
open Printf
      
let rec print_typ = function
  | TypInteger    -> "integer"
  | TypBoolean    -> "boolean"
  | TypArray(ty)  -> (print_typ ty)^"array"
let print_identifier_info i = print_typ i.typ

let print_symb_tbl tbl =
  Symb_Tbl.fold (fun v i s ->
    (sprintf "  var %s %s;\n" (print_identifier_info i) v) ^ s
  ) tbl ""

let print_literal = function
  | Int i -> sprintf "%d" i
  | Bool b -> if b then "true" else "false"
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
let rec print_expression = function
  | Literal lit -> print_literal lit
  | Location id -> print_location id
  | Binop(op, e1, e2) -> sprintf "( %s %s %s )" (print_expression e1) (print_binop op) (print_expression e2)
  | FunCall(c) -> print_call c
  | NewArray(e, ty) -> sprintf "[%s]%s" (print_expression e) (print_typ ty)
and print_location = function
  | Identifier x -> x
  | ArrayAccess(aa) -> print_a_access aa
and print_a_access (e1, e2) = sprintf "%s[%s]" (print_expression e1) (print_expression e2)
and print_call (id, args) =
  sprintf "%s(%s)" id (print_args args)
and print_args = function
  | []      -> ""
  | [e]     -> print_expression e
  | e::args -> sprintf "%s, %s" (print_expression e) (print_args args)

let offset o = String.make (2*o) ' '
let rec print_block o = function
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
  | ProcCall(c) -> print_call c

let rec print_formals = function
  | [] -> ""
  | [(id, ty)] -> sprintf "%s %s" (print_typ ty) id
  | (id,ty)::formals -> sprintf "%s %s, %s" (print_typ ty) id (print_formals formals)

let print_ret_type = function
  | None -> ""
  | Some t -> sprintf "(%s) " (print_typ t)
    
let print_function f_id f_info =
  sprintf "%s(%s) (\n%s%s)\n"
    f_id (print_formals f_info.formals)
    (print_symb_tbl f_info.locals) (print_block 1 f_info.code)
  
let print_program prog =
  List.fold_right (fun (f_id, f_info) s ->
    sprintf "%s\n\n%s" (print_function f_id f_info) s
  ) prog ""
