(* Syntaxe abstraite non typée *)
(* Cette version est obtenu en retirant tous les indications de typage *)
(* Pour l'instant, les types [expression], [instruction] et associés sont
   redéfinis à l'identique *)
module Symb_Tbl = SourceAst.Symb_Tbl

type identifier_kind = SourceAst.identifier_kind
type identifier_info = identifier_kind

type literal     = SourceAst.literal
type binop       = SourceAst.binop

type expression  =
  | Literal  of literal                         (* Valeur immédiate    *)
  | Location of location                        (* Valeur en mémoire   *)
  | Binop    of binop * expression * expression (* Opération binaire   *)
  | FunCall  of call                            (* Appel de fonction   *)
  | NewArray of expression                      (* Création de tableau *)

and location =
  | Identifier  of string (* Variable en mémoire *)
  | ArrayAccess of access (* Case d'un tableau   *)    
and access = expression * expression

and call = string * expression list
    
type block = instruction list
and instruction =
  | Set   of location   * expression    (* Affectation *)
  | While of expression * block         (* Boucle      *)
  | If    of expression * block * block (* Branchement *)
  | Print of expression                 (* Affichage   *)
  | ProcCall of call                    (* Appel       *)

type program = (string * function_info) list
and function_info = { 
  locals:  identifier_info Symb_Tbl.t;
  formals: int; (* On ne retient plus que le nombre de paramètres formels *)
  code:    block
}
