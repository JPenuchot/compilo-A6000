open SourceAst

(* Rapports d'erreurs *)
exception Type_error of typ * typ

(* comparetype: typ -> typ -> unit
   Lève une exception si les types diffèrent. *)
let comparetype t1 t2 =
  if t1 <> t2
  then raise (Type_error(t1, t2))

(* type_literal: literal -> typ *)
let type_literal = function
  | Int _  -> TypInteger
  | Bool _ -> TypBoolean
    
(* [type_binop] renvoie le type des opérandes et le type du résultat
   d'un opérateur binaire. *)
(* type_binop: binop -> typ * typ *)
let type_binop = function
  | Add | Sub | Mult     -> TypInteger, TypInteger
  | Eq  | Neq | Lt  | Le -> TypInteger, TypBoolean
  | And | Or             -> TypBoolean, TypBoolean

(* [type_expression] et les fonctions associées prennent en paramètre
   le programme lui-même (pour connaître les types des fonctions) et
   la table de symboles locale (pour connaître les types des variables).
   Leur résultat est de type [typ]. *)
let rec type_expression prog symb_tbl = function
  | Literal(lit)  -> type_literal lit
  | Location(loc) -> type_location prog symb_tbl loc
  | Binop(op, e1, e2) ->
    let ty_op, ty_r = type_binop op in
    comparetype ty_op (type_expression prog symb_tbl e1);
    comparetype ty_op (type_expression prog symb_tbl e2);
    ty_r
  | FunCall(c) ->
    (match type_call prog symb_tbl c with
      | None -> failwith "Non-void function expected"
      | Some t -> t)
  | NewArray(e, t) ->
    comparetype TypInteger (type_expression prog symb_tbl e);
    TypArray(t)
      
and type_location prog symb_tbl = function
  | Identifier(id)  -> (Symb_Tbl.find id symb_tbl).typ
  | ArrayAccess(aa) -> type_a_access prog symb_tbl aa
    
(* Un accès bien formé demande que [e1] désigne un tableau et [e2] un entier *)
and type_a_access prog symb_tbl (e1, e2) =
  comparetype TypInteger (type_expression prog symb_tbl e2);
  match type_expression prog symb_tbl e1 with
    | TypArray(t) -> t
    | _ -> failwith "Array type expected"

(* Un appel bien formé demande que chaque paramètre effectif ait un type 
   compatible avec le paramètre formel correspondant *)
(* Contrairement aux précédentes, [type_call] renvoie un résultat [typ option] *)
and type_call prog symb_tbl (fid, args) =
  let f = List.assoc fid prog in
  let expected_types = List.map snd f.formals in
  let arg_types = List.map (type_expression prog symb_tbl) args in
  List.iter2 comparetype expected_types arg_types;
  f.return
 
    
(* Vérification des types d'une fonction *)
let typecheck_function prog fun_info =
  let symb_tbl = fun_info.locals in

  (* [typecheck_block/instruction] vérifient le bon typage des instructions
     et lèvent une exception en cas de problème. *)
  (* typecheck_block: block -> unit *)
  let rec typecheck_block b = List.iter typecheck_instruction b

  (* typecheck_instruction: instruction -> unit *)
  and typecheck_instruction = function
    | Set(l, e) ->
      comparetype
	(type_location prog symb_tbl l)
	(type_expression prog symb_tbl e)
	
    | While(e, b) ->
      comparetype TypBoolean (type_expression prog symb_tbl e);
      typecheck_block b
	
    | If(e, b1, b2) ->
      comparetype TypBoolean (type_expression prog symb_tbl e);
      typecheck_block b1;
      typecheck_block b2;
      
    | Print(e) ->
      comparetype TypInteger (type_expression prog symb_tbl e)

    | ProcCall(c) ->
      (match type_call prog symb_tbl c with
	| None -> ()
	| _ -> failwith "Void function expected")

  in
    
  typecheck_block fun_info.code

let typecheck_program p =
  List.iter (fun (_, fun_info) -> typecheck_function p fun_info) p
