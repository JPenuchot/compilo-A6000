(* Transformation de la syntaxe abstraite typée
   en syntaxe abstraite non typée. *)

(* À ce stade, la plupart de ces fonctions traduisent le programme
   sans rien modifier. *)

module S = SourceAst  (* Source de la transformation *)
module T = UntypedAst (* Cible de la transformation  *)
  
let erase_function p f =
  let rec erase_block b = List.map erase_instruction b
  and erase_instruction : S.instruction -> T.instruction = function
    | Set(l, e)     -> T.Set(erase_location l, erase_expression e)
    | While(e, b)   -> T.While(erase_expression e, erase_block b)
    | If(e, b1, b2) -> T.If(erase_expression e, erase_block b1, erase_block b2)
    | Print(e)      -> T.Print(erase_expression e)
    | ProcCall(c)   -> T.ProcCall(erase_call c)
      
  and erase_expression : S.expression -> T.expression = function
    | Literal(l)        -> T.Literal(l)
    | Location(l)       -> T.Location(erase_location l)
    | Binop(op, e1, e2) -> T.Binop(op, erase_expression e1, erase_expression e2)
    | FunCall(c)        -> T.FunCall(erase_call c)
    | NewArray(e, _)    -> T.NewArray(erase_expression e)
      
  and erase_location : S.location -> T.location = function
    | Identifier(id)  -> T.Identifier(id)
    | ArrayAccess(aa) -> T.ArrayAccess(erase_a_access aa)
      
  and erase_a_access (e1, e2) = (erase_expression e1, erase_expression e2)

  and erase_call (id, args) =
    (id, List.map erase_expression args)
  in

  (* erase_identifier_info: S.identifier_info -> T.identifier_info *)
  let erase_identifier_info i = i.S.kind in
  let locals =
    S.Symb_Tbl.fold
      (fun id info tbl ->
	T.Symb_Tbl.add id (erase_identifier_info info) tbl)
      f.S.locals
      T.Symb_Tbl.empty
  in
  
  let code = erase_block f.S.code in
  
  { locals; T.formals = List.length f.formals; code }    

let erase_program p =
  List.map (fun (id, f) -> (id, erase_function p f)) p
