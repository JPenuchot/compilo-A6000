open SourceAst

(* Rapports d'erreurs *)
exception Type_error of typ * typ
exception Call_error of typ list * typ list
exception Untyped_expression_error

(* comparetype: typ -> typ -> unit
   Lève une exception si les types diffèrent. *)
let comparetype t1 t2 =
  if t1 <> t2
  then raise (Type_error(t1, t2))

(* Vérification des types d'un programme *)
let typecheck_func prog f =

  (* L'analyse de tout le programme se fait dans le contexte de la même
     table de symboles. On la définit ici puis on définit les fonctions
     d'analyse récursives dans ce contexte. *)
  let symb_tbl = f.locals in

  (* [typecheck_block/instruction] vérifient le bon typage des instructions
     et lèvent une exception en cas de problème. *)
  (* typecheck_block: block -> unit *)
  let rec typecheck_block b = List.iter typecheck_instruction b

  (* typecheck_instruction: instruction -> unit *)
  and typecheck_instruction = function
    | Set(l, e) ->
      comparetype (type_location l) (type_expression e)

    | While(e, b) ->
      comparetype TypBoolean (type_expression e);
      typecheck_block b

    | If(e, b1, b2) ->
      comparetype TypBoolean (type_expression e);
      typecheck_block b1;
      typecheck_block b2;

    | Print(e) ->
      comparetype TypInteger (type_expression e)

    | Call(c) ->
      typecheck_call c

  (* Vérifie le typage d'un appel de fonction *)
  and typecheck_call (name, args) =
    (* Faire le match entre les types des arguments de la fonction et
       les arguments donnés à la fonction. *)
    let formals = (Symb_Tbl.find name prog).formals
    in let rec argcmp form exps =
         match form, exps with
         | f::ftl, e::etl -> 
           (if f <> (type_expression e) then raise
                (Call_error(form, (List.rev_map (type_expression) exps))));
           argcmp ftl etl
         | [], [] -> ()
         | [], _ | _,  [] ->
           (raise (Call_error (form, (List.rev_map (type_expression) exps))))
    in argcmp formals args

  (* [type_expression/literal/location] vérifient le bon typage des
     expressions et renvoient leur type. *)
  (* type_expression: expression -> typ *)
  and type_expression = function
    | Literal(lit)  -> type_literal lit

    | Location(loc) -> type_location loc

    | Binop(op, e1, e2) ->
      let ty_op, ty_r = type_binop op in
      comparetype ty_op (type_expression e1);
      comparetype ty_op (type_expression e2);
      ty_r

    | Call(name, exps) ->
      let t = match (Symb_Tbl.find name prog).return with
        | Some(t) -> t
        | _ -> raise Untyped_expression_error

      (* Vérifier le typage des expressions *)
      in List.iter (fun e -> let _ = (type_expression e) in ()) exps;
      (* Retourner le type de la fonction *)
      t

  (* type_literal: literal -> typ *)
  and type_literal = function
    | Int _  -> TypInteger
    | Bool _ -> TypBoolean

  (* type_location: location -> typ *)
  and type_location = function
    | Identifier(id) -> (Symb_Tbl.find id symb_tbl).typ

  (* [type_binop] renvoie le type des opérandes et le type du résultat
     d'un opérateur binaire. *)
  (* type_binop: binop -> typ * typ *)
  and type_binop = function
    | Add | Sub | Mult | Div  -> TypInteger, TypInteger
    | Eq  | Neq | Lt   | Le   -> TypInteger, TypBoolean
    | And | Or                -> TypBoolean, TypBoolean

  in typecheck_block f.code

let typecheck_prog p =
  Symb_Tbl.iter (fun _ f -> (let _ = typecheck_func p f in ())) p