module S = IrAst
module T = AllocatedAst
module IG = IrInterferenceGraph

(* Allocation *)
let allocate_main reg_flag p =
  let current_offset = ref 0 in

  let tbl =
    if reg_flag then
      (* Initialisation du graphe *)
      let ig = IG.interference_graph p in
      failwith "ok"

      (* Création des relations *)
      
      (* Coloration du graphe *)

      (* Assignation des emplacements de variables *)

    else
      (* Tout sur la pile *)
      S.Symb_Tbl.mapi (fun id (info: S.identifier_info) ->
	match info with
	  | FormalX ->
        T.Stack (0);
	  | Local   ->
        (current_offset := !current_offset - 4);
        T.Stack !current_offset
      ) p.S.locals
  in
  { T.locals = tbl; T.offset = !current_offset; T.code = p.S.code }
