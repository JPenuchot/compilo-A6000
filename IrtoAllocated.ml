module S = IrAst
module T = AllocatedAst
module IG = IrInterferenceGraph
module GC = GraphColoring

(* Allocation *)
let allocate_main reg_flag p =
  let current_offset = ref 0 in

  let tbl =
    if reg_flag then
      let ig = IG.interference_graph p in
      Graph.dump ig;
      failwith "L'allocation des registres marche pas, ok ? \
      Pas la peine d'essayer."

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
