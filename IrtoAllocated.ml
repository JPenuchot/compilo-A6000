module S = IrAst
module T = AllocatedAst

module NodeMap = GraphColoring.NodeMap
    
(**
   Transformation des couleurs en allocations
   Les couleurs sont représentées par des entiers
*)
let color_to_alloc i =
  if i < 8
  then T.Reg (Printf.sprintf "$t%d" (i+2))
  else T.Stack ((i-7)*(-4))

(**
   Renvoie le décalage associé à la plus grande couleur utilisée
   Note : cette information n'est pas réellement utile dans le fragment
   A6000, mais peut servir dans la deuxième séquence.
*)
let max_offset coloring =
  let max_color = GraphColoring.max_color coloring in
  match color_to_alloc(max_color) with
    | T.Reg _   -> 0
    | T.Stack o -> o
    
(** Allocation *)
let allocate_function reg_flag f =
  let current_offset = ref 0 in

  let tbl =
    if reg_flag
    then
      (* Avec allocation de registres *)
      let g = IrInterferenceGraph.interference_graph f in
      let coloring = GraphColoring.colorize g in
      let _ = current_offset := max_offset coloring in
      S.Symb_Tbl.mapi (fun id (info: S.identifier_info) ->
      	match info with
	  | Return   -> T.Stack (8+4*f.formals)
      	  | Formal k -> T.Stack (8+4*(f.formals - k))
      	  | Local    -> let color = NodeMap.find id coloring in
      		        color_to_alloc color
      ) f.locals
    else
      (* Tout sur la pile *)
      S.Symb_Tbl.mapi (fun id (info: S.identifier_info) ->
	match info with
	  | Return   -> T.Stack (8+4*f.formals)
	  | Formal k -> T.Stack (8+4*(f.formals - k))
	  | Local    -> (current_offset := !current_offset - 4;
		         T.Stack !current_offset)
      ) f.S.locals
  in

  { T.locals = tbl; T.offset = !current_offset; T.code = f.S.code }


let allocate_program reg_flag p =
  List.map (fun (id, info) -> id, allocate_function reg_flag info) p
