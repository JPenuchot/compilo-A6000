(**
   Les couleurs sont des entiers.
   Une coloration associe les identifiants à des couleurs.
*)
module NodeMap = Map.Make(String)
type color = int
type coloring = color NodeMap.t

(** Plus grande couleur utilisée dans une coloration *)
let max_color coloring =
  NodeMap.fold (fun _ c mc -> max c mc) coloring 0

(** Renvoie le plus petit entier positif ou nul vérifiant le prédicat passé
    en paramètre *)
let find_min p =
  let rec find_min_aux k =
    if p k
    then k
    else find_min_aux (k+1)
  in
  find_min_aux 0

(** Étant donnés un graphe partiellement coloré et un sommet [n], renvoie la
    plus petite couleur pouvant être affectée à [n] *)
let pick_color g coloring n =
  let nbg = Graph.neighbours g n in
  let nbg_colors =
    List.map (fun n -> NodeMap.find n coloring) nbg
  in
  find_min (fun c -> not (List.mem c nbg_colors))

(** Renvoie une coloration pour le graphe [g]. *)
let rec colorize (g : Graph.t) : coloring =
  match Graph.min_degree g with
    | None   -> NodeMap.empty
    | Some n -> let g' = Graph.del_node g n in
		let coloring = colorize g' in
		let c = pick_color g coloring n in
		NodeMap.add n c coloring

