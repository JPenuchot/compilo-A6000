open IrAst
open IrLiveness

(**
   Construction du graphe d'interférence :

   1. Pour chaque instruction définissant une variable a, hors copies,
   où les variables vivantes en sortie sont b₁, ..., bₖ, ajouter les
   arêtes (a, b₁), ..., (a, bₖ).

   2. Pour chaque instruction de copie a ← c, où les variables vivantes
   en sortie sont b₁, ..., bₖ, ajouter les arêtes (a, b₁), ..., (a, bₖ)
   pour les bᵢ distincts de c.
*)

(* Fonction auxiliaire : ajoute à un graphe l'ensemble des interférences
   dues à une instruction donnée, connaissant l'ensemble des variables
   vivantes en sortie de cette instruction. *)
let add_interferences g lv_out_at_node = function  
  | Value(a, Identifier b) ->
    (* Copie : ne pas introduire de conflit entre [a] et [b]. *)
    VarSet.fold (fun c g ->
      if b = c
      then g
      else Graph.add_edge g a c
    ) lv_out_at_node g
      
  | Value(a, _)
  | Binop(a, _, _, _) ->
    (* Définition normale. *)
    VarSet.fold (fun b g ->
      Graph.add_edge g a b
    ) lv_out_at_node g
      
  | _ -> g

(* Fonction principale, qui itère sur l'ensemble des points du programme. *)
let interference_graph p : Graph.t =
  (* D'abord, définir le graphe sans arêtes contenant un sommet pour chaque
     identifiant de la table des symboles. *)
  let g =
    Symb_Tbl.fold (fun id _ g -> Graph.add_node g id) p.locals Graph.empty
  in

  (* Ensuite, récupérer le résultat de l'analyse de vivacité. *)
  let _, lv_out = mk_lv p in

  (* Enfin, itérer sur l'ensemble des points du programme. *)
  List.fold_left (fun g (lab, i) ->
    let lv_out_at_node = Hashtbl.find lv_out lab in
    add_interferences g lv_out_at_node i
  ) g p.code
