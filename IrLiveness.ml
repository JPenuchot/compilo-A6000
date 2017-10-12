open IrAst

(* Création du graphe de flot de contrôle, sous la forme d'une table associant
   à chaque étiquette d'un point de programme les étiquettes de ses successeurs.
     [mk_succ: IrAst.block -> (IrAst.label * IrAst.label) Hashtbl.t]

   Note à propos des tables [Hashtbl] de Caml :

   - un appel [Hashtbl.add tbl k v] ajoute à la table [tbl] une association
     entre la clé [k] et la valeur [v], sans effacer définitivement une
     éventuelle association entre [k] et une autre valeur [v']

   - un appel [Hashtbl.find tbl k] renvoie la dernière valeur associée à
     la clé [k] ; lève l'exception [Not_found] s'il n'y a pas de valeur
     associée à [k]

   - un appel [Hashtbl.find_all tbl k] renvoie la liste de toutes les valeurs
     associées à la clé [k] ; renvoie la liste vide s'il n'y a aucune valeur
     associée

   - un appel [Hashtbl.replace tbl k v] ajoute à la table [tbl] une association
     entre la clé [k] et la valeur [v], en effaçant une éventuelle association
     précédente
*)

let mk_succ code =
  (* Création avec une capacité arbitraire ; la table sera étendue au besoin *)
  let succ = Hashtbl.create 257 in

  (* Parcours du code du programme et remplissage à la volée de la table *)
  let rec mk_succ : IrAst.block -> unit = function
    | (lab, Goto(       target_lab)) :: code
    | (lab, CondGoto(_, target_lab)) :: code ->

      Hashtbl.add succ lab target_lab;
      (
        match code with
        | (labn, next) :: tl -> Hashtbl.add succ lab labn; mk_succ code
        | [] -> ()
      )
    | (lab, _) :: code ->
      (
        match code with
        | (labn, next) :: tl -> Hashtbl.add succ lab labn; mk_succ code
        | [] -> ()
      )
    | [] -> ()
  in
  mk_succ code;
  (* À la fin, on renvoie la table qu'on a remplie *)
  succ


(* Définition des ensembles d'identifiants
   - le type d'un ensemble d'identifiants est [VarSet.t]
   - la constante [VarSet.empty] désigne l'ensemble vide
   - un appel [VarSet.singleton id] renvoie l'ensemble contenant uniquement
     l'identifiant [id]
   - les fonctions [VarSet.union] et [VarSet.diff] prennent en paramètres deux
     ensembles, et renvoient respectivement leur union et leur différence
     ensemblistes.
*)
module VarSet = Set.Make(String)

(* Fonction principale, renvoie deux tables associant à chaque étiquette d'un
   point de programme l'ensemble des variables vivantes en entrée/en sortie.
     [mk_lv: IrAst.main ->
      ((IrAst.label, VarSet.t) Hashtbl.t) * ((IrAst.label, VarSet.t) Hashtbl.t)]
*)
let mk_lv p =

  (* Création des deux tables destinées à accumuler le résultat, et
     calcul du flot de contrôle. *)
  let code = p.code in
  let lv_in  = Hashtbl.create 257
  and lv_out = Hashtbl.create 257
  and succ = mk_succ code
  in

  (* Initialisation des tables [lv_in] et [lv_out],
     associe [VarSet.empty] à chaque point de programme. *)
  List.iter (fun (lab, _) ->
      Hashtbl.add lv_in  lab VarSet.empty;
      Hashtbl.add lv_out lab VarSet.empty
    ) code;

  (* Les fonctions [lv_gen] et [lv_kill] prennent en paramètre une instruction
     et indiquent respectivement l'ensemble des variables vivantes qu'elle
     crée ou tue.
       [lv_gen:  IrAst.instruction -> VarSet.t]
       [lv_kill: IrAst.instruction -> VarSet.t]
  *)
  let rec lv_gen : IrAst.instruction -> VarSet.t = function
    | Binop(_, _, v1, v2) ->
      let helper = function
        | Literal(_) -> VarSet.empty
        | Identifier(i) -> VarSet.singleton i
      in VarSet.union (helper v1) (helper v2)
    | Print(Identifier(v)) | CondGoto(Identifier(v),_) | Value(_, Identifier(v))
      -> VarSet.singleton v
    | _ -> VarSet.empty

  and lv_kill : IrAst.instruction -> VarSet.t = function
    | Binop(i,_,_,_)
    | Value(i,_)      -> VarSet.singleton i
    | _        -> VarSet.empty
  in

  (* Booléen qu'on met à [true] lorsque les tables [lv_in] et [lv_out] sont
     encore en train de changer. Il est initialisé à [true] car à l'origine il
     y a bien du calcul à faire, mais il sera repassé à [false] avant chaque
     itération, pour n'être remis à [true] que si des changements sont
     observés. *)
  let change = ref true in

  (* Un appel [lv_step_instruction (lab, instr)] met à jour les entrées pour
     le point de programme [lab] (comportant l'instruction [instr]) dans les
     tables [lv_in] et [lv_out], en appliquant les équations de flot de données.
     Rappel :
        In[lab]  =  (Out[lab] \ Kill[instr]) ∪ Gen[instr]
       Out[lab]  =  ⋃ᵣ In[r]                                r ∈ Succ[lab]

     Cette fonction doit aussi faire passer le booléen [change] à [true] si
     les valeurs In[lab] et Out[lab] ont été modifiées.
  *)
  let lv_step_instruction (lab, instr) =
    (* Récupération de la liste des successeurs *)
    let succs = Hashtbl.find_all succ lab
    (* Récupération de la liste des gens/kills *)
    and gens = lv_gen instr
    and kills = lv_kill instr
    (* Récupération des ins/outs DANS LES TABLES *)
    and stored_outs = Hashtbl.find lv_out lab
    and stored_ins = Hashtbl.find lv_in lab
    in
    let nouts =  List.fold_left (fun acc succ ->
        VarSet.union acc (Hashtbl.find lv_out succ) ) VarSet.empty succs
    in
    let nins = VarSet.union (VarSet.diff nouts kills) gens
    in
    change := (!change) || nins <> stored_ins || nouts <> stored_outs;
    (* Update tables *)
    Hashtbl.remove lv_out lab;
    Hashtbl.add lv_out lab nouts;
    Hashtbl.remove lv_in lab;
    Hashtbl.add lv_in lab nins
  in

  (* Une passe complète : met à jour une fois chaque instruction *)
  let lv_step_main () =
    List.iter lv_step_instruction (List.rev code)
  in
  (* Répéter tant qu'il reste des changements *)
  while !change do
    change := false;
    lv_step_main ();
  done;
  (* Enfin, renvoyer les versions finales des tables *)
  lv_in, lv_out
