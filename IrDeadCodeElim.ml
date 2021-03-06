open IrAst
open IrLiveness

(* Élimination des instructions mortes.
   Renvoie un couple [(f, p)] où [p] est le programme simplifié, et
   où le booléen [b] vaut [true] si au moins une instruction a été
   éliminée.
     [dce_step: IrAst.main -> bool * IrAst.main]
*)
let dce_step p =

  (* Calcul des informations de vivacité *)
  let _, lv_out = mk_lv p in

  (* À partir d'une instruction et de son étiquette, répond [true]
     si l'instruction est vivante, et [false] sinon.
       [live_instr: IrAst.label * IrAst.instruction -> bool]

     Une instruction morte est une instruction affectant une valeur à
     un registre virtuel qui n'est pas vivant en sortie de cette instruction.
     Toutes les autres sont vivantes.
  *)
  let live_instr = function
    | (lab, ins) ->
      match ins with
      | Value(i,_) | Binop(i,_,_,_) ->
        (VarSet.diff
           (VarSet.singleton i) (Hashtbl.find lv_out lab)) == VarSet.empty
      | _ -> true
  in
  (* Filtre la liste pour ne garder que les instructions vivantes *)
  let filtered_code = List.filter live_instr p.code in
  (* Renvoie le booléen et le code simplifié *)
  List.length p.code <> List.length filtered_code, { p with code=filtered_code }


(* Élimination itérée *)
let rec dce_func p =
  let ismodified, np = dce_step p in
  if ismodified then
    dce_func np
  else
    np

let dce p = Symb_Tbl.map dce_func p