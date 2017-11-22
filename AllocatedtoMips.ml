open AllocatedAst
open Mips

(* Génère un nom de label pour un saut
 * (pour éviter de confondre avec les sauts dans les fonctions) *)
let fun_id_to_jal_label name = "func_" ^ name

(* Génération des fonctions *)
let generate_func p f fname =
  (* Affecte des emplacements mémoire aux variables locales. *)
  let sp_off   = f.offset in
  let symb_tbl = f.locals in
  let find_alloc id =
    try  AllocatedAst.Symb_Tbl.find id symb_tbl
    with Not_found -> failwith (Printf.sprintf "Node %s not found" id)
  in

  let rec generate_block = function
    | []       -> nop
    | (l,i)::b -> comment l @@ generate_instr i @@ generate_block b

  (* Un appel [load_literal r v] génère du code qui place la valeur
       immédiate [v] dans le registre [r]. *)
  and load_literal r : AllocatedAst.literal -> 'a Mips.asm = function
    | Int(i)  -> li r i
    | Bool(b) -> let i = if b then -1 else 0 in li r i

  (* Un appel [load_value r v] génère du code qui place la valeur [v]
     dans le registre [r]. *)
  and load_value r : AllocatedAst.value -> 'a Mips.asm = function
    | Literal(lit)   -> load_literal r lit
    | Identifier(id) -> (match find_alloc id with
        | Reg r'  -> move r r'
        | Stack o -> lw r o ~$fp)

  and load_operand r : AllocatedAst.value -> Mips.register * 'a Mips.asm =
    function
    | Literal(lit)   -> r, load_literal r lit
    | Identifier(id) ->
      (match find_alloc id with
       | Reg r'  -> r', nop
       | Stack o -> r, lw r o ~$fp
      )
  (* Spécialisations pour les premier et second opérandes d'une opération. *)
  and load_first_operand v  = load_operand ~$t0 v
  and load_second_operand v = load_operand ~$t1 v

  and generate_instr : AllocatedAst.instruction -> 'a Mips.asm = function
    | Value(dest, v) ->
      (match find_alloc dest with
       | Reg r   -> load_value r v
       | Stack o -> let r, a = load_first_operand v in
         a @@ sw r o ~$fp
      )

    | Binop(dest, op, o1, o2) ->
      let op = match op with
        | Add  -> add
        | Sub  -> sub
        | Mult -> mul
        | Div  -> div
        | Eq   -> seq
        | Neq  -> sne
        | Lt   -> slt
        | Le   -> sle
        | And  -> and_
        | Or   -> or_
      in
      let (r1, a1) = load_first_operand o1 in
      let (r2, a2) = load_second_operand o2 in
      let aop = match find_alloc dest with
        | Reg r   -> op r r1 r2
        | Stack o -> op ~$t0 r1 r2 @@ sw ~$t0 o ~$fp
      in
      a1 @@ a2 @@ aop

    | Print(v)        -> load_value ~$a0 v @@ li ~$v0 11 @@ syscall
    | Label(lab)      -> label lab
    | Goto(lab)       -> b lab
    | CondGoto(v,lab) -> load_value ~$t0 v @@ bnez ~$t0 lab
    | Comment(s)      -> comment s

    (* TODO *)
    | FunCall(dest, name, vals) ->
      (* Appel de fonction *)
      let f = Symb_Tbl.find name p in
      let has_res = match Symb_Tbl.find_opt "result" p with
        | None -> false | _ -> true in
      (* Allocation de l'emplacement de la valeur de retour *)
      let res_alloc = (if has_res then addi sp sp 4 else nop) in

      let param_offset = (if has_res then 4 else 0)
      and num_formals = List.fold_left (fun acc _ -> acc + 1) 0 vals in

      (* Allocation des emplacements des paramètres *)
      let param_alloc = addi sp sp (4 * num_formals) in

      (* Affectation des valeurs des paramètres *)
      let affect_vals, _ = List.fold_left (
          fun (inst, cnt) value ->
            (inst
             (* On charge la valeur *)
             @@ (load_value ~$t0 value)
             (* On la place où il faut *)
             @@ sw t0 cnt fp,
             cnt + 4)
        ) ((nop), param_offset + 4) vals in

      (* Compilation de la phase pré-appel *)
      let before = res_alloc @@ param_alloc @@ affect_vals in

      (* Dépilement des paramètres formels de la fonction appelée *)
      let unstack_param = addi sp sp (4 * num_formals) in
      let aff_param =
        match find_alloc dest with
        | Reg r   -> lw r 0 sp
        | Stack o -> lw t0 0 sp @@ sw t0 o fp
      in
      let restore = addi sp sp (param_offset + (4 * num_formals)) in

      before @@ jal (fun_id_to_jal_label name) @@ aff_param @@ restore

    | ProcCall(name, vals) ->
      (* Appel de procédure (sans prendre en compte le retour éventuel) *)
      let f = Symb_Tbl.find name p in
      let has_res = match Symb_Tbl.find_opt "result" p with
        | None -> false | _ -> true in
      (* Allocation de l'emplacement de la valeur de retour *)
      let res_alloc = (if has_res then addi sp sp 4 else nop) in

      let param_offset = (if has_res then 4 else 0)
      and num_formals = List.fold_left (fun acc _ -> acc + 1) 0 vals in

      (* Allocation des emplacements des paramètres *)
      let param_alloc = addi sp sp (4 * num_formals) in

      (* Affectation des valeurs des paramètres *)
      let affect_vals, _ = List.fold_left (
          fun (inst, cnt) value ->
            (inst
             (* On charge la valeur *)
             @@ (load_value ~$t0 value)
             (* On la place où il faut *)
             @@ sw t0 cnt fp,
             cnt + 4)
        ) ((nop), param_offset + 4) vals in

      (* Compilation de la phase pré-appel *)
      let before = res_alloc @@ param_alloc @@ affect_vals in

      (* Dépilement des paramètres formels de la fonction appelée *)
      let restore = addi sp sp (param_offset + (4 * num_formals)) in

      before @@ jal (fun_id_to_jal_label name) @@ restore
  in

  (* Génération d'une fonction *)
  if fname <> "main" then
    (* Retourne la taille de la pile de f + 4 *)
    let sp_shift =
      (* Nb var locales + 4 *)
      Symb_Tbl.cardinal f.locals + 4
    in

    (* Sauvegarde de $ra et $old_fp *)
    let init =
      label (fun_id_to_jal_label fname)
      @@ addi sp sp (-8) (* $sp pointe au sommet de la pile de l'appelant,
                          * on l'incrémente de 2 cases (8 octets) pour stocker
                          * $ra et old_$fp. *)

      @@ sw ra 0 sp     (* $sp pointe où fp devra pointer, CàD à l'emplacement
                         * de sauvegarde de $ra. *)

      @@ sw fp 4 sp     (* Une case (4 octets) au-dessus on stocke old_$fp. *)
      @@ addi fp sp 0   (* $fp <- $sp *)
      @@ addi sp fp sp_shift (* On alloue enfin la place nécessaire pour les
                              * variables locales de la fonction. *)


    (* Dépilement, restauration des registres *)
    and close =
      addi sp fp 0        (* On dépile tout. *)
      @@ lw ra 0 sp       (* On restaure $ra *)
      @@ lw fp 4 sp       (* On restaure $fp avec old_$fp *)
      @@ addi sp sp (-8)  (* On désalloue $ra et old_$fp de la pile *)
      @@ jal ra           (* On retourne à l'appelant *)
    in init @@ (generate_block f.code) @@ close

  else  (* Cas du main *)
    let init_main =
      move fp sp
      @@ addi fp fp (-4)
      @@ lw a0 0 a1
      @@ jal "atoi"
      @@ sw v0 0 fp
      @@ addi sp sp sp_off

    and close_main = li v0 10 @@ syscall in

    (* TODO : Un peu plus de checks sur les paramètres formels du main... *)

    init_main
    @@ (generate_block (Symb_Tbl.find "main" p).code)
    @@ close_main

let generate_prog p =
  let main = generate_func p (Symb_Tbl.find "main" p) "main" in
  (*let new_p = Symb_Tbl.remove "main" p in*)
  (*let functions =*)
  (*  Symb_Tbl.fold (fun key e acc ->*)
  (*      acc @@ (generate_func new_p e key)*)
  (*    ) main*)
  (*in*)
  (*and functions = generate_block f.code*)

  let built_ins =
    label "atoi"
    @@ move t0 a0
    @@ li   t1 0
    @@ li   t2 10
    @@ label "atoi_loop"
    @@ lbu  t3 0 t0
    @@ beq  t3 zero "atoi_end"
    @@ li   t4 48
    @@ blt  t3 t4 "atoi_error"
    @@ li   t4 57
    @@ bgt  t3 t4 "atoi_error"
    @@ addi t3 t3 (-48)
    @@ mul  t1 t1 t2
    @@ add  t1 t1 t3
    @@ addi t0 t0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra
  in
  { text = main (*@@ functions*) @@ built_ins; data = nop }