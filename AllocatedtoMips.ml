open AllocatedAst
open Mips

let push r = sw r 0 sp @@ addi sp sp (-4)
let pop  r = addi sp sp 4 @@ lw r 0 sp	

let generate_function f =

  (* Affecte des emplacements mémoire aux variables locales. *)
  let sp_off = f.offset in
  let symb_tbl = f.locals in
  let find_alloc id =
    try  AllocatedAst.Symb_Tbl.find id symb_tbl
    with Not_found -> failwith (Printf.sprintf "Node %s not found" id)
  in
  
  let rec generate_block = function
    | []       -> Nop
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

  (* Variante optimisée de [load_value], qui place la valeur dans le
     registre [r] sauf si cette valeur est déjà stockée dans un
     registre [r'].
     Renvoie le code généré et le registre de destination effectif. *)
  and load_operand r : AllocatedAst.value -> Mips.register * 'a Mips.asm =
    function
      | Literal(lit)   -> r, load_literal r lit
      | Identifier(id) ->
	(match find_alloc id with
	  | Reg r'  -> r', Nop
	  | Stack o -> r, lw r o ~$fp
	)
  (* Spécialisations pour les premier et second opérandes d'une opération. *)
  and load_first_operand v  = load_operand ~$t0 v
  and load_second_operand v = load_operand ~$t1 v

  (* Un appel [to_dest dest r] génère du code qui place la valeur du
     registre [r] dans l'emplacement de destination [dest]. *)
  and to_dest dest r =
    match find_alloc dest with
      | Reg r'  -> move r' r
      | Stack o -> sw r o fp

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
	| Mult -> mul
	| Sub  -> sub
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

    (* [ProcCall] et [FunCall] utilisent tous deux [generate_call]. *)
    | ProcCall(c)   -> generate_call c
    | FunCall(dest, c) ->
      let d = match find_alloc dest with
	| Reg r   -> pop r
	| Stack o -> pop t0 @@ sw t0 o fp
      in
      push zero            (* Avant l'appel, alloue une case pour le résultat *)
      @@ generate_call c   (* Appel *)
      @@ d                 (* Récupération du résultat *)

    (* [Load] et [Store] utilisent la même fonction [generate_access] pour
       calculer l'adresse à laquelle lire ou écrire. *)
    | Load(dest, a) ->
      generate_access a                (* Calcul d'adresse *)
      @@ lw t0 0 t0 @@ to_dest dest t0 (* Lecture          *)
    | Store(a, v) ->
      generate_access a                (* Calcul d'adresse *)
      @@ load_value t1 v @@ sw t1 0 t0 (* Écriture         *)

    (* [New] alloue et initialise un bloc avec un nombre de mots donné *)
    | New(dest, v) ->
      load_value t0 v @@ li t1 4 @@ mul a0 t0 t1 (* Calcul de taille *)
      @@ li v0 9 @@ syscall                      (* Allocation       *)
      @@ sw t0 0 v0                              (* Entête           *)
      @@ to_dest dest v0                         (* Retour           *)
	
  (* Appel de fonction *)
  (* Note : cette version simple n'est pas compatible avec l'allocation de
     registres. Pour cela, il faudrait avant l'appel effectuer des sauvegardes
     des registres utilisés, puis après l'appel les restaurer. *)
  and generate_call (id, args) =
    generate_args args      (* On place les paramètres effectifs sur la pile *)
    @@ jal id               (* Appel                                         *)
    @@ addi sp sp (4*(List.length args)) (* Après l'appel, on libère la pile *)

  and generate_args = function
    | []      -> nop
    | v::args -> load_value t0 v @@ push t0 @@ generate_args args

  (* Calcul d'adresse v2[v1] = v1 + 4*(v2+1) *)
  and generate_access (v1, v2) =
    load_value t0 v2 @@ li t1 4 @@ mul t0 t0 t1  (* Calcul du décalage    *)
    @@ load_value t1 v1 @@ add t0 t0 t1          (* Combinaison avec base *)
      
  in

  (* On organise le code d'une fonction en trois partie : [init], [code]
     et [close] *)
  (* Convention d'appel : l'appelé, au début *)
  let function_init =
    push fp
    @@ push ra
    @@ addi fp sp 4
    @@ addi sp sp sp_off
  in
  (* Corps de la fonction *)
  let function_code = generate_block f.code in
  (* Convention d'appel : l'appelé, à la fin *)
  let function_close =
    addi sp sp (-sp_off)
    @@ pop ra
    @@ pop fp
    @@ jr ra
  in
  function_init @@ function_code @@ function_close

let generate_program p =
    
  let init =
    lw a0 0 a1
    @@ jal "atoi"
    @@ push v0
    @@ jal "main"
    @@ li v0 10
    @@ syscall
  in

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
  
  let asm = List.fold_right (fun (id, f_info) asm ->
    label id @@ generate_function f_info @@ asm
  ) p nop
  in
  { text = init @@ asm @@ built_ins; data = Nop }
