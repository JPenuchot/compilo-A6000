open AllocatedAst
open Mips

let generate_main p =

  (* Affecte des emplacements mémoire aux variables locales. *)
  let sp_off   = p.offset in
  let symb_tbl = p.locals in
  let find_alloc id =
    try  AllocatedAst.Symb_Tbl.find id symb_tbl
    with Not_found -> failwith (Printf.sprintf "Node %s not found" id)
  in
  
  let rec generate_block = function
    | []       -> nop
    | (l,i)::b -> comment l @@ generate_instr i @@ generate_block b

  (* Un appel [load_value r v] génère du code qui place la valeur [v]
     dans le registre [r]. *)
  and load_value r : AllocatedAst.value -> 'a Mips.asm = function
    | Identifier(id) -> (match find_alloc id with
	| Stack o -> lw r o ~$fp
	| Reg r1 -> move r r1)
    | Literal(Int(i)) -> li r i
    | Literal(Bool(true)) -> li r 1
    | Literal(Bool(false)) -> li r 0

  and generate_instr : AllocatedAst.instruction -> 'a Mips.asm = function
    | Print(v)  -> load_value ~$a0 v @@ li ~$v0 11 @@ syscall
    | Label(l)  -> label l
    | Goto(l)   -> b l
    | CondGoto(v, e) -> load_value ~$t0 v @@ bnez ~$t0 e
    | Comment(s) -> comment s
    | Binop(i, b, v0, v1) -> load_value ~$t0 v0
                          @@ load_value ~$t1 v1
                          @@ (match b with
                              | Add   -> add
                              | Div   -> div
                              | Mult  -> mul
                              | Sub   -> sub
                              | Eq    -> seq
                              | Neq   -> sne
                              | Lt    -> slt
                              | Le    -> sle
                              | And   -> and_
                              | Or    -> or_
                            ) ~$t0 ~$t0 ~$t1
                          @@ (match find_alloc i with
                              | Stack(o) -> sw ~$t0 o ~$fp
                              | Reg(s) -> move s ~$t0
                            )

    | Value(i, v) ->  match find_alloc i with
                      | Stack(o) -> load_value ~$t0 v
                                 @@ sw ~$t0 o ~$fp
                      | Reg(s) -> load_value s v
  in

  let init =
    move fp sp
    @@ addi fp fp (-4)
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ sw v0 0 fp
    @@ addi sp sp sp_off
  in

  let close = li v0 10 @@ syscall in
  
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
  
  let asm = generate_block p.code in
  { text = init @@ asm @@ close @@ built_ins; data = nop }
