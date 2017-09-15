(* Transformation de la syntaxe abstraite non typée
   vers la syntaxe abstraite "goto". *)
module S = UntypedAst
module T = GotoAst

let destructure_main p =

  (* new_label: unit -> string *)
  (* Un appel [new_label()] crée une nouvelle étiquette qui peut être
     utilisée pour créer des sauts. *)
  let new_label =
    let cpt = ref 0 in
    fun () -> incr cpt; Printf.sprintf "_label_main_%i" !cpt
  in

  (* destructure_block: S.block -> T.block *)
  let rec destructure_block = function
    | []     -> []
    | i :: b -> destructure_instruction i @ (destructure_block b)
      
  (* destructure_instruction: S.instruction -> T.block *)
  and destructure_instruction : S.instruction -> T.block = function
    | Print(e)  -> [ T.Print(e)  ]
    | Set(l, e) -> [ T.Set(l, e) ]
    
    | While(e, b) ->
      
      let test_label = new_label()
      and code_label = new_label()
    in
      [ T.Goto(test_label);
        T.Label(code_label) ]
      @ (destructure_block b)
      @ [ T.Label(test_label);
          T.CondGoto(e, code_label) ]
    
    | If(e, b1, b2) ->

      let b1_label = new_label()
      and end_label = new_label()
    in
      [ T.CondGoto(e, b1_label) ]
      @ (destructure_block b2)
      @ [ T.Goto(end_label);
          T.Label(b1_label)]
      @ (destructure_block b1)
      @ [ T.Label(end_label) ]
  in

  { T.locals = p.S.locals; T.code = destructure_block p.S.code }
