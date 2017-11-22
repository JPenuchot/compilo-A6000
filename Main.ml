open Format

let usage = "usage: compilo [options] file.a6m"

(* Par défaut : génération de code MIPS sans optimisation. *)
let interpret      = ref false
let reg_allocation = ref false
let dead_code_elim = ref false
let input = ref 0

let spec =
  [ "-r", Arg.Set reg_allocation, "  with register allocation";
    "-dce", Arg.Set dead_code_elim, "  with dead code elimination";
    "-O", Arg.Tuple [Arg.Set reg_allocation; Arg.Set dead_code_elim],
    "  full optimisation";
    "-i", Arg.Tuple [Arg.Set_int input; Arg.Set interpret],
    "  interpreter only";
  ]

let file = 
  let file = ref None in
  let set_file s =  
    if not (Filename.check_suffix s ".a6m") then 
      raise (Arg.Bad "no .a6m extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  print_string "Let's begin !\n";
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let p  = SourceParser.prog SourceLexer.token lb in
  close_in c;
  print_string "Parsing, done.\n";
  (*SourceTypeChecker.typecheck_prog p;*)
  print_string "Type checking, done.\n"; 
  if !interpret
  then let _ = SourceInterpreter.eval_prog p !input in ()
  else begin
    let p = print_string "SourceToUntyped...\n";SourcetoUntyped.erase_prog p in
    let p = UntypedtoGoto.destructure_prog p in
    let p = GototoIr.flatten_prog p in

    (* Dead code elimination *)
    let p =
      if   !dead_code_elim
      then IrDeadCodeElim.dce p (* TODO *)
      else p
    in

    let p = IrtoAllocated.allocate_prog !reg_allocation p in
    let asm = AllocatedtoMips.generate_prog p in (* TODO *)
    let output_file = (Filename.chop_suffix file ".a6m") ^ ".asm" in
    let out = open_out output_file in
    let outf = formatter_of_out_channel out in
    Mips.print_program outf asm;
    pp_print_flush outf ();
    close_out out;
  end;
  exit 0
