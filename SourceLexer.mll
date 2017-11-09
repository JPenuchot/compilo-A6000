{

  open Lexing
  open SourceParser

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [	
          "integer",  INT;
	        "print",    PRINT;
          "while",    WHILE;
          "for",      FOR;
          "if",       IF;
          "then",     THEN;
          "else",     ELSE;
          "var",      VAR;
          "integer",  INT;
          "boolean",  BOOL;
          "true",     BOOLVAL(true);
          "false",    BOOLVAL(false);
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*
let integer = (digit)*

rule token = parse
  | ['\n']
      { token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  (*| "true" | "false"*)
  (*    { BOOLVAL(bool_of_string (lexeme lexbuf)) }*)
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | integer
      { INTVAL(int_of_string (lexeme lexbuf)) }
  | "("
      { BEGIN }
  | ")"
      { END }
  | ";"
      { SEMI }
  | ","
      { COMMA }
  | "+"
      { ADD }
  | "-"
      { SUB }
  | "*"
      { MULT }
  | "/"
      { DIV }
  | "=="
      { EQ }
  | "!="
      { NEQ }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "<"
      { LT }
  | "<="
      { LE }
  | "=<"
      { LE }
  | ":="
      { AFFECT }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }

and comment = parse
  | "(*"
      { comment lexbuf; comment lexbuf }
  | "*)"
      { () }
  | _
      { comment lexbuf }
  | eof
      { failwith "Unterminated comment" }
