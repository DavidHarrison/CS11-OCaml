(*
 * lexer.mll
 *
 *     bogoscheme Lexer.
 *
 *)

{
open Parser
}

(* Some useful definitions. *)
let whitespace = [' ' '\t' '\n']
let integer    = '-'? ['0' - '9']+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']

(* The lexer definition itself. *)
rule lex = parse
  | ';' [^ '\n']*   { lex lexbuf                 } (* comments *)
  | whitespace      { lex lexbuf                 } (* skip blanks *)
  | '('             { TOK_LPAREN                 }
  | ')'             { TOK_RPAREN                 }
  | eof             { TOK_EOF                    }
  | "#u"            { TOK_UNIT                   }
  | "#t"            { TOK_BOOL(true)             }
  | "#f"            { TOK_BOOL(false)            }
  | integer as lxm  { TOK_INT(int_of_string lxm) }
  | id_chars + as c { TOK_ID(c)                  }

  (* lexer error -- this should never happen *)
  | _              { 
      raise (Failure ("unrecognized token: " ^ (Lexing.lexeme lexbuf))) 
    }

{
(* Nothing. *)
}
