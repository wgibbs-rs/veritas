(*
ZLib License

Copyright (c) 2025 William Gibbs

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.
*)

let parse_file filename =
  let input = In_channel.with_open_text filename In_channel.input_all in
  let lexbuf = Lexing.from_string input in
  try
    Parser.program Lexer.token lexbuf
  with
  | Parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf
        "Parse error at line %d, column %d\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      exit 1

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in

  match (Lexer.validate_syntax filename) with
  | Failure o -> print_string o
  | Success -> print_string "";

  let ast = parse_file filename in

  Ast.print_ast ast