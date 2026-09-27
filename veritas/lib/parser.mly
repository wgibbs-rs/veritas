%{
  (* OCaml code emitted into the generated parser. *)
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT

(* Keywords *)
%token FUNCTION
%token IF
%token ELSE
%token END
%token WHILE
%token FOR
%token RETURN

(* Arithmetic *)
%token PLUS
%token MINUS
%token STAR
%token FSLASH
%token PERCENT

(* Comparisons *)
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE

(* Boolean / logical operators *)
%token AND
%token OR
%token NOT

(* Assignment *)
%token ASSIGN

(* Punctuation *)
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token COMMA
%token SEMICOLON
%token COLON
%token DOT

%token NEWLINE

%token EOF

%start <Ast.program> program

%%

program:
  | statements EOF { $1 }

statements:
  | { [] }
  | statement NEWLINE { [$1] }
  | statement { [$1] }
  | statement NEWLINE statements { $1 :: $3 }

statement:
  | IDENT ASSIGN expr { Ast.Assignment ($1, $3) }
  | NEWLINE { Ast.NewLine }

expr:
  | INT { Ast.Int $1 }
  | IDENT { Ast.Ident $1 }

// args:
//   | IDENT { [Ast.Ident $1] }
//   | IDENT COMMA args { Ast.Ident $1 :: $3 }
