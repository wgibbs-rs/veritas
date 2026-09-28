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

%left PLUS MINUS
%left STAR FSLASH

%start <Ast.program> program

%%

program:
  | statements EOF { $1 }

statement:
  | IDENT ASSIGN expr { Ast.Assignment ($1, $3) }
  | FUNCTION IDENT LPAREN RPAREN NEWLINE statements END { Ast.Function ($2, [], $6) }
  | FUNCTION IDENT LPAREN args RPAREN NEWLINE statements END { Ast.Function ($2, $4, $7) }
  | RETURN expr { Ast.Return $2 }
  | ws { Ast.WS }

statements:
  | /* empty */ { [] }
  | statement ws statements { $1 :: $3 }
  | statement { [$1] }

expr:
  | INT { Ast.Int $1 }
  | IDENT { Ast.Ident $1 }
  | STRING { Ast.String $1 }
  | expr PLUS expr { Ast.Add ($1, $3) }
  | expr MINUS expr { Ast.Subtract ($1, $3) }
  | expr STAR expr { Ast.Multiply ($1, $3) }
  | expr FSLASH expr { Ast.Divide ($1, $3) }
  | LPAREN expr RPAREN { $2 }

args:
  | IDENT { [Ast.Typeless $1] }
  | IDENT COMMA args { Ast.Typeless $1 :: $3 }

ws:
  | NEWLINE { () }
  | NEWLINE ws { () }
