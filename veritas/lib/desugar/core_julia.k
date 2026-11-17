// Defines the core Julia language that Veritas is capable of verifying.
// All other Julia features are desugared into this core language.
module CORE-JULIA

      // rule _ ^ _ => (_ || _) && !(_ && _)
      // rule and = (!a -> b) -> (a <-> b)
      // rule or = !((a <-> b) && (a -> false))
      // rule xor = !(a <-> b)

      syntax Ident ::= r"[a-zA-Z][a-zA-Z0-9_]*" [token]

      syntax Stmt ::= Ident "=" Expr
                  | Stmt ";" Stmt
                  | "if" Expr Stmt "end"
                  > "if" Expr Stmt "else" Stmt "end"

      syntax Expr ::= Ident
                  | Bool
                  | "call" "(" Ident ")"


      rule (true) => true
      rule (false) => false

      rule !true => false
      rule !false => true

      rule true && true  => true
      rule _ && _ => false [avoid]

      rule false || false => false
      rule _ || _ => true [avoid]

      rule true ^ true  => false
      rule false ^ false => false
      rule _ ^ _ => true [avoid]

endmodule
