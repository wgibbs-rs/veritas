
From Stdlib Require Import Bool.
From Stdlib Require Import Init.Nat.
From Stdlib Require Import Arith.
From Stdlib Require Import EqNat. Import Nat.
From Stdlib Require Import Lia.
From Stdlib Require Import List. Import ListNotations.
From Stdlib Require Import Strings.String.
From Model Require Export Maps.

Module CoreJulia.

(* 
expr represents any Julia expression.
We simplify Julia's expressions into out "Core Julia", which contais only
uninterpreted expressions and operations.
We represent any term of any Julia code as a single Expr.
This could be a function, a number, a variable, etc.
 *)

Inductive expr : Type :=
    | E.    (* An uninterpreted expression. *)

Inductive bexpr : Type :=
    | JL_True
    | JL_False
    | JL_Eq (e1 e2 : expr)
    | JL_Neq (e1 e2 : expr)
    | JL_Le (e1 e2 : expr)
    | JL_Gt (e1 e2 : expr)
    | JL_Not (b : bexpr)
    | JL_And (b1 b2 : bexpr).
    
(* Define a program state as a map. *)
Definition state := total_map nat.
(* Define an empty state with no key. *)
Definition empty_st := (__ !-> 0).
(* Define notation for a singleton state. *)
Notation "x '!->' v" := (x !-> v ; empty_st) (at level 100, right associativity).

Declare Custom Entry com.
Declare Scope com_scope.

(* Define notation for CoreJulia script. *)
Notation "<{ e }>" := e
    (e custom com, format "'[hv' <{ '/ ' '[v' e ']' '/' }> ']'") : com_scope.

Notation "e1 == e2" := (JL_Eq e1 e2) (in custom com at level 70, no associativity).
Notation "e1 != e2" := (JL_Neq e1 e2) (in custom com at level 70, no associativity).
Notation "e1 <= e2" := (JL_Le e1 e2) (in custom com at level 70, no associativity).
Notation "e1 ≤ e2" := (JL_Le e1 e2) (in custom com at level 70, no associativity).
Notation "e1 > e2" := (JL_Eq e1 e2) (in custom com at level 70, no associativity).
Notation "! b" := (JL_Not b) (in custom com at level 70, no associativity).
Notation "b1 && b2" := (JL_And b1 b2) (in custom com at level 70, no associativity).


(* Paranthesis notation support. *)
Notation "( x )" := x (in custom com, x at level 99).

Notation "x" := x (in custom com at level 0, x constr at level 0).

Inductive com : Type :=
    | JL_Empty                                  (* Any empty block. *)
    | JL_Asgn (e : expr) (a : expr)             (* Assign a value to a DatatypeExpr. *)
    | JL_Seq (c1 c2 : com)                      (* Write two or more in sequence. *)
    | JL_If (b : bexpr) (c1 c2 : com)            (* An if statement; may have an else.*)
    | JL_While (b : bexpr) (c : com)             (* The most basic while loop. *)
    | JL_Function0 (c : com)                    (* A function with no arguments. *)
    | JL_Function1 (e : expr) (c : com)         (* A function with no arguments. *)
    | JL_Function2 (e1 e2 : expr) (c : com).    (* A function with no arguments. *)

Notation "x ':=' y" := (JL_Asgn x y)
    (in custom com at level 0, x constr at level 0, y at level 85, no associativity,
        format "x ':=' y") : com_scope.

Notation "e1 ; e2" := (JL_Seq e1 e2)
    (in custom com at level 88,
        right associativity,
        format "'[v' e1 ';' e2 ']'") : com_scope.
    
Notation "'if' b e 'end'" := (JL_If b e JL_Empty)
    (in custom com at level 89, b at level 99, e at level 99,
        format "'[v' 'if' b '/ ' e '/' 'end' ']'") : com_scope.

Notation "'if' b e1 'else' e2 'end'" := (JL_If b e1 e2)
    (in custom com at level 89, b at level 99, e1 at level 99, e2 at level 99,
        format "'[v' 'if' b '/ ' e1 '/' 'else' '/ ' e2 '/' 'end' ']'") : com_scope.

Notation "'while' b e 'end'" := (JL_While b e)
    (in custom com at level 89, b at level 99, e at level 99,
        format "'[v' 'while' b '/ ' e '/' 'end' ']'") : com_scope.

Notation "'function' '(' a1 ',' a2 ')' e 'end'" := (JL_Function2 a1 a2 e)
    (in custom com at level 89, a1 at level 99, a2 at level 99, e at level 99,
        format "'[v' 'function' '(' a1 ',' a2 ')' '/' e '/' 'end' ']'") : com_scope.

Notation "'function' '(' a ')' e 'end'" := (JL_Function1 a e)
    (in custom com at level 89, a at level 99, e at level 99,
        format "'[v' 'function' '(' a ')' '/' e '/' 'end' ']'") : com_scope.

Notation "'function' '(' ')' e 'end'" := (JL_Function0 e)
    (in custom com at level 89, e at level 99,
        format "'[v' 'function' '(' ')' '/' e '/' 'end' ']'") : com_scope.

Open Scope com_scope.

Definition fact_in_rocq : com :=
    <{ 
        E := E ;
        E := E ;
        while (E == E)
            E := E ;
            E := E
        end ;
        function ( E, E )
            E := E
        end
    }>.

Unset Printing Notations.
Print fact_in_rocq.

End CoreJulia.