
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
We represent any term of any Julia code as a single DatatypeObject.
This could be a function, a number, a variable, etc.
 *)

Inductive expr : Type :=
    | DatatypeExpr. (* An uninterpreted expression. *)

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

(* Paranthesis notation support. *)
Notation "( x )" := x (in custom com, x at level 99).

Notation "x" := x (in custom com at level 0, x constr at level 0).

Inductive com : Type :=
    | JL_Empty                          (* Any empty block. *)
    | JL_Asgn (x : string) (a : expr)   (* Assign a value to a DatatypeExpr. *)
    | JL_Seq (c1 c2 : com)              (* Write two or more in sequence. *)
    | JL_If (b : expr) (c1 c2 : com)    (* An if statement; may have an else.*)
    | JL_While (b : expr) (c : com).    (* The most basic while loop. *)

Notation "x '=' y" := (JL_Asgn x y)
    (in custom com at level 0, x constr at level 0, y at level 85, no associativity,
        format "x '=' y") : com_scope.

(* For Rocq's parser, we'll need to use a line separator. *)
Notation "x ; y" := (JL_Seq x y)
  (in custom com at level 90,
    right associativity,
    format "'[v' x ; '/' y ']'") : com_scope.

Notation "'if' x y 'else' z 'end'" :=
    (JL_If x y z)
    (in custom com at level 89,
     x constr at level 0,
     y at level 99,
     z at level 99) : com_scope.

Notation "'while' x y 'end'" :=
    (JL_While x y)
    (in custom com at level 89,
     x constr at level 0,
     y at level 90,
     format "'[v' 'while' x '/' y '/' 'end' ']'")
    : com_scope.

Definition ex_core_julia : com :=
    <{ 
        Z = X ;
        Y = Z ;
        while Z
            Y = Y ;
            Z = Z
        end
    }>.

End CoreJulia.