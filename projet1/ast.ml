type lambda =
  | Var of string
  | Abs of string * lambda
  | App of lambda * lambda

  (* Extensions Partie 3 *)
  | Int of int
  | Add of lambda * lambda
  | Sub of lambda * lambda
  | Mult of lambda * lambda
  | List of lambda liste
  | Head of lambda
  | Tail of lambda
  | IfZero of lambda * lambda * lambda
  | IfEmpty of lambda * lambda * lambda
  | Fix of lambda
  | Let of string * lambda * lambda

  (* Partie impérative *)
  | Unit
  | Reference of lambda
  | Address of int
  | Dereference of lambda
  | AssignTo of lambda * lambda

and 'a liste =
  | Empty
  | Cons of 'a * 'a liste
