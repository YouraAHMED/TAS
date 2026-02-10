open Ast

(* ---------------------------------------------------------------------- *)
(*                       Pretty Printer complet                           *)
(* ---------------------------------------------------------------------- *)

let rec string_of_lambda = function
  | Var x -> x
  | Abs (x,t) -> "λ" ^ x ^ ". " ^ string_of_lambda t
  | App (t1,t2) -> "(" ^ string_of_lambda t1 ^ " " ^ string_of_lambda t2 ^ ")"

  (* Entiers et opérateurs arithmétiques *)
  | Int n -> string_of_int n
  | Add (t1,t2) -> "Add(" ^ string_of_lambda t1 ^ "," ^ string_of_lambda t2 ^ ")"
  | Sub (t1,t2) -> "Sub(" ^ string_of_lambda t1 ^ "," ^ string_of_lambda t2 ^ ")"
  | Mult (t1,t2) -> "Mult(" ^ string_of_lambda t1 ^ "," ^ string_of_lambda t2 ^ ")"

  (* Listes *)
  | List l -> list_to_string l
  | Head t -> "Head(" ^ string_of_lambda t ^ ")"
  | Tail t -> "Tail(" ^ string_of_lambda t ^ ")"

  (* Conditionnels *)
  | IfZero (c,t1,t2) ->
      "IfZero(" ^ string_of_lambda c ^ "," ^ string_of_lambda t1 ^ "," ^ string_of_lambda t2 ^ ")"
  | IfEmpty (c,t1,t2) ->
      "IfEmpty(" ^ string_of_lambda c ^ "," ^ string_of_lambda t1 ^ "," ^ string_of_lambda t2 ^ ")"

  (* Fix et Let *)
  | Fix t -> "Fix(" ^ string_of_lambda t ^ ")"
  | Let (x,e1,e2) ->
      "Let " ^ x ^ " = " ^ string_of_lambda e1 ^ " in " ^ string_of_lambda e2

  (* Impératif *)
  | Unit -> "()"
  | Reference t -> "ref(" ^ string_of_lambda t ^ ")"
  | Address a -> "Addr(" ^ string_of_int a ^ ")"
  | Dereference t -> "!" ^ string_of_lambda t
  | AssignTo (t1,t2) -> string_of_lambda t1 ^ " := " ^ string_of_lambda t2

(* Format listes *)
and list_to_string l =
  let rec aux = function
    | Empty -> ""
    | Cons (h,Empty) -> string_of_lambda h
    | Cons (h,t) -> string_of_lambda h ^ "," ^ aux t
  in
  "[" ^ aux l ^ "]"

let print_lambda t =
  print_endline (string_of_lambda t)
