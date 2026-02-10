

(* Syntaxe des types *)
type ptype =
  | TVar of string                 (* Variable de type : X, Y, Z *)
  | Arr of ptype * ptype           (* T1 -> T2 *)
  | Nat                            (* N, type des entiers *)
  | TList of ptype                 (* [T] listes natives *)
  | Ref of ptype                   (* Référence : Ref T *)
  | UnitT                          (* type du () *)
  | Forall of string * ptype       (* ∀X. T (let-polymorphisme) *)



let rec string_of_ptype = function
  | TVar x -> x
  | Nat -> "N"
  | UnitT -> "Unit"
  | TList t -> "[" ^ string_of_ptype t ^ "]"
  | Ref t -> "Ref(" ^ string_of_ptype t ^ ")"
  | Forall (x, t) -> "∀" ^ x ^ "." ^ string_of_ptype t
  | Arr (t1, t2) ->
      "(" ^ string_of_ptype t1 ^ " -> " ^ string_of_ptype t2 ^ ")"
