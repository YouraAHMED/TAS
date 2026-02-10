(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)

(*
   The constant domain.
 *)

open Abstract_syntax_tree
open Value_domain

module Constants = (struct

  (* type of abstract values *)
  type t =
    | Cst of Z.t  (* the set is a single value (constant) *)
    | BOT         (* the set is empty (not reachable) *)
    | TOP         (* the set of all integers (not constant)  *)

  (* lift unary arithmetic operations, from integers to t *)
  let lift1 f x =
    match x with
    | BOT -> BOT
    | TOP -> TOP
    | Cst a -> Cst (f a)

  (* lift binary arithmetic operations *)
  let lift2 f x y =
    match x, y with
    | BOT, _ | _, BOT -> BOT
    | TOP, _ | _, TOP -> TOP
    | Cst a, Cst b -> Cst (f a b)

  (* unrestricted value *)
  let top = TOP

  (* bottom value *)
  let bottom = BOT

  (* constant *)
  let const c = Cst (Int32_semantics.normalize c)

  (* interval *)
  let rand x y =
    let x = Int32_semantics.normalize x in
    let y = Int32_semantics.normalize y in
    if Z.equal x y then Cst x
    else if Z.lt x y then TOP
    else BOT

  (* arithmetic operations *)
  let neg = lift1 Int32_semantics.neg
  let add = lift2 Int32_semantics.add
  let sub = lift2 Int32_semantics.sub

  (* multiplication with a useful precision trick: 0 * anything = 0 *)
  let mul x y =
    match x, y with
    | BOT, _ | _, BOT -> BOT
    | Cst z, _ when Z.equal z Z.zero -> Cst Z.zero
    | _, Cst z when Z.equal z Z.zero -> Cst Z.zero
    | Cst a, Cst b -> Cst (Int32_semantics.mul a b)
    | _ -> TOP

  let div a b =
    match a, b with
    | BOT, _ | _, BOT -> BOT
    | _, Cst z when Z.equal z Z.zero -> BOT
    | Cst x, Cst y ->
        (match Int32_semantics.div x y with
         | None -> BOT
         | Some r -> Cst r)
    | _ -> TOP

  (* set-theoretic operations *)
  let join a b =
    match a, b with
    | BOT, x | x, BOT -> x
    | Cst x, Cst y when Z.equal x y -> a
    | _ -> TOP

  let meet a b =
    match a, b with
    | TOP, x | x, TOP -> x
    | Cst x, Cst y when Z.equal x y -> a
    | _ -> BOT

  (* finite height => widening = join *)
  let widen = join

  (* comparison operations (filters) *)

  (* a == b *)
  let eq a b =
    match a, b with
    | BOT, _ | _, BOT -> BOT, BOT
    | Cst x, Cst y ->
        if Z.equal x y then (a, b) else (BOT, BOT)
    | Cst x, TOP -> (Cst x, Cst x)
    | TOP, Cst y -> (Cst y, Cst y)
    | TOP, TOP -> (TOP, TOP)

  (* a != b *)
  let neq a b =
    match a, b with
    | BOT, _ | _, BOT -> BOT, BOT
    | Cst x, Cst y ->
        if Z.equal x y then (BOT, BOT) else (a, b)
    | _ ->
        (* cannot represent "all values except c" in constant domain *)
        (a, b)

  (* a >= b *)
  let geq a b =
    match a, b with
    | BOT, _ | _, BOT -> BOT, BOT
    | Cst x, Cst y ->
        if Z.geq x y then (a, b) else (BOT, BOT)
    | _ -> (a, b)

  (* a > b *)
  let gt a b =
    match a, b with
    | BOT, _ | _, BOT -> BOT, BOT
    | Cst x, Cst y ->
        if Z.gt x y then (a, b) else (BOT, BOT)
    | _ -> (a, b)

  (* subset inclusion of concretizations *)
  let subset a b =
    match a, b with
    | BOT, _ | _, TOP -> true
    | Cst x, Cst y -> Z.equal x y
    | _ -> false

  (* check the emptiness of the concretization *)
  let is_bottom a =
    a = BOT

  (* print abstract element *)
  let print fmt x =
    match x with
    | BOT -> Format.fprintf fmt "⊥"
    | TOP -> Format.fprintf fmt "⊤"
    | Cst z -> Format.fprintf fmt "{%s}" (Z.to_string z)

  (* operator dispatch *)
  let unary x op =
    match op with
    | AST_UNARY_PLUS  -> x
    | AST_UNARY_MINUS -> neg x

  let binary x y op =
    match op with
    | AST_PLUS     -> add x y
    | AST_MINUS    -> sub x y
    | AST_MULTIPLY -> mul x y
    | AST_DIVIDE   -> div x y

  let compare x y op =
    match op with
    | AST_EQUAL         -> eq x y
    | AST_NOT_EQUAL     -> neq x y
    | AST_GREATER_EQUAL -> geq x y
    | AST_GREATER       -> gt x y
    | AST_LESS_EQUAL    -> let y', x' = geq y x in (x', y')
    | AST_LESS          -> let y', x' = gt  y x in (x', y')

  let bwd_unary x op r =
    match op with
    | AST_UNARY_PLUS  -> meet x r
    | AST_UNARY_MINUS -> meet x (neg r)

  let bwd_binary x y op r =
    match op with
    | AST_PLUS ->
        (* r=x+y => x=r-y and y=r-x *)
        meet x (sub r y), meet y (sub r x)

    | AST_MINUS ->
        (* r=x-y => x=y+r and y=x-r *)
        meet x (add y r), meet y (sub x r)

    | AST_MULTIPLY ->
        (* r=x*y => (x=r/y or y=r=0) and (y=r/x or x=r=0)  *)
        let contains_zero o = subset (const Z.zero) o in
        (if contains_zero y && contains_zero r then x else meet x (div r y)),
        (if contains_zero x && contains_zero r then y else meet y (div r x))

    | AST_DIVIDE ->
        (* sound, but not precise *)
        x, y

end : VALUE_DOMAIN)
