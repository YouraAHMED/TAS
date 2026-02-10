open Abstract_syntax_tree
open Value_domain

type parity =
  | Even
  | Odd
  | Top
  | Bot

module ParityDomain : sig
  include VALUE_DOMAIN with type t = parity
  val is_even : t -> bool
  val is_odd : t -> bool
end = struct
  type t = parity

  let top = Top
  let bottom = Bot

  let is_even = function Even -> true | _ -> false
  let is_odd  = function Odd  -> true | _ -> false

  let const x =
    let x = Int32_semantics.normalize x in
    if Z.equal (Z.rem x (Z.of_int 2)) Z.zero then Even else Odd

  let rand a b =
    let a = Int32_semantics.normalize a in
    let b = Int32_semantics.normalize b in
    if Z.equal a b then const a
    else if Z.gt a b then Bot
    else Top

  let join x y =
    match x, y with
    | Bot, v | v, Bot -> v
    | Even, Even -> Even
    | Odd,  Odd  -> Odd
    | _ -> Top

  let meet x y =
    match x, y with
    | Bot, _ | _, Bot -> Bot
    | Top, v | v, Top -> v
    | Even, Even -> Even
    | Odd,  Odd  -> Odd
    | _ -> Bot

  let widen = join

  let subset x y =
    match x, y with
    | Bot, _ -> true
    | _, Top -> true
    | Even, Even -> true
    | Odd,  Odd  -> true
    | _ -> false

  let is_bottom x = (x = Bot)

  let print fmt = function
    | Even -> Format.fprintf fmt "even"
    | Odd  -> Format.fprintf fmt "odd"
    | Top  -> Format.fprintf fmt "⊤"
    | Bot  -> Format.fprintf fmt "⊥"

  let unary x = function
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> x  (* -even = even, -odd = odd *)

  let binary x y op =
    match op with
    | AST_PLUS | AST_MINUS ->
        (match x, y with
         | Bot, _ | _, Bot -> Bot
         | Even, Even | Odd, Odd -> Even
         | Even, Odd  | Odd, Even -> Odd
         | _ -> Top)

    | AST_MULTIPLY ->
        (match x, y with
         | Bot, _ | _, Bot -> Bot
         | Even, _ | _, Even -> Even
         | Odd, Odd -> Odd
         | _ -> Top)

    | AST_DIVIDE ->
        (* division parity is tricky; keep it sound *)
        (match x, y with
         | Bot, _ | _, Bot -> Bot
         | _, Even ->
             (* could be 0/2, 1/2, 3/2, ... result can be even or odd or 0 *)
             Top
         | Even, Odd -> Even
         | Odd,  Odd -> Top  (* e.g. 3/1=3 odd, 3/3=1 odd, 3/5=0 even *)
         | _ -> Top)

  let compare x y op =
    if x = Bot || y = Bot then (Bot, Bot)
    else match op with
      | AST_EQUAL ->
          if x = y then (x, y)
          else if x = Top then (y, y)
          else if y = Top then (x, x)
          else (Bot, Bot)
      | AST_NOT_EQUAL ->
          if x = y then (Bot, Bot) else (x, y)
      | _ ->
          (x, y)

  let bwd_unary x op r =
    meet x (unary r op)

  let bwd_binary x y _op r =
    (meet x r, meet y r)
end
