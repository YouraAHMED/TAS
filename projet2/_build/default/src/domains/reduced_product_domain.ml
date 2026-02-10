
open Value_domain
open Interval_domain
open Parity_domain

module type REDUCED_PRODUCT = sig
  include VALUE_DOMAIN
  val reduce : t -> t
end

module ReducedProduct (D1 : VALUE_DOMAIN) (D2 : VALUE_DOMAIN) 
  : REDUCED_PRODUCT with type t = D1.t * D2.t = struct
  
  type t = D1.t * D2.t

  let reduce ((x1, x2) as x) =
    if D1.is_bottom x1 || D2.is_bottom x2 then
      (D1.bottom, D2.bottom)
    else 
      x

  let top = (D1.top, D2.top)
  let bottom = (D1.bottom, D2.bottom)

  let const c = 
    let x1 = D1.const c in
    let x2 = D2.const c in
    reduce (x1, x2)

  let rand a b =
    let x1 = D1.rand a b in
    let x2 = D2.rand a b in
    reduce (x1, x2)

  let join (x1, y1) (x2, y2) = 
    reduce (D1.join x1 x2, D2.join y1 y2)

  let meet (x1, y1) (x2, y2) =
    reduce (D1.meet x1 x2, D2.meet y1 y2)

  let widen (x1, y1) (x2, y2) =
    reduce (D1.widen x1 x2, D2.widen y1 y2)

  let subset (x1, y1) (x2, y2) =
    D1.subset x1 x2 && D2.subset y1 y2

  let is_bottom (x, y) =
    D1.is_bottom x || D2.is_bottom y

  let print fmt (x, y) =
    Format.fprintf fmt "%a ∧ %a" D2.print y D1.print x

  let unary (x, y) op =
    reduce (D1.unary x op, D2.unary y op)

  let binary (x1, y1) (x2, y2) op =
    reduce (D1.binary x1 x2 op, D2.binary y1 y2 op)

  let compare (x1, y1) (x2, y2) op =
    let (rx1, rx2) = D1.compare x1 x2 op in
    let (ry1, ry2) = D2.compare y1 y2 op in
    reduce (rx1, ry1), reduce (rx2, ry2)

  let bwd_unary (x, y) op (rx, ry) =
    let x' = D1.bwd_unary x op rx in
    let y' = D2.bwd_unary y op ry in
    reduce (x', y')

  let bwd_binary (x1, y1) (x2, y2) op (rx, ry) =
    let (x1', x2') = D1.bwd_binary x1 x2 op rx in
    let (y1', y2') = D2.bwd_binary y1 y2 op ry in
    reduce (x1', y1'), reduce (x2', y2')
end

module ParityInterval = struct
  module Base = ReducedProduct(IntervalDomain)(ParityDomain)

  include Base

  let next_even x =
    let r = Z.rem x (Z.of_int 2) in
    if Z.equal r Z.zero then x
    else Z.add x (Z.sub (Z.of_int 2) r)

  let prev_even x =
    let r = Z.rem x (Z.of_int 2) in
    if Z.equal r Z.zero then x
    else Z.sub x r

  let next_odd x =
    let r = Z.rem x (Z.of_int 2) in
    if Z.equal r Z.zero then Z.add x Z.one else x

  let prev_odd x =
    let r = Z.rem x (Z.of_int 2) in
    if Z.equal r Z.zero then Z.sub x Z.one else x

  (* compare bounds locally (we cannot reuse IntervalDomain.compare_bound here) *)
  let compare_bound_local (a:bound) (b:bound) : int =
    match a, b with
    | NegInf, NegInf | PosInf, PosInf -> 0
    | NegInf, _ | _, PosInf -> -1
    | PosInf, _ | _, NegInf -> 1
    | Int x, Int y -> Z.compare x y

  let adjust_bounds_for_parity i p =
    match i with
    | BOT -> (BOT, ParityDomain.bottom)

    | Iv (a, b) ->
        (match p with
         | Even ->
             let new_a =
               match a with
               | Int x -> Int (next_even x)
               | _ -> a
             in
             let new_b =
               match b with
               | Int x -> Int (prev_even x)
               | _ -> b
             in
             if compare_bound_local new_a new_b > 0 then (BOT, ParityDomain.bottom)
             else (Iv (new_a, new_b), p)

         | Odd ->
             let new_a =
               match a with
               | Int x -> Int (next_odd x)
               | _ -> a
             in
             let new_b =
               match b with
               | Int x -> Int (prev_odd x)
               | _ -> b
             in
             if compare_bound_local new_a new_b > 0 then (BOT, ParityDomain.bottom)
             else (Iv (new_a, new_b), p)

         | _ ->
             (i, p))

  (* réduction bidirectionnelle :
     - intervalle -> parité (singleton)
     - parité -> intervalle (bornes paires/impaires)
  *)
  let reduce (i, p) =
    match i, p with
    | BOT, _ | _, Bot ->
        (BOT, ParityDomain.bottom)

    (* Si l’intervalle est un singleton, on force la parité. *)
    | Iv (Int a, Int b), _ when Z.equal a b ->
        let p' = ParityDomain.meet p (ParityDomain.const a) in
        if ParityDomain.is_bottom p' then (BOT, ParityDomain.bottom)
        else (i, p')

    | _ ->
        let (i', p') = adjust_bounds_for_parity i p in
        (* et si après ajustement on tombe sur un singleton, on force aussi la parité *)
        (match i' with
         | Iv (Int a, Int b) when Z.equal a b ->
             let p'' = ParityDomain.meet p' (ParityDomain.const a) in
             if ParityDomain.is_bottom p'' then (BOT, ParityDomain.bottom)
             else (i', p'')
         | _ ->
             (i', p'))


  let const c = reduce (Base.const c)
  let rand a b = reduce (Base.rand a b)

  let join x y = reduce (Base.join x y)
  let meet x y = reduce (Base.meet x y)
  let widen x y = reduce (Base.widen x y)

  let unary x op = reduce (Base.unary x op)
  let binary x y op = reduce (Base.binary x y op)

  let compare x y op =
    let (x', y') = Base.compare x y op in
    (reduce x', reduce y')

  let bwd_unary x op r =
    reduce (Base.bwd_unary x op r)

  let bwd_binary x y op r =
    let (x', y') = Base.bwd_binary x y op r in
    (reduce x', reduce y')
end



