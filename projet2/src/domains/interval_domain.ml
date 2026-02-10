(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
*)

open Abstract_syntax_tree
open Value_domain  

type bound =
  | PosInf
  | NegInf
  | Int of Z.t

type intervalTyp =
  | Iv of bound * bound
  | BOT

module IntervalDomain : VALUE_DOMAIN with type t = intervalTyp = struct

  type t = intervalTyp

  (* int32 bounds *)
  let i32_min = Int32_semantics.min_i32
  let i32_max = Int32_semantics.max_i32

  let machine_top () : t =
    Iv (Int i32_min, Int i32_max)

  let is_machine_top = function
    | Iv (Int a, Int b) -> Z.equal a i32_min && Z.equal b i32_max
    | _ -> false

  (* ===== original bound utilities ===== *)

  let compare_bound (a:bound) (b:bound) : int =
    match a, b with
    | NegInf, NegInf | PosInf, PosInf -> 0
    | NegInf, _    | _, PosInf    -> -1
    | PosInf, _    | _, NegInf    ->  1
    | Int i, Int j -> Z.compare i j

  let min_bound (a:bound) (b:bound) : bound =
    if compare_bound a b <= 0 then a else b

  let max_bound (a:bound) (b:bound) : bound =
    if compare_bound a b >= 0 then a else b

  let negate_bound = function
    | PosInf  -> NegInf
    | NegInf  -> PosInf
    | Int x   -> Int (Z.neg x)

  let decrement_bound = function
    | PosInf  -> PosInf
    | NegInf  -> NegInf
    | Int a   -> Int (Z.sub a Z.one)

  let increment_bound = function
    | PosInf  -> PosInf
    | NegInf  -> NegInf
    | Int a   -> Int (Z.add a Z.one)

  let gt_value (x:bound) (y:bound) : bool =
    match x, y with
    | NegInf, _  | _, PosInf -> false
    | PosInf, _  | _, NegInf -> true
    | Int v1, Int v2 -> Z.gt v1 v2

  let max_value x y = not (gt_value y x)

  (* ===== original lift2 (kept) ===== *)

  let lift2 (f: Z.t -> Z.t -> Z.t) (x:t) (y:t) : t =
    match x, y with
    | BOT, _ | _, BOT -> BOT
    | Iv(a, b), Iv(c, d) ->
      begin match a,b,c,d with
      | Int a, PosInf, Int c,  _    
      | Int a,     _, Int c, PosInf -> Iv(Int (f a c), PosInf)
      | Int a, Int b, Int c, Int d ->
          Iv(Int (f a c), Int (f b d))
      | NegInf,  _,   _,     PosInf  
      | _,    PosInf, NegInf,   _    
      | NegInf, PosInf, _,      _    
      | _,     _,   NegInf,  PosInf  
        -> Iv(NegInf, PosInf)
      | NegInf, Int b, _, Int d
      | _, Int b, NegInf, Int d
        -> Iv(NegInf, Int (f b d))
      | _ -> BOT
      end

  (* ===== interface basics ===== *)

  let top = Iv(NegInf, PosInf)
  let bottom = BOT
  let is_bottom (v:t) = (v = BOT)

  (* constants / rand *)
  let const (c:Z.t) =
    if !Int32_semantics.enabled
    then Iv(Int (Int32_semantics.normalize c), Int (Int32_semantics.normalize c))
    else Iv(Int c, Int c)

  let rand (a:Z.t) (b:Z.t) : t =
    if !Int32_semantics.enabled then
      let a = Int32_semantics.normalize a in
      let b = Int32_semantics.normalize b in
      if Z.gt a b then BOT else Iv(Int a, Int b)
    else
      if Z.gt a b then BOT else Iv(Int a, Int b)

  (* meet/join/subset identical to original *)
  let meet (x:t) (y:t) : t =
    match x, y with
    | BOT, _ | _, BOT -> BOT
    | Iv(a1, b1), Iv(a2, b2) ->
       if gt_value a2 b1 || gt_value a1 b2 then BOT
       else Iv(max_bound a1 a2, min_bound b1 b2)

  let join (a:t) (b:t) : t =
    match a,b with
    | BOT, x | x, BOT -> x
    | Iv(i, j), Iv(k, l) -> Iv(min_bound i k, max_bound j l)

  let subset (a:t) (b:t) : bool =
    match a,b with
    | BOT, _ -> true
    | _, BOT -> false
    | Iv(a1, b1), Iv(a2, b2) ->
       compare_bound a2 a1 <= 0 && compare_bound b1 b2 <= 0

  (* ===== arithmetic: original behavior in math mode ===== *)

  (* In int32 mode: if wrap makes the result non-convex, return machine_top. *)
  let safe_i32_interval_from_math (lo:Z.t) (hi:Z.t) : t =
    let nlo = Int32_semantics.normalize lo in
    let nhi = Int32_semantics.normalize hi in
    if Z.equal nlo nhi then Iv(Int nlo, Int nlo)
    else if Z.gt nlo nhi then machine_top ()
    else Iv(Int nlo, Int nhi)

  let add (x:t) (y:t) : t =
    if not !Int32_semantics.enabled then
      lift2 Z.add x y
    else
      match x, y with
      | BOT, _ | _, BOT -> BOT
      | Iv(Int a, Int b), Iv(Int c, Int d) ->
          safe_i32_interval_from_math (Z.add a c) (Z.add b d)
      | _ ->
          machine_top ()

  let neg (x:t) : t =
    if not !Int32_semantics.enabled then
      match x with
      | BOT -> BOT
      | Iv(a,b) -> Iv(negate_bound b, negate_bound a)
    else
      match x with
      | BOT -> BOT
      | Iv(Int a, Int b) ->
          (* if interval contains INT_MIN then -x can wrap => non-convex *)
          if Z.leq a i32_min && Z.geq b i32_min then machine_top ()
          else safe_i32_interval_from_math (Z.neg b) (Z.neg a)
      | _ ->
          machine_top ()

  let sub (x:t) (y:t) : t =
    if not !Int32_semantics.enabled then
      lift2 Z.add x (neg y)
    else
      match x, y with
      | BOT, _ | _, BOT -> BOT
      | Iv(Int a, Int b), Iv(Int c, Int d) ->
          safe_i32_interval_from_math (Z.sub a d) (Z.sub b c)
      | _ ->
          machine_top ()

  let mul (x: t) (y: t) : t =
    if not !Int32_semantics.enabled then
      match x, y with
      | BOT, _ | _, BOT -> BOT
      | Iv(Int a, Int b), Iv(Int c, Int d) ->
         let candidates = [ Z.mul a c; Z.mul a d; Z.mul b c; Z.mul b d ] in
         let mini = List.fold_left Z.min (List.hd candidates) (List.tl candidates) in
         let maxi = List.fold_left Z.max (List.hd candidates) (List.tl candidates) in
         Iv(Int mini, Int maxi)
      | Iv(_, _), Iv(_, _) ->
         Iv(NegInf, PosInf)
    else
      (* conservative but sound in int32 mode *)
      match x, y with
      | BOT, _ | _, BOT -> BOT
      | Iv(Int a, Int b), Iv(Int c, Int d) when Z.equal a b && Z.equal c d ->
          let r = Int32_semantics.mul a c in
          Iv(Int r, Int r)
      | _ ->
          machine_top ()

  let div (x:t) (y:t) : t =
    if not !Int32_semantics.enabled then
      match x, y with
      | _, Iv(_, Int y2) when Z.equal y2 Z.zero -> BOT
      | BOT, _ | _, BOT -> BOT
      | Iv(Int a, Int b), Iv(Int c, Int d) ->
         let candidates = [ Z.div a c; Z.div a d; Z.div b c; Z.div b d ] in
         let mini = List.fold_left Z.min (List.hd candidates) (List.tl candidates) in
         let maxi = List.fold_left Z.max (List.hd candidates) (List.tl candidates) in
         Iv(Int mini, Int maxi)
      | _ ->
         BOT
    else
      match x, y with
      | BOT, _ | _, BOT -> BOT
      | Iv(Int a, Int b), Iv(Int c, Int d)
        when Z.equal a b && Z.equal c d && not (Z.equal c Z.zero) ->
          (match Int32_semantics.div a c with
           | None -> BOT
           | Some r -> Iv(Int r, Int r))
      | _ ->
          machine_top ()

  let unary (x:t) (op:int_unary_op) : t =
    match op with
    | AST_UNARY_PLUS  -> x
    | AST_UNARY_MINUS -> neg x

  let binary (x:t) (y:t) (op:int_binary_op) : t =
    match op with
    | AST_PLUS      -> add x y
    | AST_MINUS     -> sub x y
    | AST_MULTIPLY  -> mul x y
    | AST_DIVIDE    -> div x y

  (* widening: original in math mode, machine-bounds in int32 mode *)
  let widen (x:t) (y:t) : t =
    match x, y with
    | BOT, _ -> y
    | _, BOT -> x
    | Iv(a, b), Iv(c, d) ->
       if not !Int32_semantics.enabled then
         let new_left  = if max_value c a then a else NegInf in
         let new_right = if max_value b d then b else PosInf in
         Iv(new_left, new_right)
       else
         let new_left  = if max_value c a then a else Int i32_min in
         let new_right = if max_value b d then b else Int i32_max in
         Iv(new_left, new_right)

  (* ===== comparisons: keep original math filtering; conservative at machine_top in int32 ===== *)

  let not_equal a b = match a, b with
    | BOT, _ | _, BOT -> BOT, BOT
    | Iv(x, y), Iv(v, w) when compare_bound x v = 0 && compare_bound y w = 0 -> a, b
    | Iv(x, y), Iv(v, _) when compare_bound x v = 0 -> Iv(increment_bound x, y), b
    | Iv(x, y), Iv(_, w) when compare_bound y w = 0 -> Iv(x, decrement_bound y), b
    | _, _ -> let m = meet a b in m, m

  let compare x y op =
    if !Int32_semantics.enabled && (is_machine_top x || is_machine_top y) then
      (x, y)
    else
      match op with
      | AST_EQUAL ->
          let m = meet x y in
          m, m
      | AST_NOT_EQUAL -> not_equal x y

      | AST_GREATER_EQUAL -> 
        (match x, y with
        | BOT, _ | _, BOT -> BOT, BOT
        | Iv(a1,b1), Iv(a2,b2) ->
          if gt_value a2 b1 then BOT, BOT
          else
            let nx = Iv(max_bound a1 a2, b1) in 
            let ny = Iv(a2, min_bound b1 b2) in
            nx, ny)

      | AST_GREATER ->
        (match x, y with 
        | BOT, _ | _, BOT -> BOT, BOT
        | Iv(a1,b1), Iv(a2,b2) ->
          if compare_bound b1 a2 <= 0 then BOT, BOT
          else
            let nx = Iv(max_bound a1 (increment_bound a2), b1) in
            let ny = Iv(a2, min_bound (decrement_bound b1) b2) in
            nx, ny)

      | AST_LESS -> 
        (match x, y with
        | BOT, _ | _, BOT -> BOT, BOT 
        | Iv(a1,b1), Iv(a2,b2) ->
          if compare_bound b1 a2 < 0 then (x, y)  
          else
            let nx = Iv(a1, min_bound b1 (decrement_bound b2)) in
            let ny = match b2 with
              | PosInf -> y  
              | _ -> Iv(max_bound a2 (increment_bound a1), b2)
            in
            nx, ny)

      | AST_LESS_EQUAL -> 
        (match x, y with
        | BOT, _ | _, BOT -> BOT, BOT
        | Iv(a1,b1), Iv(a2,b2) ->
          if compare_bound b1 a2 < 0 then (x, y)
          else
            let nx = Iv(a1, min_bound b1 b2) in
            let ny = match b2 with
              | PosInf -> y
              | _ -> Iv(max_bound a2 a1, b2)
            in
            nx, ny)

  (* backward operations: original ones *)
  let bwd_unary (x:t) (op:int_unary_op) (r:t) : t =
    match op with
    | AST_UNARY_PLUS  -> meet x r
    | AST_UNARY_MINUS -> meet x (neg r)

  let bwd_binary (x:t) (y:t) (op:int_binary_op) (r:t) : (t * t) =
    match op with
    | AST_PLUS ->
      let x' = meet x (sub r y) in
      let y' = meet y (sub r x) in
      (x', y')

    | AST_MINUS ->
      let x' = meet x (add y r) in
      let y' = meet y (sub x r) in
      (x', y')

    | AST_MULTIPLY ->
      let containsZero o =
        subset (Iv(Int Z.zero, Int Z.zero)) o
      in
      let x' =
        if containsZero y && containsZero r then x
        else meet x (div r y)
      in
      let y' =
        if containsZero x && containsZero r then y
        else meet y (div r x)
      in
      (x', y')

    | AST_DIVIDE ->
      (x, y)

  (* printing: keep your original printer (ok) *)
  let print fmt x =
    let string_of_bound = function
      | NegInf   -> "-∞"
      | PosInf   -> "+∞"
      | Int z    -> Z.to_string z
    in
    match x with
    | BOT -> Format.fprintf fmt "⊥"
    | Iv (NegInf, PosInf) -> Format.fprintf fmt "[-∞;+∞]"
    | Iv (a, b) -> Format.fprintf fmt "[%s;%s]" (string_of_bound a) (string_of_bound b)

end
