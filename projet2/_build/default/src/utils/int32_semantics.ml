(*
  Int32 signed machine semantics (2's complement wrap-around),
  activated only when [enabled] is true.
*)

let enabled = ref false

(* 32-bit signed bounds: [-2^31 ; 2^31 - 1] *)
let two_pow_31 = Z.shift_left Z.one 31
let two_pow_32 = Z.shift_left Z.one 32

let min_i32 = Z.neg two_pow_31
let max_i32 = Z.sub two_pow_31 Z.one

let is_in_i32_range (x: Z.t) : bool =
  Z.compare x min_i32 >= 0 && Z.compare x max_i32 <= 0

(* Normalize any integer into signed 32-bit range. *)
let normalize (x: Z.t) : Z.t =
  if not !enabled then x
  else
    (* r = x mod 2^32 in [0, 2^32-1] *)
    let r = Z.erem (Z.add x two_pow_32) two_pow_32 in
    (* interpret as signed *)
    if Z.compare r two_pow_31 >= 0 then Z.sub r two_pow_32 else r

(* Arithmetic with optional int32 normalization *)
let neg x = normalize (Z.neg x)
let add x y = normalize (Z.add x y)
let sub x y = normalize (Z.sub x y)
let mul x y = normalize (Z.mul x y)

(* C-like division: we assume Z.div matches the project’s intended semantics
   (trunc toward zero). We then normalize result. *)
let div x y =
  if Z.equal y Z.zero then None
  else Some (normalize (Z.div x y))

(* Helpers for domains: safest full range for int32 values *)
let top_interval_if_int32 () =
  if !enabled then (min_i32, max_i32) else (min_i32, max_i32) (* unused when disabled *)

