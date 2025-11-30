(* ========================================================================= *)
(*                               UTILS                                       *)
(* ========================================================================= *)

open Ast
open Pretty

(* ------------------------------------------------------------------------- *)
(*                             MÉMOIRE                                       *)
(* ------------------------------------------------------------------------- *)

type memory = (int * lambda) list

(* Alloue une nouvelle adresse et stocke la valeur *)
let memory_extend (v : lambda) (mem : memory) : int * memory =
  let new_addr =
    match mem with
    | [] -> 0
    | (a, _) :: _ -> a + 1
  in
  (new_addr, (new_addr, v) :: mem)

(* Recherche d’une adresse *)
let mem_lookup (addr : int) (mem : memory) : lambda option =
  try Some (List.assoc addr mem)
  with Not_found -> None

(* Mise à jour de la mémoire *)
let mem_update (addr : int) (v : lambda) (mem : memory) : memory =
  let rec update = function
    | [] -> [(addr, v)]
    | (a, old) :: tl ->
        if a = addr then (a, v) :: tl
        else (a, old) :: update tl
  in
  update mem

(* ------------------------------------------------------------------------- *)
(*                          AFFICHAGE DE LA MÉMOIRE                          *)
(* ------------------------------------------------------------------------- *)

let show_memory (mem : memory) : string =
  let binding_to_string (addr, v) =
    "(" ^ string_of_int addr ^ " , " ^ string_of_lambda v ^ ")"
  in
  let rec aux = function
    | [] -> ""
    | [b] -> binding_to_string b
    | b :: tl -> binding_to_string b ^ " ; " ^ aux tl
  in
  "[" ^ aux mem ^ "]"

(* ------------------------------------------------------------------------- *)
(*                             LISTES NATIVES                                *)
(* ------------------------------------------------------------------------- *)

let is_list t =
  match t with
  | List l -> l
  | _ -> failwith "L'expression n'est pas une liste"

let get_head_list = function
  | Empty -> failwith "Head d'une liste vide"
  | Cons (h, _) -> h

let get_tail_list = function
  | Empty -> failwith "Tail d'une liste vide"
  | Cons (_, t) -> List t

(* ------------------------------------------------------------------------- *)
(*                               VALEURS                                     *)
(* ------------------------------------------------------------------------- *)

let rec verif_terme = function
  | Abs _ -> true
  | Int _ -> true
  | Unit -> true
  | Address _ -> true
  | List l -> verif_list l
  | _ -> false

and verif_list = function
  | Empty -> true
  | Cons (h, t) -> verif_terme h && verif_list t
