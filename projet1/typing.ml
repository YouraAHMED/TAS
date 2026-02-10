(* ========================================================================= *)
(*                              TYPAGE TAS                                    *)
(* ========================================================================= *)

open Types
open Ast

(* ======================================================================== *)
(*                  OUTILS : environnement, équations                       *)
(* ======================================================================== *)

type equation = ptype * ptype
type env = (string * ptype) list

(* Génération de variables de type fraîches *)
let compteur_type = ref 0
let fresh_tvar () =
  incr compteur_type;
  TVar ("T" ^ string_of_int !compteur_type)

(* ======================================================================== *)
(*                      OCCUR CHECK (empêche X = Arr X)                     *)
(* ======================================================================== *)

let rec occur x ty =
  match ty with
  | TVar y -> x = y
  | Arr (t1, t2) -> occur x t1 || occur x t2
  | TList t -> occur x t
  | Ref t -> occur x t
  | Forall (_, t) -> occur x t
  | _ -> false

(* ======================================================================== *)
(*     SUBSTITUTION D'UN TYPE PAR UN AUTRE DANS TOUTES LES ÉQUATIONS        *)
(* ======================================================================== *)

let rec subst_type x tnew ty =
  match ty with
  | TVar y -> if y = x then tnew else ty
  | Arr (t1, t2) -> Arr (subst_type x tnew t1, subst_type x tnew t2)
  | TList t -> TList (subst_type x tnew t)
  | Ref t -> Ref (subst_type x tnew t)
  | Forall (y, t) ->
      if y = x then ty else Forall (y, subst_type x tnew t)
  | _ -> ty

let subst_eq x tnew eqs =
  List.map (fun (l, r) -> (subst_type x tnew l, subst_type x tnew r)) eqs

(* ======================================================================== *)
(*                      UNIFICATION : UNE ÉTAPE                             *)
(* ======================================================================== *)

let rec unify eqs =
  match eqs with
  | [] -> []
  | (t1, t2) :: rest ->
      if t1 = t2 then unify rest
      else
        match (t1, t2) with

        (* Variable de type *)
        | TVar x, t | t, TVar x ->
            if occur x t then failwith "Occur-check failed"
            else unify (subst_eq x t rest)

        (* Types flèche *)
        | Arr(a1, a2), Arr(b1, b2) ->
            unify ((a1, b1) :: (a2, b2) :: rest)

        (* Types de liste *)
        | TList a, TList b ->
            unify ((a, b) :: rest)

        (* Références *)
        | Ref a, Ref b ->
            unify ((a, b) :: rest)

        | _ ->
            failwith "Unification failed: incompatible types"

(* ======================================================================== *)
(*                   GÉNÉRATION D’ÉQUATIONS DE TYPAGE                       *)
(* ======================================================================== *)

let rec gen (e:env) (t:lambda) (target:ptype) : equation list =
  match t with

  (* Variable *)
  | Var x ->
      let tx =
        try List.assoc x e
        with Not_found -> failwith ("Variable "^x^" non typée")
      in
      [ (tx, target) ]

  (* Abstraction λx.M *)
  | Abs (x, body) ->
      let ta = fresh_tvar () in
      let tr = fresh_tvar () in
      (Arr(ta, tr), target)
      :: gen ((x, ta) :: e) body tr

  (* Application M N *)
  | App (m, n) ->
      let ta = fresh_tvar () in
      let eq1 = gen e m (Arr(ta, target)) in
      let eq2 = gen e n ta in
      eq1 @ eq2

  (* Entier *)
  | Int _ ->
      [(Nat, target)]

  (* Addition / soustraction / multiplication *)
  | Add (a, b)
  | Sub (a, b)
  | Mult (a, b) ->
      let eq1 = gen e a Nat in
      let eq2 = gen e b Nat in
      eq1 @ eq2 @ [(Nat, target)]

  (* Listes *)
  | List l ->
      let te = fresh_tvar () in
      let rec gen_list ll =
        match ll with
        | Empty -> []
        | Cons (h, t) -> gen e h te @ gen_list t
      in
      gen_list l @ [ (TList te, target) ]

  (* head *)
  | Head t1 ->
      let te = fresh_tvar () in
      gen e t1 (TList te) @ [ (te, target) ]

  (* tail *)
  | Tail t1 ->
      let te = fresh_tvar () in
      gen e t1 (TList te) @ [ (TList te, target) ]

  (* IfZero *)
  | IfZero (c, t1, t2) ->
      gen e c Nat
      @ gen e t1 target
      @ gen e t2 target

  (* IfEmpty *)
  | IfEmpty (c, t1, t2) ->
      let te = fresh_tvar () in
      gen e c (TList te)
      @ gen e t1 target
      @ gen e t2 target

  (* Fix *)
  | Fix f ->
      let tf = fresh_tvar () in
      gen e f (Arr(tf, tf))
      @ [(tf, target)]

  (* Let x = e1 in e2 *)
  | Let (x, e1, e2) ->
      let tx = fresh_tvar () in
      let eq1 = gen e e1 tx in
      let e' = (x, tx) :: e in
      eq1 @ gen e' e2 target

  (* Références *)
  | Reference t1 ->
      let te = fresh_tvar () in
      gen e t1 te @ [ (Ref te, target) ]

  | Dereference t1 ->
      let te = fresh_tvar () in
      gen e t1 (Ref te) @ [ (te, target) ]

  | Address _ ->
      failwith "Impossible de typer une adresse brute"

  | AssignTo (t1, t2) ->
      let te = fresh_tvar () in
      gen e t1 (Ref te)
      @ gen e t2 te
      @ [ (UnitT, target) ]

  | Unit -> [ (UnitT, target) ]

(* ======================================================================== *)
(*                             INFÉRENCE DE TYPE                            *)
(* ======================================================================== *)

let infer t =
  let tv = fresh_tvar () in
  let eqs = gen [] t tv in
  let _ = unify eqs in
  tv
