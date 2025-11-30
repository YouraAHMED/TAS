(* ========================================================================= *)
(*                       SEMANTIQUE DU LANGAGE TAS                           *)
(* ========================================================================= *)

open Ast
open Utils
open Pretty

(* ======================================================================== *)
(*                    ALPHA-CONVERSION (Barendregt)                         *)
(* ======================================================================== *)

(* Générateur de noms de variables frais *)
let compteur_var = ref 0

let fresh_var () =
  incr compteur_var;
  "x" ^ string_of_int !compteur_var

(* alpha_convert t env
   env : association (ancienne_var, nouvelle_var)
   On renomme toutes les variables LIÉES pour respecter Barendregt :
   - variables liées distinctes entre elles
   - variables liées distinctes des variables libres
*)
let rec alpha_convert (t : lambda) (env : (string * string) list) : lambda =
  match t with
  | Var x ->
      (* si x est dans env, on le remplace, sinon on le laisse *)
      (match List.assoc_opt x env with
       | Some y -> Var y
       | None -> Var x)

  | Abs (x, body) ->
      (* on génère un nouveau nom pour la variable liée *)
      let x' = fresh_var () in
      let env' = (x, x') :: env in
      Abs (x', alpha_convert body env')

  | App (t1, t2) ->
      App (alpha_convert t1 env, alpha_convert t2 env)

  (* Partie entiers / listes / contrôle / let / fix *)
  | Int n -> Int n
  | Add (t1, t2) -> Add (alpha_convert t1 env, alpha_convert t2 env)
  | Sub (t1, t2) -> Sub (alpha_convert t1 env, alpha_convert t2 env)
  | Mult (t1, t2) -> Mult (alpha_convert t1 env, alpha_convert t2 env)

  | List l ->
      List (alpha_convert_list l env)

  | Head t1 -> Head (alpha_convert t1 env)
  | Tail t1 -> Tail (alpha_convert t1 env)

  | IfZero (c, t1, t2) ->
      IfZero (alpha_convert c env,
              alpha_convert t1 env,
              alpha_convert t2 env)

  | IfEmpty (c, t1, t2) ->
      IfEmpty (alpha_convert c env,
               alpha_convert t1 env,
               alpha_convert t2 env)

  | Fix t1 ->
      Fix (alpha_convert t1 env)

  | Let (x, e1, e2) ->
      let x' = fresh_var () in
      let env' = (x, x') :: env in
      Let (x',
           alpha_convert e1 env',
           alpha_convert e2 env')

  (* Partie impérative *)
  | Unit -> Unit
  | Reference e -> Reference (alpha_convert e env)
  | Address a -> Address a
  | Dereference e -> Dereference (alpha_convert e env)
  | AssignTo (e1, e2) ->
      AssignTo (alpha_convert e1 env, alpha_convert e2 env)

(* alpha-conversion pour les listes *)
and alpha_convert_list (l : lambda liste) (env : (string * string) list) :
    lambda liste =
  match l with
  | Empty -> Empty
  | Cons (hd, tl) ->
      Cons (alpha_convert hd env, alpha_convert_list tl env)

(* ======================================================================== *)
(*                            SUBSTITUTION                                  *)
(* ======================================================================== *)

(* subst x v t
   remplace toutes les occurrences LIBRES de x dans t par v
*)
let rec subst (x : string) (v : lambda) (t : lambda) : lambda =
  match t with
  | Var y ->
      if y = x then v else t

  | Abs (y, body) ->
      if y = x then
        (* x est liée ici, on ne substitue pas sous ce lambda *)
        Abs (y, body)
      else
        Abs (y, subst x v body)

  | App (t1, t2) ->
      App (subst x v t1, subst x v t2)

  (* Extensions : entiers / listes / op arithmétiques *)
  | Int n -> Int n
  | Add (t1, t2) -> Add (subst x v t1, subst x v t2)
  | Sub (t1, t2) -> Sub (subst x v t1, subst x v t2)
  | Mult (t1, t2) -> Mult (subst x v t1, subst x v t2)

  | List l ->
      List (subst_list x v l)

  | Head t1 -> Head (subst x v t1)
  | Tail t1 -> Tail (subst x v t1)

  | IfZero (c, t1, t2) ->
      IfZero (subst x v c, subst x v t1, subst x v t2)

  | IfEmpty (c, t1, t2) ->
      IfEmpty (subst x v c, subst x v t1, subst x v t2)

  | Fix t1 ->
      Fix (subst x v t1)

  | Let (y, e1, e2) ->
      let e1' = subst x v e1 in
      if y = x then
        (* y masque x, on ne substitue pas dans e2 *)
        Let (y, e1', e2)
      else
        Let (y, e1', subst x v e2)

  (* Partie impérative *)
  | Unit -> Unit
  | Address a -> Address a
  | Reference e -> Reference (subst x v e)
  | Dereference e -> Dereference (subst x v e)
  | AssignTo (e1, e2) ->
      AssignTo (subst x v e1, subst x v e2)

and subst_list (x : string) (v : lambda) (l : lambda liste) : lambda liste =
  match l with
  | Empty -> Empty
  | Cons (hd, tl) ->
      Cons (subst x v hd, subst_list x v tl)

(* ======================================================================== *)
(*                        PETITE SÉMANTIQUE LTR-CBV                         *)
(* ======================================================================== *)

(* On sépare bien la notion de valeur (au sens CBV) *)
let rec is_value (t : lambda) : bool =
  match t with
  | Abs _ -> true
  | Int _ -> true
  | Unit -> true
  | Address _ -> true
  | List l -> is_value_list l
  (* Selon ton choix, tu peux considérer qu'une liste de valeurs est une valeur *)
  | _ -> false

and is_value_list (l : lambda liste) : bool =
  match l with
  | Empty -> true
  | Cons (hd, tl) -> is_value hd && is_value_list tl

(* Une étape de réduction LTR CBV avec état mémoire *)
let rec step_ltr_cbv (t : lambda) (mem : memory)
  : (lambda * memory) option =
  match t with
  (* β-réduction (λx.body) v *)
  | App (Abs (x, body), v) when is_value v ->
      Some (subst x v body, mem)

  (* Application générale : on évalue d'abord la fonction, puis l'argument *)
  | App (t1, t2) ->
      begin
        match step_ltr_cbv t1 mem with
        | Some (t1', mem') -> Some (App (t1', t2), mem')
        | None ->
            if is_value t1 then
              match step_ltr_cbv t2 mem with
              | Some (t2', mem') -> Some (App (t1, t2'), mem')
              | None -> None
            else
              None
      end

  (* Addition *)
  | Add (t1, t2) ->
      begin
        match step_ltr_cbv t1 mem with
        | Some (t1', mem') -> Some (Add (t1', t2), mem')
        | None ->
            match step_ltr_cbv t2 mem with
            | Some (t2', mem') -> Some (Add (t1, t2'), mem')
            | None ->
                (match t1, t2 with
                 | Int n1, Int n2 -> Some (Int (n1 + n2), mem)
                 | _ -> failwith "Addition attend deux entiers")
      end

  (* Soustraction *)
  | Sub (t1, t2) ->
      begin
        match step_ltr_cbv t1 mem with
        | Some (t1', mem') -> Some (Sub (t1', t2), mem')
        | None ->
            match step_ltr_cbv t2 mem with
            | Some (t2', mem') -> Some (Sub (t1, t2'), mem')
            | None ->
                (match t1, t2 with
                 | Int n1, Int n2 -> Some (Int (n1 - n2), mem)
                 | _ -> failwith "Soustraction attend deux entiers")
      end

  (* Multiplication *)
  | Mult (t1, t2) ->
      begin
        match step_ltr_cbv t1 mem with
        | Some (t1', mem') -> Some (Mult (t1', t2), mem')
        | None ->
            match step_ltr_cbv t2 mem with
            | Some (t2', mem') -> Some (Mult (t1, t2'), mem')
            | None ->
                (match t1, t2 with
                 | Int n1, Int n2 -> Some (Int (n1 * n2), mem)
                 | _ -> failwith "Multiplication attend deux entiers")
      end


  (* Head / Tail : on laisse la gestion du test de liste à Utils *)
  | Head t1 ->
      let l = is_list t1 in
      Some (get_head_list l, mem)

  | Tail t1 ->
      let l = is_list t1 in
      Some (get_tail_list l, mem)

  (* IfZero *)
  | IfZero (Int 0, t1, _) -> Some (t1, mem)
  | IfZero (Int _, _, t2) -> Some (t2, mem)
  | IfZero (c, t1, t2) ->
      begin
        match step_ltr_cbv c mem with
        | Some (c', mem') -> Some (IfZero (c', t1, t2), mem')
        | None -> failwith "Condition de IfZero non réductible"
      end

  (* IfEmpty *)
  | IfEmpty (List l, t1, t2) ->
      (match l with
       | Empty -> Some (t1, mem)
       | Cons _ -> Some (t2, mem))
  | IfEmpty (c, t1, t2) ->
      begin
        match step_ltr_cbv c mem with
        | Some (c', mem') -> Some (IfEmpty (c', t1, t2), mem')
        | None -> failwith "Condition de IfEmpty non réductible"
      end

  (* Fixpoint : fix (λf.body) → body[fix (λf.body)/f] *)
  | Fix (Abs (f, body)) ->
      Some (subst f t body, mem)

  | Fix t1 ->
      begin
        match step_ltr_cbv t1 mem with
        | Some (t1', mem') -> Some (Fix t1', mem')
        | None -> None
      end

  (* Let x = e1 in e2 : on commence par évaluer e1 *)
  | Let (x, e1, e2) ->
      begin
        match step_ltr_cbv e1 mem with
        | Some (e1', mem') -> Some (Let (x, e1', e2), mem')
        | None ->
            if is_value e1 then
              Some (subst x e1 e2, mem)
            else
              None
      end

  (* Partie impérative *)

  (* ref e : on évalue e, puis on alloue une nouvelle adresse *)
  | Reference e ->
      begin
        match step_ltr_cbv e mem with
        | Some (e', mem') -> Some (Reference e', mem')
        | None ->
            if is_value e then
              let (adr, mem') = memory_extend e mem in
              Some (Address adr, mem')
            else
              None
      end

  (* !e : on évalue e jusqu'à une adresse, puis on lit dans la mémoire *)
  | Dereference e ->
      begin
        match step_ltr_cbv e mem with
        | Some (e', mem') -> Some (Dereference e', mem')
        | None ->
            (match e with
             | Address a ->
                 (match mem_lookup a mem with
                  | Some v -> Some (v, mem)
                  | None -> failwith "Adresse non trouvée dans la mémoire")
             | _ -> failwith "Déréférencement sur une expression non adresse")
      end

  (* e1 := e2 *)
  | AssignTo (e1, e2) ->
      begin
        match step_ltr_cbv e1 mem with
        | Some (e1', mem') -> Some (AssignTo (e1', e2), mem')
        | None ->
            match e1 with
            | Address a ->
                begin
                  match step_ltr_cbv e2 mem with
                  | Some (e2', mem') -> Some (AssignTo (e1, e2'), mem')
                  | None ->
                      if is_value e2 then
                        let mem' = mem_update a e2 mem in
                        Some (Unit, mem')
                      else
                        None
                end
            | _ -> failwith "Assignation sur une expression non adresse"
      end

  | _ -> None

(* ======================================================================== *)
(*                     NORMALISATION ET AFFICHAGE                           *)
(* ======================================================================== *)

let rec normalize (t : lambda) (mem : memory)
                  (step : lambda -> memory -> (lambda * memory) option)
  : lambda * memory =
  match step t mem with
  | Some (t', mem') -> normalize t' mem' step
  | None -> (t, mem)

let normalize_cbv t mem =
  normalize t mem step_ltr_cbv

(* Version avec timeout pour éviter les boucles infinies *)
let normalize_with_timeout t mem step time_limit =
  let start = Sys.time () in
  let rec loop t mem =
    if Sys.time () -. start > time_limit then
      None
    else
      match step t mem with
      | Some (t', mem') -> loop t' mem'
      | None -> Some (t, mem)
  in
  loop t mem

(* Affichage des étapes de réduction *)
let rec print_reduction_steps t mem step =
  print_endline (string_of_lambda t);
  match step t mem with
  | Some (t', mem') ->
      Printf.printf "=> ";
      print_reduction_steps t' mem' step
  | None ->
      Printf.printf "=> (normal form)\n"

(* Version spécialisée pour notre stratégie cbv *)
let cbv_norm t mem = normalize_cbv t mem
let cbv_norm_timeout t mem time_limit =
  normalize_with_timeout t mem step_ltr_cbv time_limit

let print_reduction_cbv t mem =
  print_reduction_steps t mem step_ltr_cbv
