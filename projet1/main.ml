open Ast
open Pretty
open Utils
open Semantics
open Types
open Typing

(* Fichier de log *)
let log_channel = open_out "resultats.txt"

let log s =
  print_endline s;            (* Affiche à l'écran *)
  output_string log_channel s;   (* Ecrit dans le fichier *)
  output_string log_channel "\n"

let separator title =
  let line = "================ " ^ title ^ " ================" in
  log line

(* ======================= TEST 1 : Pretty Printer ======================= *)
let test_pretty () =
  separator "TEST PRETTY";
  let t = App(Abs("x", Var "x"), Int 5) in
  log (string_of_lambda t)

(* ======================= TEST 2 : Réduction simple ======================= *)
let test_eval_simple () =
  separator "TEST EVAL SIMPLE";
  let term = App(Abs("x", Add(Var "x", Int 3)), Int 7) in
  let (nf, _) = cbv_norm term [] in
  log ("Résultat = " ^ string_of_lambda nf)

(* ======================= TEST 3 : Listes ======================= *)
let test_lists () =
  separator "TEST LISTES";
  let lst = List(Cons(Int 1, Cons(Int 2, Empty))) in
  let (hn, _) = cbv_norm (Head lst) [] in
  let (tn, _) = cbv_norm (Tail lst) [] in
  log ("Head = " ^ string_of_lambda hn);
  log ("Tail = " ^ string_of_lambda tn)

(* ======================= TEST 4 : Fixpoint ======================= *)
let fact =
  Fix(
    Abs("fact",
        Abs("n",
            IfZero(Var "n",
                   Int 1,
                   Mult(Var "n",
                        App(Var "fact", Sub(Var "n", Int 1))
                   )
            )
        )
    )
  )

let test_fix () =
  separator "TEST FIX (fact 5)";
  let (nf, _) = cbv_norm (App(fact, Int 5)) [] in
  log ("Résultat = " ^ string_of_lambda nf)

(* ======================= TEST 5 : Références ======================= *)
let test_ref () =
  separator "TEST REFERENCES";
  let term = Let("x", Reference (Int 10), AssignTo(Var "x", Int 42)) in
  let (nf, mem') = cbv_norm term [] in
  log ("Résultat = " ^ string_of_lambda nf);
  log ("Mémoire finale = " ^ Utils.show_memory mem')

(* ======================= TEST 6 : Typage ======================= *)
let test_typing () =
  separator "TEST TYPAGE";
  let ty = infer (Abs("x", Add(Var "x", Int 3))) in
  log ("Type = " ^ string_of_ptype ty)

(* ======================= MAIN ======================= *)
let () =
  test_pretty ();
  test_eval_simple ();
  test_lists ();
  test_fix ();
  test_ref ();
  test_typing ();
  log "================ FIN DES TESTS ================";
  close_out log_channel
