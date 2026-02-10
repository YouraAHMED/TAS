open Abstract_syntax_tree
open Abstract_syntax_printer
open Domain

let trace = ref false

let unroll = ref 0 
let delay = ref 0

let iteration_count = ref 0

let error ext s =
  Format.printf "%s: ERROR: %s@\n" (string_of_extent ext) s

let fatal_error ext s =
  Format.printf "%s: FATAL ERROR: %s@\n" (string_of_extent ext) s;
  exit 1

module type INTERPRETER =
sig
  val eval_prog: prog -> unit
end

module Interprete(D : DOMAIN) =
(struct

  type t = D.t


  let filter (a:t) (e:bool_expr ext) (r:bool) : t =

    let rec doit a (e,_) r = match e with

    | AST_bool_unary (AST_NOT, e) ->
        doit a e (not r)
    | AST_bool_binary (AST_AND, e1, e2) ->
        (if r then D.meet else D.join) (doit a e1 r) (doit a e2 r)
    | AST_bool_binary (AST_OR, e1, e2) ->
        (if r then D.join else D.meet) (doit a e1 r) (doit a e2 r)
    | AST_bool_const b ->
        if b = r then a else D.bottom ()

    | AST_compare (cmp, (e1,_), (e2,_)) ->
        let inv = function
        | AST_EQUAL         -> AST_NOT_EQUAL
        | AST_NOT_EQUAL     -> AST_EQUAL
        | AST_LESS          -> AST_GREATER_EQUAL
        | AST_LESS_EQUAL    -> AST_GREATER
        | AST_GREATER       -> AST_LESS_EQUAL
        | AST_GREATER_EQUAL -> AST_LESS
        in
        let cmp = if r then cmp else inv cmp in
        D.compare a e1 cmp e2

    in
    doit a e r


  let rec eval_stat (a:t) ((s,ext):stat ext) : t =
    let r = match s with

    | AST_block (decl,inst) ->
        let a =
          List.fold_left
            (fun a ((_,v),_) -> D.add_var a v)
            a decl
        in
        let a = List.fold_left eval_stat a inst in
        List.fold_left
          (fun a ((_,v),_) -> D.del_var a v)
          a decl

    | AST_assign ((i,_),(e,_)) ->
        D.assign a i e

    | AST_if (e,s1,Some s2) ->
        let t = eval_stat (filter a e true ) s1 in
        let f = eval_stat (filter a e false) s2 in
        D.join t f

    | AST_if (e,s1,None) ->
        let t = eval_stat (filter a e true ) s1 in
        let f = filter a e false in
        D.join t f

   
    | AST_while (e, s) ->

      let body x =
        eval_stat (filter x e true) s
      in

      let rec do_unroll k inside outside =
        if k <= 0 then (inside, outside)
        else
          let cont = filter inside e true in
          let exit = filter inside e false in
          let outside' = D.join outside exit in
          if D.is_bottom cont then
            (D.bottom (), outside')
          else
            let inside' = eval_stat cont s in
            do_unroll (k - 1) inside' outside'
      in

      let (inside0, outside0) = do_unroll !unroll a (D.bottom ()) in

      let step x =
        D.join inside0 (body x)
      in

      let rec fix i x =
        let fx = step x in
        if D.subset fx x then fx
        else
          let next =
            if i < !delay then D.join x fx
            else D.widen x fx
          in
          fix (i + 1) next
      in

      let inv = fix 0 inside0 in
      let exit_after_fp = filter inv e false in

      D.join outside0 exit_after_fp



    | AST_assert e ->
      let res = filter a e false in
      if not (D.is_bottom res) then error ext "assertion failure";
      filter a e true
    

    | AST_print l ->
        let l' = List.map fst l in
        Format.printf "%s: %a@\n"
          (string_of_extent ext) (fun fmt v -> D.print fmt a v) l';
        a

    | AST_PRINT_ALL ->
        Format.printf "%s: %a@\n"
          (string_of_extent ext) D.print_all a;
        a

    | AST_HALT ->
        D.bottom ()
    in

    if !trace then
      Format.printf "stat trace: %s: %a@\n"
        (string_of_extent ext) D.print_all r;
    r

  let eval_prog (l:prog) : unit =
    iteration_count := 0;
    let _ = List.fold_left eval_stat (D.init()) l in
    Format.printf "analysis ended@\n";
    ()

end : INTERPRETER)