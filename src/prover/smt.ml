(********************************************************
   This file is part of jStar 
	src/prover/smt.ml
   Release 
        $Release$
   Version 
        $Rev$
   $Copyright$
   
   jStar is distributed under a BSD license,  see, 
      LICENSE.txt
 ********************************************************)
 
(*
TODO list: 

- mark things that are constructors as such in the term structure; 
  at the moment they're treated as predicates. 
  
- Fix the handling of errors: we should get the SMT back to a vanilla
  state after faulting. 

*)

open Psyntax
open Clogic
open Cterm
open Congruence
open Unix
open List
open Backtrack

exception SMT_error of string

let smt_run = ref false;; 
let smt_fdepth = ref 0;; 
let smtout = ref Pervasives.stdin;; 
let smtin = ref Pervasives.stdout;;
let smterr = ref Pervasives.stdin;;

let solver_path = ref "z3";;


let smt_init 
    (path : string) 
    : unit = 
  if !(Debug.debug_ref) then Format.printf "Initialising SMT\n"; 
  let o, i, e = Unix.open_process_full path (environment()) in 
  smtout := o;
  smtin := i;
  smterr := e;
  smt_run := true; 
  if !(Debug.debug_ref) then Format.printf "SMT running...\n"


let rec unzipmerge xs = 
  match xs with 
  | [] -> []
  | ((a,b)::xs) -> a::b::(unzipmerge xs)


let unzip (xs : ('a * 'b) list) : ('a list * 'b list) = 
  fold_right (fun (e1,e2) (l1, l2) -> ((e1::l1),(e2::l2)) ) xs ([],[])
  

let rec do_n (n : int) (f : unit -> unit) : unit =
  match n with 
  | 0 -> () 
  | n -> f() ; do_n (n-1) f


(* concatenate n instances of string s *)
let rec nstr (s : string) (n : int) : string =
  match n with 
  | 0 -> ""
  | _ -> s^(nstr s (n-1))
  
  
(* Partition a list into sublists of equal elements *)
let rec equiv_partition
    (eq : 'a -> 'a -> bool) 
    (xs : 'a list)
    : 'a list list = 
  match xs with 
  | x::xs -> 
     let (e, xs') = List.partition (eq x) xs in 
     let eqs = equiv_partition eq xs' in 
     (x::e) :: eqs
  | [] -> []


let rec list_to_pairs 
    (xs : 'a list) 
    : ('a * 'a) list = 
  match xs with 
  | x::xs -> (List.map (fun y -> (x,y)) xs) @ list_to_pairs xs
  | [] -> [] 


(* Datatype to hold smt type annotations (which btw are essentially guesses) *)
type smt_type = 
  | SMT_Var of Vars.var
  | SMT_Pred of string * int 


module SMTTypeSet = 
  Set.Make(struct
    type t = smt_type
    let compare = compare
  end)
type smttypeset = SMTTypeSet.t


let make_smttype_pred (p : string * Psyntax.args) : smt_type = 
  match p with s, a -> 
  match a with Arg_op ("tuple",al) -> SMT_Pred(s,(length al))


let smt_union_list (l : smttypeset list) : smttypeset = 
  fold_right SMTTypeSet.union l SMTTypeSet.empty        



(* Functions to convert various things to sexps & get types *)

let string_sexp_eq (a : Psyntax.args * Psyntax.args) : string =
  match a with a1, a2 -> 
  Format.fprintf Format.str_formatter "(= %a %a)" 
                 Psyntax.string_args_sexp a1 Psyntax.string_args_sexp a2;
  Format.flush_str_formatter()


let string_sexp_neq (a : Psyntax.args * Psyntax.args) : string =
  match a with a1, a2 -> 
  Format.fprintf Format.str_formatter "(distinct %a %a)" 
                 Psyntax.string_args_sexp a1 Psyntax.string_args_sexp a2;
  Format.flush_str_formatter()

  
let string_sexp_pred (p : string * Psyntax.args) : (string * smttypeset)= 
  match p with s, a -> 
  let types = fold_right (fun x -> SMTTypeSet.add (SMT_Var(x))) (get_vars a) SMTTypeSet.empty in 
  let types = SMTTypeSet.add (make_smttype_pred p) types in 
  match a with Arg_op ("tuple",al) ->
       Format.fprintf Format.str_formatter "(%s %a)"
             s string_args_list_sexp al; 
       ( Format.flush_str_formatter(), types )


let rec string_sexp_form 
    (ts : term_structure)
    (form : formula) 
    : (string * smttypeset) = 
  (* Construct equalities and inequalities *)  
  let eqs = map (fun (a1,a2) -> ((get_pargs false ts [] a1),
                                   (get_pargs false ts [] a2))) form.eqs in 
  let neqs = map (fun (a1,a2) -> ((get_pargs false ts [] a1),
                                    (get_pargs false ts [] a2))) form.neqs in 
  let eq_sexp = String.concat " " (map string_sexp_eq eqs) in 
  let neq_sexp = String.concat " " (map string_sexp_eq neqs) in 
  
  let eq_types = fold_right (fun x -> SMTTypeSet.add (SMT_Var(x))) 
                 (flatten (map get_vars (unzipmerge (eqs@neqs)))) 
                 SMTTypeSet.empty in 
  
  let disj_list, disj_type_list = 
     unzip (map (fun (f1,f2) -> 
                  let f1s, f1v = string_sexp_form ts f1 in 
                  let f2s, f2v = string_sexp_form ts f2 in 
                  ( String.concat "" ["(or "; f1s; f2s; ")"], 
                                    SMTTypeSet.union f1v f2v ) ) form.disjuncts) in 
  let disj_sexp = String.concat " " disj_list in 
  let disj_types = smt_union_list disj_type_list in 
  
  let plain_list, plain_type_list = 
     unzip ( map string_sexp_pred
            ( RMSet.map_to_list form.plain 
            (fun (s,r) -> (s, get_pargs false ts [] r)))) in 
  let plain_sexp = String.concat " " plain_list in 

  let plain_types = smt_union_list plain_type_list in                     

  let types = smt_union_list [eq_types; disj_types; plain_types]in 

  let form_sexp = String.concat " " [ "(and true "; 
                                         eq_sexp; 
                                         neq_sexp; 
                                         disj_sexp; 
                                         plain_sexp; ")" ]  in
  (form_sexp, types) 


let string_sexp_decl (t : smt_type) : string = 
  ( match t with 
    | SMT_Var v -> Format.fprintf Format.str_formatter 
                                "(declare-fun %s () Int)\n" 
                                (Vars.string_var v)
    | SMT_Pred (s,i) -> Format.fprintf Format.str_formatter
                                   "(declare-fun %s (%s) Bool)" s (nstr "Int " i)) ;
  Format.flush_str_formatter()
  

(* Main SMT functions *)
let smt_command 
    (cmd : string) 
    : string = 
  if !(Debug.debug_ref) then Format.printf "SMT command: %s\n" cmd; 
  output_string !smtin cmd; 
  output_string !smtin "\n"; 
  flush !smtin; 
  let r = (input_line !smtout) in
  if !(Debug.debug_ref) then Format.printf "Result: %s\n" r;
  if (String.length r >= 6) && (String.sub r 0 6) = "(error" then 
    raise (SMT_error r)
  else r  


let smt_push () : unit = 
  smt_command "(push)"; 
  smt_fdepth := (!smt_fdepth + 1)

  
let smt_pop () : unit = 
  smt_command "(pop)";
  smt_fdepth := (!smt_fdepth - 1) 


(* Check whether two args are equal under the current assumptions *)
let smt_test_eq (a1 : Psyntax.args) (a2 : Psyntax.args) : bool = 
  smt_push; 
  let test_query = String.concat " " ["(assert"; string_sexp_neq (a1,a2); ")"] in 
  smt_command test_query; 
  let r = smt_command "(check-sat)" in 
  smt_pop; 
  (String.length r >= 5) && (String.sub r 0 5) = "unsat"


(* try to establish that the pure parts of a sequent are valid using the SMT solver *)
let finish_him 
    (ts : term_structure)
    (asm : formula)
    (obl : formula)
    : bool = 
  try
    (* Push a frame to allow reuse of prover *)
    smt_push; 
  
    (* Construct equalities and ineqalities from ts *)
    let eqs = filter (fun (a,b) -> a <> b) (get_eqs ts) in
    let neqs = filter (fun (a,b) -> a <> b) (get_neqs ts) in 
    let asm_eq_sexp = String.concat " " (map string_sexp_eq eqs) in 
    let asm_neq_sexp = String.concat " " (map string_sexp_neq neqs) in
    
    let ts_types = fold_right (fun x -> SMTTypeSet.add (SMT_Var(x))) 
                              (flatten (map get_vars (get_args_all ts))) 
                              SMTTypeSet.empty in 
    
    (* Construct sexps and types for assumption and obligation *)
    let asm_sexp, asm_types = string_sexp_form ts asm in 
    let obl_sexp, obl_types = string_sexp_form ts obl in
    
    let types = smt_union_list [ts_types; asm_types; obl_types] in 
    
    (* declare variables and predicates *)
    SMTTypeSet.iter (fun x -> smt_command (string_sexp_decl x);()) types; 

    (* Construct and run the query *)                    
    let query = String.concat " " [ "(assert (not (=> (and "; 
                                      asm_eq_sexp; asm_neq_sexp; asm_sexp; ") "; 
                                      obl_sexp; 
                                     ")))"] 
    in                         
    smt_command query;    
                                      
    let r = smt_command "(check-sat)" in 
    smt_pop;
    (* check whether the forumula is unsatisfiable *)
    (String.length r >= 5) && (String.sub r 0 5) = "unsat"
  with SMT_error r -> 
    let n = !smt_fdepth in do_n n smt_pop; 
    flush Pervasives.stdout;
    Format.printf "\n"; 
    System.warning(); Format.printf "SMT error: %s" r; System.reset(); 
    false
  

let true_sequent_smt (seq : sequent) : bool =  
  (Clogic.true_sequent seq)
    ||
  (* Call the SMT if the other check fails *)
  (if (not !smt_run) then false 
  else 
  (if !(Debug.debug_ref) 
   then Format.printf "Calling SMT to prove\n %a\n" Clogic.pp_sequent seq; 
   Clogic.plain seq.assumption 
    &&
   Clogic.plain seq.obligation 
    && 
   finish_him seq.ts seq.assumption seq.obligation))


let frame_sequent_smt (seq : sequent) : bool = 
  (seq.obligation = empty) 
    ||
  (if (not !smt_run) then false 
  else 
  (if !(Debug.debug_ref) 
   then Format.printf "Calling SMT to get frame from\n %a\n" Clogic.pp_sequent seq; 
   Clogic.plain seq.obligation
    && 
   finish_him seq.ts seq.assumption seq.obligation))



(* Update the congruence closure using the SMT solver *)
let ask_the_audience 
    (ts : term_structure)
    (form : formula)
    : term_structure = 
  if (not !smt_run) then raise No_match 
  else try 
    if !(Debug.debug_ref) then Format.printf "Calling SMT to update congruence closure\n"; 
  
    smt_push; 
    
    (* Construct equalities and ineqalities from ts *)
    let eqs = filter (fun (a,b) -> a <> b) (get_eqs ts) in
    let neqs = filter (fun (a,b) -> a <> b) (get_neqs ts) in 
    let ts_eq_sexp = String.concat " " (map string_sexp_eq eqs) in 
    let ts_neq_sexp = String.concat " " (map string_sexp_neq neqs) in
  
    let ts_types = fold_right (fun x -> SMTTypeSet.add (SMT_Var(x))) 
                              (flatten (map get_vars (get_args_all ts))) 
                              SMTTypeSet.empty in 
  
    (* Construct sexps and types for assumption and obligation *)
    let form_sexp, form_types = string_sexp_form ts form in 
  
    let types = smt_union_list [ts_types; form_types] in 
  
    (* declare variables and predicates *)
    SMTTypeSet.iter (fun x -> smt_command (string_sexp_decl x);()) types; 

    (* Assert the assumption *)                    
    let assm_query = String.concat " " [ "(assert (and "; ts_eq_sexp; 
                                      ts_neq_sexp; form_sexp; ")) " ] 
    in smt_command assm_query;    

    (* check for a contradiction *)
    let r = smt_command "(check-sat)" in 
    if (String.length r >= 5) && (String.sub r 0 5) = "unsat" 
    then (if !(Debug.debug_ref) then Format.printf "SMT found contradiction in assumption\n"; 
          raise Assm_Contradiction); 

    (* check whether there are new equalities to find; otherwise raise No_Match *)
    smt_push; 
    let reps = get_args_rep ts in 
    let rep_sexps = String.concat " " (List.map (fun (x,y) -> string_sexp_neq (snd x,snd y)) 
                                                (list_to_pairs reps) )
    in 
    let reps_query = String.concat " " [ "(assert (and "; rep_sexps; "))"] 
    in 
    smt_command reps_query; 
    let r = smt_command "(check-sat)" in 
    smt_pop; 
    if ((String.length r >= 3) && (String.sub r 0 3) = "sat") then raise No_match; 

    (* Update the term structure using the new equalities *)  
    let req_equiv = map (map fst)
                        (equiv_partition (fun x y -> smt_test_eq (snd x) (snd y)) reps) 
    in 
    smt_pop;
    fold_left make_list_equal ts req_equiv
  with SMT_error r -> 
    let n = !smt_fdepth in do_n n smt_pop; 
    flush Pervasives.stdout;
    Format.printf "\n"; 
    System.warning(); Format.printf "SMT error: %s" r; System.reset(); 
    raise No_match
  