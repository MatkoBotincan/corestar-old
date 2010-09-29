(********************************************************
   This file is part of jStar 
	src/prover/prover.ml
   Release 
        $Release$
   Version 
        $Rev$
   $Copyright$
   
   jStar is distributed under a BSD license,  see, 
      LICENSE.txt
 ********************************************************)


open Backtrack
open Clogic
open Cterm
open Debug
open Format
open Misc
open Psyntax
open Backtrack
open Smt
open Vars

let prover_counter_example : Clogic.sequent list ref = ref []

let pprint_counter_example ppf () = 
  fprintf ppf "Needed to prove:@   @[%a@]@\n@\n"
    (list_format "\nor" Clogic.pp_sequent)
    !prover_counter_example

let print_counter_example = pprint_counter_example std_formatter
  
let get_counter_example () =
  let out_buff = Buffer.create 1000 in
  let out_ft = Format.formatter_of_buffer out_buff in
  pprint_counter_example out_ft ();
  Format.pp_print_flush out_ft ();
  let r = Buffer.contents out_buff in
  Buffer.clear out_buff;
  r

let pprint_proof (f : formatter) : unit = 
  fprintf f "%s" (Buffer.contents buffer_dump)

let string_of_proof () =
  Buffer.contents buffer_dump

exception Failed_eg of Clogic.sequent list


let rec apply_rule_list_once 
   (rules : sequent_rule list) 
   (seq : Clogic.sequent) 
   ep 
   : Clogic.sequent list list
   =
  match rules with 
    [] -> raise No_match
  | rule::rules ->
      try 
	Clogic.apply_rule (Clogic.convert_rule rule) seq (*ep*)
      with 
      | No_match -> apply_rule_list_once rules seq ep


let rec sequents_backtrack 
    f (seqss : Clogic.sequent list list) xs 
    : Clogic.sequent list =
  match seqss with 
    [] -> raise (Failed_eg xs)
  | seqs::seqss -> 
      try f seqs 
      with 
	Failed ->  
	  fprintf !proof_dump "Backtracking!@\n"; sequents_backtrack f seqss xs
      | Failed_eg x -> 
	  fprintf !proof_dump "Backtracking!@\n"; sequents_backtrack f seqss (x @ xs)

let apply_rule_list 
    (logic : logic) 
    (sequents : Clogic.sequent list) 
    (must_finish : Clogic.sequent -> bool) 
    (may_finish : Clogic.sequent -> bool) 
    : Clogic.sequent list 
    =
(* Clear pretty print buffer *)
  Buffer.clear buffer_dump;
(*  let rules,rwm,ep = logic in *)
  let n = 0 in
  if log log_prove then
    (List.iter (fun seq -> fprintf !proof_dump "Goal@ %a@\n@\n" Clogic.pp_sequent seq) sequents;
     fprintf !proof_dump "Start time :%f @\n" (Sys.time ()));
  let rec apply_rule_list_inner sequents n : Clogic.sequent list = 
    let search seqss : Clogic.sequent list = 
      sequents_backtrack 
	(fun seqs->apply_rule_list_inner seqs (n+1)) seqss [] in
    let sequents = map_option (Clogic.simplify_sequent logic.rw_rules) sequents in 
  (* Apply rules lots *)
    List.flatten 
      (List.map 
	 (fun seq ->
	   fprintf !proof_dump "%s>@[%a@]@\n@." (String.make n '-') Clogic.pp_sequent  seq;
	   if must_finish seq then 
	     [seq]
	   else
	   try 
	     search (apply_rule_list_once logic.seq_rules seq logic.ext_prover)
	   with No_match -> 
         try
		 if may_finish seq then 
		   [seq]
		 else 
		   search ([Clogic.apply_or_left seq])
	       with No_match -> 
		 try 
		   search (Clogic.apply_or_right seq)
		 with No_match -> 
                 try 
	             let ts' = Smt.ask_the_audience seq.ts seq.assumption in 
	             search [[ {seq with ts = ts'} ]]
                   with 
                   | Assm_Contradiction -> []
                   | No_match -> raise (Failed_eg [seq])
	 ) sequents 
      )
  in let res = apply_rule_list_inner sequents n in 
  if log log_prove then 
    fprintf !proof_dump "@\nEnd time :%f@ " (Sys.time ());
  res

let check_imp (logic : logic) (seq : sequent) : bool = 
    try 
      let ts = List.fold_right Cterm.add_constructor logic.consdecl seq.ts in 
      let seq = {seq with ts = ts} in 
      ignore (apply_rule_list logic [seq] Smt.true_sequent_smt Smt.true_sequent_smt); true
    with  
      Failed -> false
    | Failed_eg x -> prover_counter_example := x ; false

let check_frm (logic : logic) (seq : sequent) : Clogic.ts_formula list option =
  try
    let ts = List.fold_right Cterm.add_constructor logic.consdecl seq.ts in 
    let seq = {seq with ts = ts} in 
    let leaves = apply_rule_list logic [seq] (fun _ -> false) Smt.frame_sequent_smt in 
    Some (Clogic.get_frames leaves)
  with 
    Failed -> fprintf !proof_dump "Foo55";None 
  | Failed_eg x -> fprintf !proof_dump "Foo44"; prover_counter_example := x; None 


let check_implication_frame_pform logic heap pheap  =  
  check_frm logic (Clogic.make_implies heap pheap)


let check_implication_pform 
    (logic : logic) 
    (heap : ts_formula) 
    (pheap : pform) : bool =  
  check_imp logic (Clogic.make_implies heap pheap)


let abs logic ts_form  = 
  match check_frm logic  (Clogic.make_implies ts_form []) with 
    Some r -> r
  | None -> 
      (* Abstraction cannot fail *)
      assert false

let check_implication_syntactic logic pform pform2 = 
  let seq = Clogic.make_sequent (Clogic.convert_sequent ([],pform,pform2)) in
  match seq with 
    None -> true (* Found contradiction immediately *)
  | Some seq -> 
      check_imp logic seq

let check_implication_frame_syntactic logic pform pform2 = 
  let seq = Clogic.make_sequent (Clogic.convert_sequent ([],pform,pform2)) in
  match seq with 
    None -> Some [] (* Found contradiction immediately *)
  | Some seq -> 
      check_frm logic seq
    

let check_implication logic ts_form1 ts_form2 =
  let seq = Clogic.make_implies_inner ts_form1 ts_form2 in 
  check_imp logic seq 

let check_frame logic ts_form1 ts_form2 =
  let seq = Clogic.make_implies_inner ts_form1 ts_form2 in 
  check_frm logic seq 


let check_inconsistency logic ts_form   = assert false
(*  check_implication_inner logic ts heap1 ([],[],[False]) *)




let check_implies_list fl1 pf =
  List.for_all 
    (fun f1 -> 
      check_implication_pform empty_logic f1 pf
    ) fl1 


