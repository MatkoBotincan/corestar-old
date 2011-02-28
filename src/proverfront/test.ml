(********************************************************
   This file is part of jStar
        src/proverfront/test.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   jStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


(* TODO(rgrig): Factor the common parts of test.ml and run.ml. *)

open Debug
open Format
open Load_logic
open Psyntax

let program_file_name = ref ""
let logic_file_name = ref ""
let inductive_file_name = ref ""
 
let arg_list = Config.args_default @ 
  [ ("-f", Arg.Set_string(program_file_name), "program file name" );
    ("-l", Arg.Set_string(logic_file_name), "logic file name" ); 
    ("-i", Arg.Set_string(inductive_file_name), "inductive file name" ); ]



let main () =
  let usage_msg="Usage: -f <test_file_name> -l <logic_file_name> [-i <inductive_file_name>]" in 
  Arg.parse arg_list (fun s ->()) usage_msg;

  if !program_file_name="" then 
    printf "Test file name not specified. Can't continue....@\n %s @\n" usage_msg
  else if !logic_file_name="" then
    printf "Logic file name not specified. Can't continue....@\n %s @\n" usage_msg
  else 
    if !Config.smt_run then Smt.smt_init();
    (* Load abstract interpretation plugins *)
    List.iter (fun file_name -> Plugin_manager.load_plugin file_name) !Config.abs_int_plugins;

    let rl = if !inductive_file_name <> "" then Inductive.convert_inductive_file !inductive_file_name else [] in
    let l1,l2,cn = load_logic_extra_rules Cli_utils.logic_dirs !logic_file_name rl in
    let logic = {empty_logic with seq_rules = l1; rw_rules=l2; consdecl = cn;} in
    let s = System.string_of_file !program_file_name  in
    if log log_phase then 
      fprintf logf "@[<4>Parsing tests in@ %s.@." !program_file_name;
    let test_list  = Jparser.test_file Jlexer.token (Lexing.from_string s) in
    if log log_phase then fprintf logf "@[<4>Parsed@ %s.@." !program_file_name;
    List.iter (
    fun test ->
      match test with 
    | Psyntax.TImplication (heap1,heap2,result) ->
	(*Format.printf "Check implication\n %s\n ===> \n %s\n" (Plogic.string_form heap1) (Plogic.string_form heap2);*)
	(match (Sepprover.implies_opt logic (Sepprover.convert heap1) heap2), result with 
	  true,true | false,false -> Format.printf "."
	| true,false -> Format.printf "Test failed! Unsound as proved @\n@ %a@\n@ ===> @\n%a@\n " 
	      Psyntax.string_form heap1 
	      Psyntax.string_form heap2
	| false,true -> Format.printf "Test@ failed!@ Could@ not@ prove@ @\n@ %a@\n ===> @\n%a@\n " 
	      Psyntax.string_form heap1 
	      Psyntax.string_form heap2
	)
    | Psyntax.TFrame (heap1, heap2, result)  -> 
(*	Format.printf "Find frame for\n %s\n ===> \n %s\n" (Psyntax.string_form heap1) (Psyntax.string_form heap2);*)
	let x = Sepprover.frame_opt logic 
	    (Sepprover.convert heap1) heap2 in 
	begin 
	  match x with 
	  None -> Format.printf "Incorrect: cannot find frame. @\n%a@\n ===> @\n%a@\n" Psyntax.string_form heap1  Psyntax.string_form heap2
	| Some x -> 
	if Sepprover.implies_list x result then Format.printf "."
	else (
	  Format.printf "Incorrect frame for:@\n%a@\n ===> @\n%a@\n"
	      Psyntax.string_form heap1 
	      Psyntax.string_form heap2;
	  List.iter 
	      (fun form -> 
		Format.printf "Resulted in frames:@\n %a@\n" Sepprover.string_inner_form form) x;
	  Format.printf "Was expecting:@\n%a@\n" Psyntax.string_form result
	 )
	end
    | Psyntax.TAbs (heap1,result)  -> 
	let x = Sepprover.abs_opt logic (Sepprover.convert heap1) in
	if Sepprover.implies_list x result then Format.printf "."
	else (
	  Format.printf "Incorrect Abstraction for:@\n%a@\n "
	      Psyntax.string_form heap1;
	  List.iter 
	      (fun form -> 
		Format.printf "Resulted in forms:@\n %a@\n" Sepprover.string_inner_form form) x;
	  Format.printf "Was expecting:@\n%a@\n" Psyntax.string_form result
	 )	
    | Psyntax.TInconsistency (heap1,result) ->
	(match Sepprover.inconsistent_opt logic (Sepprover.convert heap1), result with 
	  true, true 
	| false,false -> Format.printf "."
	| true,false -> Format.printf "Test failed! Prover found@ %a@ inconsistent, test said consistent.@\n" 
	      Psyntax.string_form heap1
	| false,true -> Format.printf "Test failed! Prover could not prove@ %a@ inconsistent.@\n" 
	      Psyntax.string_form heap1
	);
    | Psyntax.TEqual (heap,arg1,arg2,result) -> ()
(*	if Prover.check_equal logic heap arg1 arg2 
	then Format.printf("Equal!\n\n") else Format.printf("Not equal!\n\n")*)
  )
      test_list


let _ = main ()
