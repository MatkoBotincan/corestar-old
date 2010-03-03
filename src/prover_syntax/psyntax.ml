(******************************************************************
    Syntax for Separation logic theorem prover

    Copyright Matthew Parkinson & Dino Distefano
 
*******************************************************************)


(************************************************************
   The syntactic representation of terms.
*************************************************************)
open Debug
open Vars
(*F#
open Microsoft.FSharp.Compatibility
F#*)

(* Main terms *)
type args = 
  | Arg_var of Vars.var
  | Arg_string of string
  | Arg_op of string * args list
  | Arg_cons of string * args list  (* Do not use *)
  | Arg_record of (string *  args) list (* Do not use *)



let mkArgRecord fldlist =
  Arg_record (List.sort (fun (a1,b1) (a2,b2) -> compare a1 a2) fldlist)

type fldlist = (string * args) list

(* Type for substitution on variables in programs *)
(*IF-OCAML*) 
module VarSet =
  Set.Make(struct
    type t = var
    let compare = compare
  end)
type varset = VarSet.t
(*ENDIF-OCAML*)

(*F#
module VarSet = Set
type varset = var VarSet.t
F#*)


let vs_mem = VarSet.mem
let vs_add = VarSet.add
let vs_empty = VarSet.empty
let vs_fold = VarSet.fold
let vs_iter = VarSet.iter
let vs_diff = VarSet.diff
let vs_exists = VarSet.exists
let vs_from_list vl = List.fold_right (fun vs v -> vs_add vs v) vl vs_empty


(*IF-OCAML*)
module VarMap = 
  Map.Make(struct
    type t = Vars.var
    let compare = compare
  end)   

type 'a varmap_t = 'a VarMap.t
(*ENDIF-OCAML*)

(*F#
module VarMap = Map
type 'a varmap_t = VarMap.t<Vars.var,'a> 
F#*)

type varmapargs = args varmap_t

let vm_add = VarMap.add
let vm_empty = VarMap.empty
let vm_find = VarMap.find
let vm_mem = VarMap.mem
let vm_is_empty = VarMap.is_empty

(*IF-OCAML*)
module VarHash = Hashtbl.Make(struct 
    type t = Vars.var
    let equal = (=)
    let hash = Hashtbl.hash
  end)
type varhashargs = args VarHash.t  
type 'a varhash_t = 'a VarHash.t
(*ENDIF-OCAML*)
 
(*F# 
module VarHash = Hashtbl
type 'a varhash_t = VarHash.t<Vars.var,'a>
type 'a varhashargs = ('a args) varhash_t
F#*)

let vh_create = VarHash.create 
let vh_add = VarHash.add
let vh_find = VarHash.find



type varmap = 
  | Plain of varmapargs
  | Freshen of varmapargs *  varhashargs


let find key (map : varmap) = 
  match map with 
    Plain map -> vm_find key map  
  | Freshen (map,h) ->( 
      try vm_find key map 
      with Not_found -> 
	try vh_find h key
	with Not_found -> 
	  let newvar = Arg_var (freshen key) in 
	  vh_add h key newvar;
	  newvar
     )
 
let add (key : Vars.var)  (v : args)  (map : varmap) : varmap =
  match map with 
    Plain map -> Plain (vm_add key v map)
  | Freshen (map,h) -> Freshen((vm_add key v map),h)

let empty : varmap = Plain (vm_empty)


let freshening_subs subs : varmap =
    match subs with 
      Plain subs -> Freshen (subs, vh_create 30 )
    | _ -> unsupported_s "freshening_subs applied to wrong argument type."



let subst_kill_vars_to_fresh_prog vars =
  Plain (vs_fold (fun ev vm -> vm_add  ev (Arg_var (freshp())) vm) vars vm_empty)

let subst_kill_vars_to_fresh_exist vars =
  Plain (vs_fold (fun ev vm -> vm_add  ev (Arg_var (freshe())) vm) vars vm_empty)

let subst_freshen_vars vars = 
  Plain (vs_fold (fun ev vm -> vm_add  ev (Arg_var (freshen ev)) vm) vars vm_empty)


(* substitution code for formula *)
let subst_var subs v = (try find v subs with Not_found -> Arg_var v)

let rec subst_args subs arg : args= 
  match arg with 
  | Arg_var v -> (subst_var subs v)
  | Arg_string s -> Arg_string s
  | Arg_op (name,args) -> Arg_op(name,List.map (subst_args subs) args)
  | Arg_cons (name,args) -> Arg_cons(name,List.map (subst_args subs) args)
  | Arg_record fldlist -> Arg_record (List.map (fun (f,v) -> f,subst_args subs v) fldlist)	

let rec string_args ppf arg = 
  match arg with 
  | Arg_var v -> Format.fprintf ppf "%s" (string_var v)
  | Arg_string s -> Format.fprintf ppf "\"%s\""  s 
  | Arg_op ("builtin_plus",[a1;a2]) -> Format.fprintf ppf "(%a+%a)" string_args a1 string_args a2
  | Arg_op (name,args) -> Format.fprintf ppf "%s(%a)" name string_args_list args 
  | Arg_cons (name,args) -> Format.fprintf ppf "%s(%a)" name string_args_list args 
  | Arg_record fldlist -> 
      Format.fprintf ppf "@[{%a}@]" string_args_fldlist fldlist
and string_args_list ppf argsl = 
  match argsl with 
    [] -> Format.fprintf ppf ""
  | [a] -> Format.fprintf ppf "%a" string_args a
  | a::al -> Format.fprintf ppf "%a,@ %a" string_args a string_args_list al
and string_args_fldlist ppf fdl =  
  match fdl with 
    [] -> Format.fprintf ppf ""
  | [(f,a)] -> Format.fprintf ppf "%s=%a" f string_args a
  | (f,a)::fdl -> Format.fprintf ppf "%s=%a;@ %a" f string_args a string_args_fldlist fdl




let rec fv_args args set = 
  match args with
    Arg_var var -> vs_add var set 
  | Arg_string _ -> set
  | Arg_op (name,argsl) -> fv_args_list argsl set
  | Arg_cons (name,argsl) -> fv_args_list argsl set
  | Arg_record fldlist -> fv_fld_list fldlist set
and fv_args_list argsl set =
  match argsl with 
    [] -> set
  | args::argsl -> fv_args_list argsl (fv_args args set)
and fv_fld_list fldlist set =
  match fldlist with
    [] -> set
  | (f,args)::fldlist -> fv_fld_list fldlist (fv_args args set)
    


let rec ev_args args set = 
  match args with
    Arg_var var -> (match var with EVar _ -> vs_add var set | _ -> set )
  | Arg_string _ -> set
  | Arg_op (name,argsl) -> ev_args_list argsl set
  | Arg_cons (name,argsl) -> ev_args_list argsl set
  | Arg_record fldlist -> ev_fld_list fldlist set
and ev_args_list argsl set =
  match argsl with 
    [] -> set
  | args::argsl -> ev_args_list argsl (ev_args args set)
and ev_fld_list fldlist set =
  match fldlist with
    [] -> set
  | (f,args)::fldlist -> ev_fld_list fldlist (ev_args args set)

(****************************************************************
   Formula
 ****************************************************************)

type pform_at =
  | P_EQ of args * args
  | P_NEQ of args * args
  | P_PPred of string * args list
  | P_SPred of string * args list 
  | P_Wand of pform * pform
  | P_Or of pform * pform
  | P_Septract of pform * pform
  | P_Garbage
  | P_False
and pform = pform_at list


let mkFalse = [P_False]

let isFalse f =
  match f with 
    [P_False] -> true 
  | _ -> false

let pconjunction (f1 : pform)  (f2 : pform) : pform = 
 if isFalse f1 then f1 else if isFalse f2 then f2 else f1 @ f2

let (&&&) = pconjunction


let pwand f1 f2 = [P_Wand(f1,f2)]

let mkNEQ(a1,a2) = [P_NEQ(a1,a2)]

let mkEQ(a1,a2) = [P_EQ(a1,a2)]

let mkPPred(n,al) = [P_PPred(n,al)]
let mkSPred(n,al) = [P_SPred(n,al)]

let mkGarbage = [P_Garbage]

let mkOr(f1,f2) = 
  if isFalse f1 then f2 
  else if isFalse f2 then f1 
  else [P_Or(f1,f2)]

let mkWand(f1,f2) = 
  if isFalse f1 then 
    mkGarbage
  else [P_Wand(f1,f2)]

let mkSeptract(f1,f2) = 
  if isFalse f1 then f1 
  else if isFalse f2 then f2 
  else [P_Septract(f1,f2)]

let mkEmpty = []


let rec subst_pform_at subs pa=
  match pa with 
   | P_EQ(a1,a2) -> mkEQ(subst_args subs  a1, subst_args subs a2)
   | P_NEQ (a1,a2) ->
       let a1,a2 = subst_args subs a1, subst_args subs a2 in 
       (*if a1=a2 then mkFalse else*) mkNEQ(a1,a2)
   | P_PPred(name,args) -> mkPPred(name,(List.map (subst_args subs) args))
   | P_SPred(name,args) -> mkSPred(name,(List.map (subst_args subs) args))
   | P_Or (f1,f2) -> mkOr(subst_pform subs f1,subst_pform subs f2)
   | P_Wand (f1,f2) -> mkWand(subst_pform subs f1,subst_pform subs f2)
   | P_Septract (f1,f2) -> mkSeptract(subst_pform subs f1,subst_pform subs f2)
   | P_Garbage -> mkGarbage 
   | P_False -> mkFalse
and subst_pform subs = 
    List.fold_left (fun pf pa -> (subst_pform_at subs pa) &&& pf) []
  



(* pretty print *)
let rec string_varlist vl = 
  match vl with  
    [] -> ""
  | v::[] -> Printf.sprintf  "%s" (string_var v)
  | v::vl -> Printf.sprintf "%s,%s" (string_var v) (string_varlist vl)

let rec list_format sep f ppf list = 
  match list with 
    [] -> Format.fprintf ppf ""
  | [x] -> Format.fprintf ppf "%a" f x 
  | x::xs -> Format.fprintf ppf "%a%s@ %a" f x sep (list_format sep f) xs 

let rec string_form_at ppf pa =  
  match pa with 
    P_NEQ(a1,a2) -> Format.fprintf ppf "%a != %a" string_args a1  string_args a2
  | P_EQ(a1,a2) -> Format.fprintf ppf "%a = %a" string_args a1  string_args a2
  | P_PPred(op,al) -> Format.fprintf ppf "%s(%a)" op string_args_list al
  | P_SPred (s,al) -> Format.fprintf ppf "%s(%a)" s string_args_list al
  | P_Or(f1,f2) -> Format.fprintf ppf "@[@[(%a)@] || @[(%a)@]@]" string_form f1 string_form f2
  | P_Wand(f1,f2) -> Format.fprintf ppf "@[@[(%a)@] -* @[(%a)@]@]" string_form f1  string_form f2
  | P_Septract(f1,f2) -> Format.fprintf ppf "@[@[(%a)@] -o @[(%a)@]@]" string_form f1  string_form f2
  | P_False -> Format.fprintf ppf "False"
  | P_Garbage -> Format.fprintf ppf "Garbage"
and string_form ppf pf = 
  Debug.list_format "*" string_form_at ppf pf 





let rec fv_form_at pa set =
  match pa with
    P_EQ(x,y) -> fv_args x (fv_args y set)
  | P_NEQ(x,y) -> fv_args x (fv_args y set)
  | P_PPred(name, argsl) -> fv_args_list argsl set
  | P_SPred(name, argsl) -> fv_args_list argsl set
  | P_Wand(f1,f2) 
  | P_Septract(f1,f2) 
  | P_Or(f1,f2) -> fv_form f1 (fv_form f2 set)
  | P_Garbage 
  | P_False -> set
and fv_form pf set =
 List.fold_left (fun set pa -> fv_form_at pa set) set pf 


let closes subs p  = 
  not (vs_exists (fun x -> not (vm_mem x subs)) (fv_form p vs_empty))


    
let rec ev_form_at pa set =
  match pa with
    P_EQ(x,y) -> ev_args x (ev_args y set)
  | P_NEQ(x,y) -> ev_args x (ev_args y set)
  | P_PPred(name, argsl) -> ev_args_list argsl set
  | P_SPred(name, argsl) -> ev_args_list argsl set
  | P_Wand(f1,f2) 
  | P_Septract(f1,f2) 
  | P_Or(f1,f2) -> ev_form f1 (ev_form f2 set)
  | P_Garbage 
  | P_False -> set
and ev_form pf set =
 List.fold_left (fun set pa -> ev_form_at pa set) set pf  

type psequent = pform * pform * pform


let fv_psequent (pff,pfl,pfr) = 
  (fv_form pff (fv_form pfl (fv_form pfr vs_empty)))

let subst_psequent subst (pff,pfl,pfr) = 
  (subst_pform subst pff, subst_pform subst pfl, subst_pform subst pfr)



let purify pal = 
  List.map (
    fun x -> 
      match x with 
	P_EQ(_,_) | P_NEQ(_,_) -> x 
      |	P_SPred(n,al) -> P_PPred(n,al)
      |	_ -> unsupported ()
	    ) pal


(***************************************************
  Logic stuff - Rules etc
 ***************************************************)


type varterm = 
    Var of varset

type where = 
  | NotInContext of varterm
  | NotInTerm of varterm * args

let string_vs ppf vs =
  vs_iter (fun v -> Format.fprintf ppf "%s" (string_var v)) vs

let string_where ppf where = 
  match where with 
    NotInContext (Var vs) -> 
      Format.fprintf ppf "%a notincontext" 
	string_vs vs
  | NotInTerm (Var vs, args) -> 
      Format.fprintf ppf "%a notin %a" 
	string_vs vs 
	string_args args



type sequent_rule = psequent * (psequent list list) * string * ((* without *) pform * pform) * (where list)

let string_pseq ppf (g,l,r) = 
  Format.fprintf ppf "%a@ | %a@ |- %a" 
    string_form g 
    string_form l
    string_form r

let string_psr ppf (sr :sequent_rule) : unit = 
    match sr with 
      (conclusion, premises, name, (without1,without2), where) ->
	Format.fprintf ppf 
	  "rule %s:@\n%a@\n%a@\nwithout@\n %a@ |- %a@\n%a"
	  name
	  string_pseq conclusion
	  (Debug.list_format_optional "if\n " " or " (Debug.list_format ";" string_pseq)) premises
	  string_form without1
	  string_form without2
	  (Debug.list_format_optional "where" ";" string_where) where
	  




type 'a rewrite_entry =  (args list * args * 'a * string * bool) list

(* substitution *)
(*IF-OCAML*)
module RewriteMap =
  Map.Make(struct
    type t = string
    let compare = compare
  end)
type 'a rewrite_map =  'a rewrite_entry RewriteMap.t 
(*ENDIF-OCAML*)

(*F#
module RewriteMap = Map
type 'a rewrite_map =  RewriteMap.t<string,'a rewrite_entry> 
F#*)

let rm_empty = RewriteMap.empty
let rm_add = RewriteMap.add
let rm_find = RewriteMap.find



type rewrite_rule = string * args list * args * (pform) * (where list) * (pform) (* if *) * string * bool

type equiv_rule = string * (pform) * (pform) * (pform) * (pform)


type rules = 
  | SeqRule of sequent_rule
  | RewriteRule of rewrite_rule
  | EquivRule of equiv_rule

type question =
  |  Implication of pform * pform 
  |  Inconsistency of pform
  |  Frame of pform * pform
  |  Equal of pform * args * args
  |  Abs of pform 


type test =
  |  TImplication of pform * pform * bool 
  |  TInconsistency of pform * bool 
  |  TFrame of pform * pform * pform 
  |  TEqual of pform * args * args * bool
  |  TAbs of pform * pform


let expand_equiv_rules rules = 
(*encode equiv rule as three rules *)
  let equiv_rule_to_seq_rule x list : rules list= 
    match x with 
      EquivRule(name, guard, leftform, rightform, without) -> 
	(SeqRule((guard, leftform, []), [[([],rightform,[])]],name ^ "_left", (without,mkEmpty) , []))
	:: 
	  (SeqRule(([],[],guard&&&leftform), [[([],[],guard&&&rightform)]], name ^"_right", (mkEmpty, without), []))
	::
	  if(guard != []) then 
	    (SeqRule((guard, [], leftform), [[([],[],rightform)]], name ^ "_split", (mkEmpty, without), []))
	    ::
	      list
	  else
	    list
    | SeqRule _ | RewriteRule _ -> x::list
  in
  List.fold_right equiv_rule_to_seq_rule rules []



(***************************************************
 from inductive
***************************************************)

type inductive_con =
{
  con_name : string;
  con_def : pform * string * args list
}

type inductive =
{
  ind_name : string;
  ind_args : args list;
  ind_cons : inductive_con list
}

type inductive_stmt = IndImport of string | IndDef of inductive

(***************************************************
 end from inductive
***************************************************)