(********************************************************
   This file is part of jStar
        src/prover/congruence.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   jStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)

open Backtrack
open Format
open Misc
open Persistentarray
open Printing
open Psyntax

(*  Implementation of paper:
    Fast congruence closure and extensions
    R. Nieuwenhuis, A. Oliveras / Information and Computation 205 (2007) 557–580

   Using persisent arrays from 
      Union Find algorithm
*)

let cc_debug = ref false


module type PCC = 
    sig
      type t
      type constant 

      type term = 
	| TConstant of constant
	| Func of constant * term list	    

      type curry_term =
	Constant of constant
      | App of curry_term * curry_term


      type pattern = 
	Hole of constant
      |	PConstant of constant
      |	PFunc of constant * pattern list

      type pattern_curry = 
	  CHole of constant
	| CPConstant of constant
	| CPApp of pattern_curry * pattern_curry 

      (* New blank ts *)
      val create : unit -> t

      (* Add a term to the structure *)
      val add_term : t -> term -> (constant * t)

      (* Add application to the structure *)
      val add_app : t -> constant -> constant -> (constant * t)

      (* Get a fresh representative *)
      val fresh : t -> (constant * t)
      (* Get a fresh representative, that can be unified *)
      val fresh_unifiable : t -> (constant * t)
      val fresh_exists : t -> (constant * t)
      val fresh_unifiable_exists : t -> (constant * t)
	  
      (* make_equal will raise a Contradiction if making the this equality invalidates a inequality *)
      val make_equal : t -> constant -> constant -> t

      val rep_eq : t -> constant -> constant -> bool
      val rep_uneq : t -> constant -> constant -> bool

      (*  Used for abstraction to deal with whether variables 
	 are still live. *)
      val rep_not_used_in : t -> constant -> constant list -> bool

      (* make_not_equal will raise a Contradiction if making this inequality invalidates an equality *)
      val make_not_equal : t -> constant -> constant -> t

      (* make_constructor will make the supplied constant treated constructorly, that is
            * different from any other constructor constant
            * When used on the left of an application the application will be considered constructor in its right argument
	 For example, if c and d are constructor and distinct then
	   rep_uneq  c d 
	 will hold.

  	 The following 
	    app(c,e) = app(d, f)
	 requires
	    c=d and f=e
	 If c and d are considerd constructor.

	 If c is constructor, then so is app(c,h) for any h.

	 So we can represent a datatype like lists as Two constructor constants
	    nil 
	    cons
	 From above, we automatically know
	    nil != app((app(cons,x),y)
	 and 
	    app((app(cons,x),y) = app((app(cons,x2),y2)  <==>  x2=x /\ y=y2
      *)
      val make_constructor : t -> constant -> t

      (* Qeury if two terms are equal. *)
(*      val eq : t -> term -> term -> bool*)

      val normalise : t -> constant -> constant 
      val others : t -> constant -> constant list 
      val eq_term : t -> curry_term -> curry_term -> bool
      val neq_term : t -> curry_term -> curry_term -> bool

      (* Pattern match, takes pattern and representative, 
	 and continuation and backtracks in continuation raises No_match  *)
      val patternmatch : t -> curry_term -> constant -> (t -> 'a) -> 'a

      val unifies : t -> curry_term -> constant -> (t -> 'a) -> 'a
      val unifies_any : t -> curry_term -> (t * constant -> 'a) -> 'a

      (* Unifies two constants, if there is only one possible way to unify them *)
      val determined_exists : t -> (constant list) -> constant -> constant -> t * ((constant * constant) list)

      (*  Make a congruence closure structure that is equivalent in content, 
	 but each constant is a representative. 
         Also returns a map for the updates to each constant*)
      val compress_full : t -> t*(constant -> constant) 

      (* {{{  Debug stuff *)
      val print : t -> unit 

      (** Like [pretty_print'], but uses * as separator (so that 
       * arguments [pp_sep] and [first] are mising). *)
      val pretty_print : 
        (constant -> bool) -> 
        (formatter -> constant -> unit) -> formatter -> t -> unit 

      (** [pretty_print' has_pp pp_c pp_sep ppf first cc] prints to [ppf]
       * the information in the congruence closure [cc]. Constants involved
       * in equalities/inequalities are printed using [pp_c], but only when
       * [has_pp] says that both constants can be handled by [pp_c], otherwise
       * that particular equality/inequality is omitted. The function [pp_sep]
       * is used to print separators. The module [Printing] explains how
       * [first] is used and what the result means. *)
      val pretty_print' : 
        (constant -> bool) -> 
        (formatter -> constant -> unit) ->
        sep_wrapper -> 
        formatter -> 
        bool -> 
        t -> 
        bool

      val pp_c : t -> (formatter -> constant -> unit) -> (formatter -> constant -> unit)  

      val get_eqs : (constant -> bool) -> (constant -> 'a) -> t -> ('a * 'a) list
      val get_neqs : (constant -> bool) -> (constant -> 'a) -> t -> ('a * 'a) list
      
      val get_consts : t -> constant list
      val get_reps : (constant -> bool) -> (constant -> 'a) -> t -> 'a list 

      (* surjective mapping from constants to integers *)
      val const_int : constant -> t -> int 

      val test : unit -> unit

      val delete : t -> constant -> t
      (* }}} *)
    end

module PersistentCC (A : GrowablePersistentArray) : PCC = 
  struct
	   
    type constant = int
	  
    type term = 
      | TConstant of constant
      | Func of constant * term list
	    
    type curry_term =
	Constant of constant
      | App of curry_term * curry_term

    type inj_term = 
	Not
      |	Self
      |	IApp of constant * constant

    type pattern = 
	Hole of constant
      |	PConstant of constant
      |	PFunc of constant * pattern list

    type pattern_curry = 
	CHole of constant
      |	CPConstant of constant
      |	CPApp of pattern_curry * pattern_curry 







    let rec curry (t : term) 
	= 
      match t with 
	TConstant c -> Constant c
      | Func (c,tl) -> List.fold_left (fun ct t -> App (ct,(curry t))) (Constant c) tl


    let rec currypattern (t : pattern)  : pattern_curry
	= 
      match t with 
	Hole c -> CHole c
      |	PConstant c -> CPConstant c
      | PFunc (c,tl) -> List.fold_left (fun ct t -> CPApp (ct,(currypattern t))) (CPConstant c) tl
	    
	    


    type complex_eq = (* a *)constant * (* b *)constant * (* c *)constant  (* app(a,b) = c *)
	  
    type flat_eq =
      |  Complex of complex_eq

    type use = 
	Complex_eq of complex_eq
      |	Not_equal of constant
	    

    module CCMap = Map.Make( 
      struct 
	type t = constant * constant
	let compare = intcmp2
      end
	)


    type var_type = 
      |	Unifiable
      |	Exists
      |	UnifiableExists
      |	Standard
      |	Deleted

(* 
   UseList  :  constant ->  use list 
   Representative : constant -> constant
   Class List : constant -> constant list
   Lookup : constant -> constant -> complex_eq option
 *)

(* Intuitively: 

  representative - mapping from constant to represntative constant. 

  classlist - mapping from represenative constants to the class of represented. 

*)
    type t = 
	{ uselist : (use list) A.t;
	  representative : constant A.t;
	  classlist : (constant list) A.t;
	  lookup : complex_eq CCMap.t;
	  rev_lookup : ((constant * constant) list) A.t;
	  not_equal : unit CCMap.t;
	  constructor : inj_term A.t;
	  unifiable : var_type A.t;
	} 


    let create () =
      {
       uselist = A.create (fun i -> []);
       representative = A.create (fun i -> i);
       classlist = A.create (fun i -> [i]);
       lookup = CCMap.empty;
       rev_lookup = A.create (fun i -> []);
       not_equal = CCMap.empty;
       constructor = A.create (fun i -> Not);
       unifiable = A.create (fun i -> Standard);
     }




    let fresh ts : int * t =
      let c = A.size ts.uselist in 
      c, 
      {ts with 
	uselist = A.grow ts.uselist 1;
	representative = A.grow ts.representative 1;
	classlist = A.grow ts.classlist 1;
        rev_lookup = A.grow ts.rev_lookup 1;
	constructor = A.grow ts.constructor 1;
	unifiable = A.grow ts.unifiable 1;
      }

    let fresh_unifiable ts : int * t =
      let c,ts = fresh ts in 
      c, {ts with unifiable = A.set ts.unifiable c Unifiable}

    let fresh_unifiable_exists ts : int * t =
      let c,ts = fresh ts in 
      c, {ts with unifiable = A.set ts.unifiable c UnifiableExists}

    let fresh_exists ts : int * t =
      let c,ts = fresh ts in 
      c, {ts with unifiable = A.set ts.unifiable c Exists}
	  
    let rep ts a = 
      A.get ts.representative a 

    let set_rep ts a r = 
      {ts with representative = A.set ts.representative a r }

    let rep_eq (ts : t) (c : constant) (c2 : constant) : bool = 
      rep ts c = rep ts c2

    let rec rep_uneq (ts : t) (c : constant) (c2 : constant) : bool = 
      let c = rep ts c in 
      let c2 = rep ts c2 in
      if c <> c2 then
	match A.get ts.constructor c, A.get ts.constructor c2 with 
	  Not, _
	| _, Not  (* At least one rep is not constructor, check if we have explicit neq.*) 
	| IApp _,IApp _  (* Don't recurse, just do simple test here, later redefine to find contradictions *)
	  ->
	  let (c,c2) = if c < c2 then (c,c2) else (c2,c) in 
	  CCMap.mem (c,c2) ts.not_equal
	| Self, _  
	| _, Self -> 
	    (* Self,Self or Self,IApp are not equal *) 
	    true
      else 
	(* Same rep, therefore not not equal. *)
	false


    let lookup ts (a,b) =
      try 
	Some (CCMap.find ((rep ts a),(rep ts b)) ts.lookup )
      with Not_found -> None



    let invariant (ts : t) : bool = if true then true else
      let n = A.size ts.representative  - 1 in 
      (* Check reps have class list *)
      for i = 0 to n do 
	let r = A.get ts.representative i in 
	let cl = A.get ts.classlist r in 
	assert (List.exists ((=) i) cl)
      done;
      (* check class list has reps *)
      for i = 0 to n do
	let cl = A.get ts.classlist i in 
	assert (List.for_all (fun c -> A.get ts.representative c = i) cl) 
      done;
      (* check lookup has appropriate rev_lookup and uses *)
      CCMap.iter
	(fun (a,b) (c,d,e) ->
	  let rl = A.get ts.rev_lookup (rep ts e) in 
	  assert (rep_eq ts a c);
	  assert (rep_eq ts b d);
	  (* Check reverse map exists *)
	  assert (List.exists (fun (r1,r2) -> rep_eq ts a r1 && rep_eq ts b r2) rl);
	  (* Check there exists a use for "a" *)
	  let ula = A.get ts.uselist (rep ts a) in 
	  assert (List.exists 
		    (function 
			Complex_eq (r1,r2,r3) -> 
			  rep_eq ts r3 e &&
			  rep_eq ts r1 a &&
			  rep_eq ts r2 b
		      |	 _ -> false) 
		     ula);
	  let ulb = A.get ts.uselist (rep ts b) in 
	  assert (List.exists 
		    (function 
			Complex_eq (r1,r2,r3) -> 
			  rep_eq ts r3 e &&
			  rep_eq ts r1 a &&
			  rep_eq ts r2 b
		      |	 _ -> false) 
		     ulb);

	  ) ts.lookup;
      (* check rev_lookup has appropriate lookup *)
      for i = 0 to n do
	let rl = A.get ts.rev_lookup i in
	assert (List.for_all 
	  (fun (r1,r2) -> 
	    match lookup ts (r1,r2) with 
	      Some (a,b,c) -> 
		rep_eq ts c i
	    | None -> assert false 
		  ) rl	    
		  )
      done;

      true
	
    let pp_c ts pp ppf i = 
       (*if true then pp ppf i else fprintf ppf "{%a}_%i" pp i i*)
      pp ppf i (*(rep ts i)*)

    let for_each_rep ts (f : constant -> unit) = 
      let n = A.size ts.representative in
      for i = 0 to n-1 do
	if A.get ts.representative i = i then 
	  f i 
      done

    let get_eqs mask map ts = 
      let acc = ref [] in 
      for_each_rep ts 
	(fun rep  -> 
          if mask rep then
	  let rp = map rep in 
	  List.iter 
	    (fun i -> if mask i && rep <> i then acc := (rp,map i)::!acc) 
	    (A.get ts.classlist rep)
	  ) ;
      !acc

    let get_neqs mask map ts =
      let r = Hashtbl.create 13 in (* to take care of duplicates *)
      CCMap.iter 
        (fun (a,b) () -> 
          let a = rep ts a in
          let b = rep ts b in
          if mask a && mask b then
          Hashtbl.add r (map (min a b), map (max a b)) ())
        ts.not_equal;
      Hashtbl.fold (fun x _ xs -> x::xs) r []

    let pretty_print' has_pp pp_term pp ppf first ts =
      let eqs = get_eqs has_pp (fun x->x) ts in
      let neqs = get_neqs (fun x -> true) (fun x->x) ts in
      let first = 
        List.fold_left (pp.separator (pp_eq pp_term) ppf) first eqs in
      List.fold_left (pp.separator (pp_neq pp_term) ppf) first neqs

    let pretty_print has_pp pp_term = 
      pp_whole (pretty_print' has_pp pp_term) pp_star

    let print (ts:t) : unit =
      let rs = ts.representative in 
      let n = A.size rs - 1 in 
      printf "Rep\n   ";
      for i = 0 to n do
	if i <> (A.get rs i) then 
	  printf "%n|->%n  " i (A.get rs i)
      done ;

(* 
      printf "\nUses";
      for i = 0 to n do
	if (A.get (ts.uselist) i) <> [] then 
	  begin 
	    printf "\n%n  |-> " i;
	    List.iter 
	      (function
		  Complex_eq (a,b,c) ->
		    printf "app(%n,%n)=%n   " a b c
		| Not_equal a ->
		    printf "%n != %n   " i a
		    )
	      (A.get (ts.uselist) i)
	  end
      done; 
*)
(*
      printf "\nClass list\n";
      for i = 0 to n do 
	if (A.get (ts.classlist) i) <> [i] then 
	  begin
	    printf "%n |->  " i;
	    List.iter
	      (fun c -> printf "%n " c)
	      (A.get (ts.classlist) i);
	    printf ";\n"
	  end
      done;
*)
      printf "\nNot equal\n";
      CCMap.iter  (fun (a,b) () -> printf "  %n!=%n;\n" a b) ts.not_equal;

      printf "\nLookup\n";
      CCMap.iter  (fun (a,b) (x,y,z) -> printf "  app(%n,%n) |-> (%n,%n,%n);\n" a b x y z) ts.lookup;

      printf "\nRev lookup";
      for i = 0 to n do
	if (A.get ts.rev_lookup i) <> [] then
	  begin
	    printf "\n %n" i;
	    List.iter
	      (fun (a,b) -> 
		printf " = app(%n,%n)" a b )
	      (A.get ts.rev_lookup i)
	  end
      done;

      printf "Injective info:\n";
      for i = 0 to n do
	match A.get ts.constructor i with
	  Not -> ()
	| Self -> printf "  inj(%i)\n" i
	| IApp(a,b) -> printf "  inj(%i) by app(%i,%i)\n" i a b 
      done;
      printf "\n\n"


    let add_lookup ts (a,b,c) =
      {ts with 
	lookup = CCMap.add ((rep ts a),(rep ts b)) (a,b,c) ts.lookup;
	rev_lookup = A.set ts.rev_lookup (rep ts c) ((a,b)::A.get ts.rev_lookup (rep ts c)) }



    let get_uselist ts r =
      A.get ts.uselist r

    let add_use ts a fe : t = 
      let a = rep ts a in 
      let oldul = A.get ts.uselist a in 
      {ts with uselist = A.set ts.uselist a (fe::oldul)}

    let clear_uselist ts r = 
	{ts with uselist = A.set ts.uselist r [] }  



    let get_cl ts r = 
      A.get ts.classlist r 

    let append_cl (ts : t) (r : constant) (cl : constant list) =
      {ts with classlist = A.set ts.classlist r ((get_cl ts r) @ cl)}

    let clear_cl ts r = 
	{ts with classlist = A.set ts.classlist r [] }  


    let make_not_equal (ts : t) (a : constant) (b : constant) : t = 
      let a,b = rep ts a, rep ts b in 
      if a=b then raise Contradiction; 
      let (a,b) = if a<b then (a,b) else (b,a) in  
      let ula = get_uselist ts a in 
      let ulb = get_uselist ts b in 
      let ula = if List.exists (fun a -> a=(Not_equal b)) ula then ula else (Not_equal b)::ula in 
      let ulb = if List.exists (fun b -> b=(Not_equal a)) ulb then ulb else (Not_equal a)::ulb in
      {ts with 
	not_equal = CCMap.add (a,b) () ts.not_equal;
	uselist = A.set (A.set ts.uselist a ula) b ulb}

	
    let rec make_use_constructor d (ts,pending) use = 
      match use with 
         (* Only deal where it is a use on the left of an application *)
      | Complex_eq (a,b,c) when (rep_eq ts a d) ->  
	  begin
	    let r =  rep ts c in
	    match A.get ts.constructor r with
          (* Can't make it an IApp, is already an constructor *)
	      Self -> raise Contradiction
	  (* Can make it an constructor *)
	    | Not -> make_uses_constructor r ({ts with constructor = A.set ts.constructor r (IApp(a,b))},pending)
	  (* Already constructor, okay assuming we can make subterms equal *)
	    | IApp(r1,r2) -> ts, (a,r1)::(b,r2)::pending
	  end
      | _ ->       
	  (ts,pending) 
    and make_uses_constructor d (ts,pending) = 
      let ul = get_uselist ts d in 
      match ul with 
	[] -> ts,pending
      | _ -> List.fold_left (make_use_constructor d) (ts,pending) ul

    (* merges the constructor details, 
       and potentially returns a list of equalities to make,
       b should be the new representive that is the target of the merge.
    *) 
    let constructor_merge ts a b pending : t * ((constant * constant) list) =
    (* Should only call this with something that is an App *)
      match A.get ts.constructor a , A.get ts.constructor b with
	Not, Not -> ts, pending 
      |	Not, i -> make_uses_constructor a (ts,pending)
      |	i, Not -> 
	  let (ts,pending) =  make_uses_constructor b (ts,pending) in
	  {ts with constructor = A.set ts.constructor b i}, pending     		
      |	IApp(a,b), IApp(c,d) ->
	  ts, (a,c)::(b,d)::pending 
      |	_,_ -> 
	  (* Other options are a contradiction 
	     and should have already been removed *)
	  assert false

    let no_live ts nr = 
	List.for_all
	  (fun x -> match (A.get ts.unifiable x) with Standard -> false | _ -> true)
	  (A.get ts.classlist (rep ts nr)) 


    let unifiable_merge ts a b : t =
      let vt =
        match A.get ts.unifiable a , A.get ts.unifiable b with
(*  
	| _, Standard -> Standard
	| Standard, _ -> Deleted
	| _, Deleted  
	| Deleted, _ -> Deleted
	| Unifiable, Unifiable -> Unifiable
	| Unifiable, UnifiableExists 
	| UnifiableExists, Unifiable 
	| UnifiableExists, UnifiableExists -> UnifiableExists
	| Exists, UnifiableExists 
	| UnifiableExists, Exists 
	| Exists, Exists 
	| Exists, Unifiable 
	| Unifiable, Exists -> Exists
*)
        | Unifiable, Unifiable -> Unifiable
        | Unifiable, UnifiableExists
        | UnifiableExists, Unifiable
        | UnifiableExists, UnifiableExists -> UnifiableExists
        | _, Unifiable
        | _, UnifiableExists -> Deleted
        | _, a -> a
      in
      {ts with unifiable = A.set ts.unifiable b vt}

    let rec propogate (ts : t) (pending : (constant * constant) list) : t = 
      match pending with 
	  [] -> ts
	| (a,b)::pending -> 
	    if !cc_debug then 
	      begin 
		printf "Making %i=%i " a b;
		if pending <> [] then 
		  begin
		    printf "with pending ";
		    List.iter (fun (a,b) -> printf "(%i,%i) " a b) pending;
		  end;
		printf " in \n";
		print ts;
	      end;
	    begin 
	      if rep_uneq ts a b then 
		raise Contradiction
	      else if rep_eq ts a b then 
		propogate ts pending 
	      else
		let old_repa = rep ts a in 
		let repb = rep ts b in 
		let rl = (A.get ts.rev_lookup old_repa) @ (A.get ts.rev_lookup repb) in 
		let ts = {ts with rev_lookup = A.set ts.rev_lookup repb rl} in 
		let ts,pending = constructor_merge ts old_repa repb pending in 
		let cl = get_cl ts old_repa in
		let ts = append_cl ts repb cl in
		let ts = List.fold_left (fun ts c -> set_rep ts c repb)  ts cl in 
		let ts = clear_cl ts old_repa in (* is this necessary? *)
		let ts = unifiable_merge ts old_repa repb in 
		let ul = get_uselist ts old_repa in 
		let (pending,ts) = List.fold_left 
		    (fun (pending,ts) -> 
		      function 
			  Complex_eq (c1,c2,c) -> 
			    begin 
			      match lookup ts (c1,c2) with 
				None -> 
				  let ts = add_lookup ts (c1,c2,c) in
				  (pending, add_use ts repb (Complex_eq (c1,c2,c)))
			      | Some (d1,d2,d) ->
				  ((c,d)::pending, ts)
			    end
			| Not_equal c1 ->
			    let ts = make_not_equal ts repb c1 in
			    (pending, ts)
		      )
		    (pending,ts) 		    
		    ul in 
		let ts = clear_uselist ts old_repa in 
		propogate ts pending
	    end
 


    let rec rep_not_used_in (ts : t) ( a : constant) (b : constant list) (visited : constant list) : bool = 
(*      printf "Looking for %i in @\n" a;
      print ts;
      printf "Entry points:@ ";
      List.iter (printf "%i @ ") b;
      printf "@\n";*)
      if List.mem a visited then 
	begin 
	  printf "Cycle in ts@\n";
	  print ts; 
	  printf "Visited@\n @[";
	  List.iter (printf "%i@ ") visited;
	  printf "@]@\n";
          true
	end 
      else if List.mem a b then 
	begin 
(*	  printf "Foo";*)
	  false
	end
      else 
	List.for_all 
	  (function 
	    | Not_equal _ -> true 
	    | Complex_eq (c1,c2,c) -> 
		rep_not_used_in ts (rep ts c) b (a::visited)
	) (get_uselist ts a)

    let rep_not_used_in (ts : t) ( a : constant) (b : constant list) : bool = 
	  rep_not_used_in ts (rep ts a) (List.map (rep ts) b) []
 
    let make_equal (ts : t) (a : constant)  (b : constant) : t = 
      assert (invariant ts);
      let ts = propogate ts [(a,b)] in 
      assert (invariant ts);
      ts

    let make_constructor (ts : t) (a : constant) : t = 
      (*assert (A.get ts.constructor (rep ts a) = Not);*)  (* FIXME: is this needed? *)
      let ts = {ts with constructor = A.set ts.constructor (rep ts a) Self} in
      let ts,p = make_uses_constructor a (ts,[]) in 
      propogate ts p 
      
    let merge (ts : t) (fe : flat_eq) : t = 
      match fe with 
      | Complex (a,b,c) -> 
	  begin
	    match lookup ts (a,b) with 
	      None -> 
		let ts = add_lookup ts (a,b,c) in
		let ts = add_use ts a (Complex_eq (a,b,c)) in 
		let ts = add_use ts b (Complex_eq (a,b,c)) in 
		(* If a is constructor, then so should c be. *)
		if A.get ts.constructor (rep ts a) <> Not then 
		  let ts,pending = make_use_constructor a (ts,[]) (Complex_eq (a,b,c)) in 
		  propogate ts pending
		else
		  ts
	    | Some (e,f,g) -> propogate ts [(c,g)]
	  end


    let rec normalize (ts  : t) (term1 : curry_term) : curry_term = 
      match term1 with 
	Constant c -> Constant (rep ts c)
      |	App (terml,termr) ->
	  let terml = normalize ts terml in 
	  let termr = normalize ts termr in 
	  match terml,termr with 
	    Constant c1, Constant c2 ->
	      begin
		match lookup ts (c1,c2) with 
		  None -> App (terml,termr)
		| Some (e,f,g) -> Constant (rep ts g)
	      end 
	  | _ ->  App(terml,termr)


    let add_app (ts : t) (c1 : constant ) (c2 : constant) : constant * t = 
      match lookup ts (c1,c2) with 
	None -> 
	  let c, ts = fresh ts in 
	  let ts = merge ts (Complex (c1,c2,c)) in 
	  c, ts
      | Some (e,f,g) -> rep ts g, ts


    let rec add_curry_term (ts  : t) (term1 : curry_term) : constant * t  = 
      match term1 with 
	Constant c -> rep ts c, ts
      |	App (terml,termr) ->
	  let c1,ts = add_curry_term ts terml in 
	  let c2,ts = add_curry_term ts termr in 
	  add_app ts c1 c2

    let add_term (ts:t) (term : term) : constant * t = 
      let c,ts = add_curry_term ts (curry term) in 
      assert (invariant ts);
      c,ts


    let compress ts cs : (t * (constant -> constant)) =
      let n = A.size ts.uselist in
	   (* The set of currently visible constants *)
      let set = Array.init n (fun _ -> false) in 
	   (* The mapping from old constant to new *)
      let map = Array.init n (fun _ -> 0) in 
	   (* The inverse mapping from new constant to old *)
      let inv = Array.init n (fun _ -> 0) in 
	   (* index of next constant to map to *)
      let i = ref 0 in 
	   (* creates mapping for the next constant *)
      let add_map x =  
	let r = rep ts x in 
	if Array.get set r then () else
	begin  
          Array.set set r true ;
	  Array.set map r !i; 
	  Array.set inv !i r;
	  i := !i + 1;
	end in 
	   (* Check if considered live *)
      let live r = Array.get set (rep ts r) in 
	   (* Get new representative for constant *)
      let newrep r = Array.get map (rep ts r) in 
	   (* Add mapping for all the externally live constants *)
      List.iter add_map cs;
      let j = ref 0 in 
	   (* For each live constant add sub terms that are live *)
	   (*  Using while rule as !i could be increased 
	      by body of loop *)
      while (!j < !i) do 
	let ul =  A.get ts.uselist (Array.get inv !j) in 
	List.iter 
	  (function 
	      Complex_eq (e,f,g) -> 
		if live e && live f then add_map g
	    | Not_equal _ -> ())
	  ul;
	j := !j + 1
      done;
	   (* Now we should have a mapping for all live constants,
	      build compressed version. *) 
      let look = ref CCMap.empty in 
      let neq = ref CCMap.empty in 
      let reps = Array.init !i (fun i -> i) in 
      let clas = Array.init !i (fun i -> [i]) in 
      let constructor = Array.init !i 
	  (fun i -> 
	    match A.get ts.constructor (Array.get inv i) with
	      Not -> Not
	    | Self -> Self
	    | IApp (a,b) -> IApp (newrep a, newrep b)
	      ) in 
      let unifiable = Array.init !i 
	  (fun i -> (A.get ts.unifiable (Array.get inv i))) in
(* Build new reverse lookup map *)
      let revl = Array.init !i 
	  (fun i -> 
	    remove_duplicates intcmp2 
	      (map_option 
		 (fun (a,b) -> 
		   if live  a && live b then 
		     Some (newrep a, newrep b)
		   else
		     None
		       ) (A.get ts.rev_lookup (Array.get inv i)))) in 
(* Create new uselist *)
      let usel = Array.init !i 
	  (fun i -> 
	    let oi = Array.get inv i in 
	    let ul = map_option
		(function 
		    Complex_eq(e,f,g) -> 
		      if live e && live f then
			begin
			  if newrep e = i then 
			    look := CCMap.add (newrep e, newrep f) (newrep e, newrep f, newrep g) !look;
			  Some (Complex_eq(newrep e,newrep f, newrep g))
			end
		      else
			None
		  | Not_equal(a) -> 
		      if live a then 
			begin
			  if newrep a < i then 
			    neq := CCMap.add (a,i) () !neq;
			  Some (Not_equal(newrep a))
			end
		      else None
			  )
		(get_uselist ts oi) in 
	    remove_duplicates (intcmp) ul
	      ) in 
      let ts,map = {
	uselist = A.unsafe_create usel (fun i -> []);
	representative = A.unsafe_create reps (fun i -> i);
	classlist = A.unsafe_create clas (fun i -> [i]);
	lookup = !look;
	rev_lookup = A.unsafe_create revl (fun i -> []);
	not_equal = !neq;
	constructor = A.unsafe_create constructor (fun i -> Not);
	unifiable = A.unsafe_create unifiable (fun i -> Standard);
      },  (fun c -> Array.get map c) 
      in
      assert (invariant ts);
      ts,map

    let compress_full ts = 
      let cs = ref [] in 
      for_each_rep ts 
	(fun i -> cs := i::!cs);
      compress ts !cs

    let test debug =
      let print_constant ts c =
	printf "Constant %n has rep %n\n" c (rep ts c) in
      let nth r1 r2 n = 
	let rec f n = 
	  match n with 
	    0 -> TConstant r2
	  | n -> Func (r1, [f (n-1)])  in
	f n in
      let ts = create () in 
      let r1,ts = fresh ts in
      let r2,ts = fresh ts in    
      let t0 = nth r1 r2 0 in 
      let t1 = nth r1 r2 1 in 
      let t2 = nth r1 r2 2 in 
      let t3 = nth r1 r2 3 in 
      let t4 = nth r1 r2 4 in 
      let c1,ts = add_term ts t1 in 
      let c0,ts = add_term ts t0 in 
      let c2,ts = add_term ts t2 in 
      let c3,ts = add_term ts t3 in 
      let c4,ts = add_term ts t4 in
      let _ = 
	let ts = make_equal ts c0 c2 in 
	let ts = make_equal ts c1 c4 in
	if rep_eq ts c1 c2 && rep_eq ts c0 c1 && rep_eq ts c1 c2 && rep_eq ts c2 c3 && rep_eq ts c3 c4
	then printf "Correct Test 1.\n" 
	else 
	  begin 
	    printf "Test 1 fails!";
	    print_constant ts c1;
	    print_constant ts c2;
	    print_constant ts c3;
	    print_constant ts c4;
	    print ts 
	  end in 
      let _ = 
	 if rep_eq ts c1 c2 || rep_eq ts c0 c1 || rep_eq ts c1 c2 || rep_eq ts c2 c3 || rep_eq ts c3 c4
	 then 
	   begin 
	     printf "Test 2 fails!";
	     print_constant ts c1;
	     print_constant ts c2;
	     print_constant ts c3;
	     print_constant ts c4;
	     print ts 
	   end
	 else 
	   printf "Correct Test 2.\n" in
       let _ = 
	 let ts = make_equal ts c0 c1 in 
	 if rep_eq ts c1 c2 && rep_eq ts c0 c1 && rep_eq ts c1 c2 && rep_eq ts c2 c3 && rep_eq ts c3 c4
	 then printf "Correct Test 3.\n" 
	 else 
	   begin 
	     printf "Test 3 fails!";
	     print_constant ts c1;
	     print_constant ts c2;
	     print_constant ts c3;
	     print_constant ts c4;
	     print ts 
	   end;
       in 
       let _ =
	 let ts = make_equal ts c0 c2 in 
	 if rep_eq ts c1 c3 && rep_eq ts c2 c4 && 
	   (not (rep_eq ts c1 c2)) && (not (rep_eq ts c2 c3)) && (not (rep_eq ts c3 c4))
	 then printf "Correct Test 4. \n" 
	 else 
	   begin 
	     printf "Test 4 fails!";
	     print_constant ts c1;
	     print_constant ts c2;
	     print_constant ts c3;
	     print_constant ts c4;
	     print ts 
	   end in 
       let _ =
	 try 
	   let ts = make_not_equal ts c0 c2 in 
	   let ts = make_equal ts c0 c1 in 
	   begin 
	     printf "Test 5 fails!";
	     print_constant ts c1;
	     print_constant ts c2;
	     print_constant ts c3;
	     print_constant ts c4;
	     print ts 
	   end
	 with Contradiction -> 
	   printf "Correct Test 5. \n" 
       in
       let _ =
	 try 
	   let ts = make_not_equal ts c0 c1 in 
	   let _ = make_equal ts c0 c2 in 
	   printf "Correct Test 6. \n" 
	 with Contradiction -> 
	   begin 
	     printf "Test 6 fails!";
	   end
       in
(* This test is hard to automatically check.  Approximate by checking a size *) 
       let _ =
	 try 
	   let ts = make_equal ts c0 c2 in 
	   let ts2,map = compress ts [r1;r2] in 
	   if A.size ts2.representative = 3 
	   then printf "Correct Test 7 a\n" 
	   else 
	     begin
	       printf "Failed Test 7 a\n";
	       print ts2
	     end;
	   let ts2 = make_not_equal ts c0 c1 in 
	   let ts2,map = compress ts2 [r1;r2] in 
	   if A.size ts2.representative = 3 
	   then printf "Correct Test 7 b\n" 
	   else 
	     begin
	       printf "Failed Test 7 b\n";
	       print ts2
	     end;
	   let ts2 = make_equal ts c0 c1 in 
	   let ts2,map = compress ts2 [r1;r2] in 
	   if A.size ts2.representative = 2 
	   then printf "Correct Test 7 c\n" 
	   else 
	     begin
	       printf "Failed Test 7 c\n";
	       print ts2
	     end
	 with Contradiction -> 
	   begin 
	     printf "Test 7 fails!\n";
	   end
       in
       let _ = 
	 let cons,ts = fresh (create ()) in
	 let nil,ts = fresh ts in    
	 let x1,ts = fresh ts in
	 let x2,ts = fresh ts in    
	 let y1,ts = fresh ts in
	 let y2,ts = fresh ts in    
	 let ts = make_constructor ts cons in
	 let ts = make_constructor ts nil in
	 let tnil,ts = add_term ts (Func(nil,[])) in 
	 let tcons1, ts = add_term ts (Func(nil, [TConstant x1;TConstant x2])) in 
	 let tcons2, ts = add_term ts (Func(nil, [TConstant y1;TConstant y2])) in 
	 let tcons3, ts = add_term ts (Func(nil, [TConstant y1;TConstant x2])) in 
	 begin 
	   (* Test 8 *)
	   if rep_uneq ts tcons1 tnil then 
	     printf "Test 8 Passed!\n" 
	   else
	     begin 
	       printf "Test 8 Failed!\n";
	       print ts;
	     end
	       ;
	   (* Test 9 *)
	   let ts2 = make_equal ts tcons1 tcons2 in
	   if rep_eq ts2 x1 y1 then 
	     printf "Test 9a Passed!\n"
	   else 
	     printf "Test 9a Failed!\n";
	   if rep_eq ts2 x2 y2 then 
	     printf "Test 9b Passed!\n"
	   else 
	     begin 
	       printf "Test 9b Failed!\n Assuming %i=%i, could not prove %i=%i\n" tcons1 tcons2 x2 y2;
	       print ts2;
	       let ts,m= compress_full ts2 in 
	       print ts
	     end;
	   if rep_eq ts2 x1 x2 then 
	     printf "Test 9c Failed!\n"
	   else 
	     printf "Test 9c Passed!\n";
	   if rep_eq ts2 y1 y2 then 
	     printf "Test 9d Failed!\n"
	   else 
	     printf "Test 9d Passed!\n"
	       ;
	   let ts3 = make_equal ts tcons3 tcons1 in 
	   let ts3 = make_equal ts3 tcons2 tcons1 in 
	   if rep_eq ts3 y1 x1 then
	     printf "Test 10 Passed!\n"
	   else 
	     printf "Test 10 Failed!\n"
	 end
       in
       ()

(* Can probably remove pattern match by using unifiable variables in terms.*)
    let rec patternmatch_inner pattern con ts (cont : t -> 'a) : 'a = 
      match pattern with 
	CHole c -> cont (try make_equal ts c con with Contradiction -> raise No_match)
      |	CPConstant c -> if rep_eq ts c con then cont ts else raise No_match
      |	CPApp (p1,p2) ->  
	  let cl = A.get ts.rev_lookup (rep ts con) in 
	  using 
	    ts cont 
	    (tryall 
	       (fun (c1,c2) 
		 -> 
		   andthen 
		     (patternmatch_inner p1 c1) 
		     (patternmatch_inner p2 c2)
		     )
	       cl)

    let rec patternmatch_inner pattern con ts (cont : t -> 'a) : 'a = 
      match pattern with 
      |	Constant c -> 
	  if rep_eq ts c con then 
	    cont ts 
	  else 
	    begin
	      match A.get ts.unifiable (rep ts c), A.get ts.unifiable (rep ts con) with
	      Unifiable, _ 
	    | UnifiableExists, Exists ->
		cont (try make_equal ts c con with Contradiction -> raise No_match)
	    | UnifiableExists, _ ->
	        if no_live ts (rep ts con) then 
		    (* If not live accesses treat as an exists *)
		    cont (try make_equal ts c con with Contradiction -> raise No_match)
		else
                  (* Needs to be an exists, so fail*)
		  raise No_match  
	    | Exists,_ 
	    | Standard, _ -> raise No_match
	    | Deleted, _ -> raise No_match
	    end
      |	App (p1,p2) ->  
	  let cl = A.get ts.rev_lookup (rep ts con) in 
	  using 
	    ts cont 
	    (tryall 
	       (fun (c1,c2) 
		 -> 
		   andthen 
		     (patternmatch_inner p1 c1) 
		     (patternmatch_inner p2 c2)
		     )
	       cl)
    let patternmatch ts pattern constant (cont : t -> 'a) : 'a = 
      patternmatch_inner pattern constant ts cont

    let unifies = patternmatch 


    let unifies_any ts c1 cont = 
      (* Very naive code, should do something clever like e-matching, but will do for now. *)
      let rec f n = 
	if n = A.size ts.uselist then
	  raise No_match
	else
	    if n <> rep ts n then f (n+1)
	    else
	      try 
		unifies ts c1 n (fun ts -> cont (ts,n)) 
	      with No_match 
		  -> f (n+1)
      in f 0


    let rec determined_exists ts cl c1 c2 : t * ((constant * constant) list) =
      if rep_eq ts c1 c2 then 
      (* They are equal *)
	ts, []
      else if rep_uneq ts c1 c2 then 
	raise Contradiction
      else if no_live ts c1 && rep_not_used_in ts c1 cl then 
	begin 
          (* They can be made equal *)
	  (* TODO Add occurs check *)
	  (make_equal ts c1 c2), []
	end
      else if no_live ts c2 && rep_not_used_in ts c2 cl then
	begin 
          (* They can be made equal *)
	  (* TODO Add occurs check *)
	  (make_equal ts c2 c1), []
	end
      else
	match A.get ts.constructor (rep ts c1), A.get ts.constructor (rep ts c2) with 
	  IApp(a,b), IApp(c,d) -> 
	    begin
	      let ts, cp1 = determined_exists ts cl a c in
	      let ts, cp2 = determined_exists ts cl b d in
	      ts,(cp1 @ cp2)
	    end
	| _ -> ts, [c1,c2]



    let normalise ts c = 
      rep ts c
    let others ts c = 
      A.get ts.classlist (rep ts c) 

   let rec inter_list (i : int) (j : int) : int list =  if i > j then [] else (i :: inter_list (i+1) j) 

    let get_consts ts = inter_list 0 (A.size ts.representative - 1)

    let get_reps mask map ts = 
      List.map map 
         (List.filter (fun i -> A.get ts.representative i = i && mask i) 
                      (inter_list 0 (A.size ts.representative - 1)))


    let rep_uneq ts c d = 
      try 
	ignore (make_equal ts c d); false
      with Contradiction -> true
      

    let const_int const ts = const


    let eq_term (ts : t) (term1 : curry_term) (term2 : curry_term) : bool = 
      normalize ts term1 = normalize ts term2 
    let neq_term (ts : t) (term1 : curry_term) (term2 : curry_term) : bool = 
      match normalize ts term1, normalize ts term2 with
	Constant c1, Constant c2 -> rep_uneq ts c1 c2 
      |	t1,t2 -> 
	  let c1,ts = add_curry_term ts t1 in 
	  let c2,ts = add_curry_term ts t2 in 
	  rep_uneq ts c1 c2

    let delete ts r = 
      let current_sort = A.get ts.unifiable r in 
      match current_sort with 
	Unifiable 
      |	UnifiableExists 
      |	Exists -> 
	  {ts with unifiable = A.set ts.unifiable r Deleted}	  
      | Deleted -> 
          (* Double dispose *)
	  ts (*assert false*)
      |	Standard ->
	  {ts with unifiable = A.set ts.unifiable r Deleted}
   
  end

module CC : PCC = PersistentCC( GrowablePersistentImpl)

(*let _ = CC.test ()*)
