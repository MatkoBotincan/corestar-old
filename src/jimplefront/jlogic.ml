(********************************************************
   This file is part of jStar
        src/jimplefront/jlogic.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   jStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


open Psyntax
open Pprinter
open Support_syntax
open Jimple_global_types

let class2args cl = Arg_string (class_name2str cl)

(* create n.si|->e *)
let mk_pointsto n si e = [P_SPred("field",[n; si; e])]

(* create cl1 <: cl2 *)
let mk_subtype1 a1 a2 = [P_PPred("subtype", a1 :: a2 :: [])]
let mk_subtype cl1 cl2 = mk_subtype1 (class2args cl1) (class2args cl2)


let base_type2args ty = 
  Arg_string (Pprinter.j_base_type2str ty)

(* create var : cl   (precise type not static type) *) 
let objtype_name = "type"
let mk_type1 a1 a2 = [P_PPred(objtype_name, a1::a2::[])]
let mk_type var cl = mk_type1 var (class2args cl)

let mk_type_all var cl = [P_PPred(objtype_name, var::(base_type2args cl)::[])]

let objtype receiver classname = [P_PPred(objtype_name, [(Arg_var receiver);(Arg_string classname)])]



(* create var <: cl  (is a subtype of) *)
let mk_objsubtyp1 a1 a2 = P_PPred("objsubtype", a1 :: a2 :: [])
let mk_objsubtyp var cl = mk_objsubtyp1 var (class2args cl)

(* create var </: cl  (is not a subtype of) *)
let mk_objnotsubtyp var cl = P_PPred("notobjsubtype", var :: (class2args cl) :: [])

(* create var statictype cl *)
let mk_statictyp1 a1 a2 = P_PPred("statictype", a1 :: a2 :: [])
let mk_statictyp var cl = mk_statictyp1 var (class2args cl)
