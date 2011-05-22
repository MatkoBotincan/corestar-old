(** Entry point for experiments on abduction with procedure calls. *)

open Format
module C = Core
module G = Cfg

let map_proc_body f x = { x with C.proc_body = f x.C.proc_body }

let parse fn =
  System.parse_file Parser.symb_question_file Lexer.token fn "core"

let mk_intermediate_cfg cs = 
  let g = G.CfgH.create () in
  let labels = Hashtbl.create 13 in
  let gotos = HashSet.create 13 in
  let miv piv s = (* make intermediate vertex *)
    let civ = G.CfgH.V.create s in
    G.CfgH.add_vertex g civ;
    (match piv with
      | Some piv -> G.CfgH.add_edge g piv civ
      | None -> ());
    match s with
      | C.Goto_stmt_core ls -> HashSet.add gotos (civ, ls); None
      | C.Label_stmt_core l -> Hashtbl.add labels l civ; Some civ
      | _ -> Some civ in
  ignore (List.fold_left miv None cs);
  let ages (v, ls) = (* add goto edges *)
    List.iter (fun l -> G.CfgH.add_edge g v (Hashtbl.find labels l)) ls in
  HashSet.iter ages gotos;
  g

let simplify_cfg g =
  let sg = G.Cfg.create () in
  let node_stack = Stack.create () in
  Stack.push None node_stack;
  let push_rep rv =
    G.Cfg.add_vertex sg rv;
    match Stack.top node_stack with
	Some v -> G.Cfg.add_edge sg v rv
      | None -> ();
    Stack.push (Some rv) node_stack in
  let pop_rep () = ignore (Stack.pop node_stack) in
  let pre v = match G.CfgH.V.label v with
      C.Assignment_core call -> 
	push_rep (G.Cfg.V.create (G.Assign_cfg call))
    | C.Call_core (fname, call) ->
	push_rep (G.Cfg.V.create (G.Call_cfg (fname, call))) 
    | _ -> () in
  let post v = match G.CfgH.V.label v with
      C.Assignment_core _
    | C.Call_core _ ->
	pop_rep ()
    | _ -> () in
  G.Dfs.iter ~pre:pre ~post:post g;
  sg

let mk_cfg cs = 
  let g = mk_intermediate_cfg cs in
  simplify_cfg g

let interpret gs =
  let f { C.proc_name=n; C.proc_spec=_; C.proc_body=_ } =
    eprintf "@[int %s@." n in
  List.iter f gs
(*   failwith "todo" *)

let output_Cfg { C.proc_name=n; C.proc_spec=_; C.proc_body=g } =
  G.output_Cfg (n ^ "_Cfg.dot") g

let output_CfgH { C.proc_name=n; C.proc_spec=_; C.proc_body=g } =
  G.output_CfgH (n ^ "_CfgH.dot") g

let main f =
  let ps = parse f in
  let igs = List.map (map_proc_body mk_intermediate_cfg) ps in
  List.iter output_CfgH igs;
  let gs = List.map (map_proc_body mk_cfg) ps in
  List.iter output_Cfg gs;
  interpret gs

let _ =
  Arg.parse [] main "alt_abd <file>";
