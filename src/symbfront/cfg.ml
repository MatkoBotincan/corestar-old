module C = Core

type cfg_vertex =
    Assign_cfg of C.call_core
  | Call_cfg of string * C.call_core

module Cfg = Graph.Imperative.Digraph.Abstract 
  (struct type t = cfg_vertex end)
module CfgH = Graph.Imperative.Digraph.Abstract 
  (struct type t = C.core_statement end)
module Dfs = Graph.Traverse.Dfs(CfgH)

module Display_Cfg = struct
  let vertex_name v = match Cfg.V.label v with
      Assign_cfg _ -> "Assign"
    | Call_cfg (fname, _) -> "Call " ^ fname
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
  include Cfg
end
module Dot_Cfg = Graph.Graphviz.Dot(Display_Cfg)

module Display_CfgH = struct
  let vertex_name v = match CfgH.V.label v with
      C.Nop_stmt_core -> "NOP"
    | C.Label_stmt_core s -> "Label " ^ s
    | C.Assignment_core _ -> "Assign"
    | C.Call_core (fname, _) -> "Call " ^ fname
    | C.Goto_stmt_core ss -> "Goto: " ^ (String.concat ", " ss)
    | C.End -> "End"
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes v = match CfgH.V.label v with
      C.Assignment_core _
    | C.Call_core _ -> [`Style `Bold]
    | _ -> []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
  include CfgH
end
module Dot_CfgH = Graph.Graphviz.Dot(Display_CfgH)
  
let output_Cfg g = Dot_Cfg.output_graph stdout g

let output_CfgH g = Dot_CfgH.output_graph stdout g
