(**
 * A module to colour graphs, intended use is allocating registers smartly
 * to optimize compiled programs.
 *)

module G = Graph

let graph_coloring g = 
  G.dump g; g