open Graph
open Tools

let init (g : int graph) (src : id) (tgt : id) : (int * int) graph = gmap g (fun x -> (0, x));;

let find_augmenting_path (g : int graph) (src : id) (tgt : id) : int list = assert false;;

let ford_fulkerson (g : int graph) (src : id) (tgt : id) : (int * int) graph =
  let i = init g src tgt in
  i;;