open Graph
open Tools

let init (g : int graph) : (int * int) graph = gmap g (fun x -> (0, x));;

(*let find_augmenting_path (g : int graph) : int list = assert false;;*)

let ford_fulkerson (g : int graph) (src : id) (tgt : id) : (int * int) graph =
  src + tgt |> ignore;
  let i = init g in
  i;;