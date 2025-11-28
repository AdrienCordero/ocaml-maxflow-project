open Graph
open Tools

let init (g : int graph) : (int * int) graph = gmap g (fun x -> (0, x));;

let rec find_augmenting_path (g : int graph) (src : id) (tgt : id) : int list =
  let out_arcs_src = out_arcs g src in
  augmenting_path_with_list g tgt out_arcs_src
and augmenting_path_with_list g tgt = function
    | [] -> []
    | t::_ when t.tgt = tgt -> [t.src; tgt] (* le chemin est il augmentant ?*)
    | t::q ->begin match find_augmenting_path g t.tgt tgt with
      | [] -> augmenting_path_with_list g tgt q
      | l -> t.src::l
      end;;

let ford_fulkerson (g : int graph) (src : id) (tgt : id) : (int * int) graph =
  src + tgt |> ignore;
  let i = init g in
  i;;