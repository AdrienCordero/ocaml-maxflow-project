open Graph

let rec find_augmenting_path (g : int graph) (src : id) (tgt : id) : int list =
  let out_arcs_src = out_arcs g src in
  augmenting_path_with_list g tgt out_arcs_src
and augmenting_path_with_list g tgt = function
    | [] -> []
    | t::_ when t.tgt = tgt && t.lbl > 0 -> [t.src; tgt]
    | t::q when t.lbl > 0->begin match find_augmenting_path g t.tgt tgt with
      | [] -> augmenting_path_with_list g tgt q
      | l -> t.src::l
      end
    | _::q -> augmenting_path_with_list g tgt q;;

let ford_fulkerson (_g : int graph) (_src : id) (_tgt : id) : (int * int) graph = assert false;;