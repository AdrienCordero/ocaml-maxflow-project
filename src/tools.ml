open Graph

let add_arc (graph : 'a graph) (src : id) (tgt : id) (lbl : int) = match find_arc graph src tgt with
  | None -> new_arc graph ({src = src; tgt = tgt; lbl = lbl})
  | Some a -> new_arc graph ({src = src; tgt = tgt; lbl = a.lbl + lbl});;

let clone_nodes (gr: 'a graph) = Graph.n_fold gr (fun gr2 id -> new_node gr2 id) empty_graph


let rec gmap gr f = 
  (* Find the out_arcs of a node. *)
  let gr2 = Graph.n_fold gr (fun acc id -> new_node acc id) empty_graph in

  Graph.e_fold gr (fun acc arc -> let arc2 ={src = arc.src; tgt = arc.tgt; lbl = f arc.lbl} in new_arc acc arc2) gr2
  (*Graph.e_fold gr f empty_graph*)
;;
