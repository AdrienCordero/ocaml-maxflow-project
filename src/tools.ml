open Graph

let add_arc (graph : 'a graph) (src : id) (tgt : id) (lbl : int) = match find_arc graph src tgt with
  | None -> new_arc graph ({src = src; tgt = tgt; lbl = lbl})
  | Some a -> new_arc graph ({src = src; tgt = tgt; lbl = a.lbl + lbl});;
