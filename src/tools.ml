open Graph

let rec clone_nodes _gr = 
  match _gr with 
  |[] -> []
  |(id, graphs):: rest -> (id,[]) :: clone_nodes (rest)
;;


let gmap _gr _f = 
  match _gr with 
  |(id, arcs) -> (id, List.map _f arcs)
;;
