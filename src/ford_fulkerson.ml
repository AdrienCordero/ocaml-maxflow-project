open Graph
open Tools

let rec algo_profondeur (g:int graph) (sommets_list: int list) (src : int) (tgt : int) : (int list) =
  let arcs = out_arcs g src in 
  if (List.exists (fun a -> a.tgt =tgt) arcs) then begin (* Si la tgt est dans les arcs *)
    let a = List.find (fun a -> a.tgt = tgt) arcs in
    if (a.lbl = 0) then
      []
    else 
    [src;tgt] 
    end 
  else 
    let rec loop arcs = 
      match arcs with 
        |[] -> [] (* aucune piste trouvée *) 
        |x::rest -> 
          if ((List.mem x.tgt sommets_list) || (x.lbl =0)) then
            loop rest (* déjà visité *)
          else
            let chemin_suivant = algo_profondeur g (x.tgt :: sommets_list) x.tgt tgt in
            match chemin_suivant with 
              |[] -> loop rest (* chemin pas trouvé *)
              |_ -> src::chemin_suivant (* chemin trouvé *) 
    in
    loop arcs
;;

exception CasQuiExistePas;;

let rec find_value (g :int graph) (chemin :int list) (m : int) : (int) =
  match chemin with 
  | [] | _::[] -> m
  | t::y::rest -> begin match Graph.find_arc g t y with
      |None -> raise CasQuiExistePas
      |Some x -> 
          find_value g (y::rest) (min m x.lbl)
    end
;;

let rec augmenter_chemin (g:int graph) (chemin: int list) (min: int) : (int graph) =
match chemin with 
| [] | _::[] -> g
|x::y::rest -> begin match Graph.find_arc g x y with
    |None -> raise CasQuiExistePas
    |Some a -> augmenter_chemin (add_arc (add_arc g a.tgt a.src min) a.src a.tgt (-min)) (y::rest) min (* augmenter_chemin (add_arc g a.src a.tgt (-min)) (y::rest) min *)
    end
;;

let arc_int_to_int_int (a : int arc) (lbl : int) : (int * int) arc = {src = a.src; tgt = a.tgt; lbl = (lbl + 2, a.lbl)};;

let arc_int_to_int_int_v2 (a : int arc) : (int * int) arc = {src = a.src; tgt = a.tgt; lbl = (0, a.lbl)};;
let arc_int_to_int_int_v3 (a : int arc) (lbl : int) : (int * int) arc = {src = a.src; tgt = a.tgt; lbl = (lbl, a.lbl)};;

let convert_residual_into_basic_graph_v2 (g_init : int graph) (g : int graph) : (int * int) graph =
  let convert_arc (acc : (int * int) graph) (a : int arc) = match Graph.find_arc g a.tgt a.src with (* a.tgt = B et a.src = A *) 
  (* Comme on part de g_init, on sait que le lien A-->B existe dans g, reste à vérifier si B-->A existe *)
    | None -> new_arc acc (arc_int_to_int_int_v2 a) (* Seulement A-->B existe dans g *)
    | Some b -> new_arc acc (arc_int_to_int_int_v3 a b.lbl) in (* A-->B existe et B-->A existe dans g *)
  Graph.e_fold g_init convert_arc (clone_nodes g_init);;


let convert_residual_into_basic_graph (g_init : int graph) (g : int graph) : (int * int) graph =
  let convert_arc (acc : (int * int) graph) (a : int arc) = match Graph.find_arc g a.tgt a.src with
    | None -> raise CasQuiExistePas
    | Some b -> new_arc acc (arc_int_to_int_int a b.lbl) in
  Graph.e_fold g_init convert_arc empty_graph;;

let rec print_chemin = function
  | [] -> ()
  | t::q -> begin
    Printf.printf "%d " t;
    print_chemin q
    end;; 

let ford_fulkerson (g : int graph) (src : id) (tgt : id) : (int * int) graph =
  let rec loop g = 
    let chemin = algo_profondeur g [] src tgt in
      print_chemin chemin;
      Printf.printf "\n";
      match chemin with
        |[] -> Printf.printf "fin de Ford Fulkerson putaing"; g(* Recherche terminée*) 
        |_::_ -> 
          let value = find_value g chemin max_int in
          assert (value >= 0);
          let g1 = augmenter_chemin g chemin value in 
          loop g1
      in
  convert_residual_into_basic_graph_v2 g (loop g)
;;

