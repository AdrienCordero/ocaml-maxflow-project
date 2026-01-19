open Graph
open Tools


let rec algo_profondeur (g:int graph) (sommets_list: int list) (src : int) (tgt : int) : (int list) =
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

  let arcs = out_arcs g src in 
  if (List.exists (fun a -> a.tgt =tgt) arcs) then begin (* Si la tgt est dans les arcs *)
    let a = List.find (fun a -> a.tgt = tgt) arcs in
    if (a.lbl = 0) then
      loop arcs (* On ne doit pas s'arrêter, il faut continuer à chercher (old : [])*)
    else 
    [src;tgt] 
    end 
  else 
    loop arcs
;;

let algo_dijkstra (g: (int*int) graph) (src : int) (tgt : int) : (int list) = 
  (* Initialisation *)
  let n = n_fold g (fun acc _ -> acc+1) 0 in (* On récupère le nbr de nodes*)
  let dist = Array.make n max_int in (* Liste des distances des sommets *)
  let visited = Array.make n false in (* Liste des sommets visités *)
  let pred = Array.make n (-1) in (* Liste des predecesseurs *)
  dist.(src) <- 0; (* Mettre la source a 0 *)

  (* Il faut créer Q qui est l'ensemble des noeuds de g *)
  let q = n_fold g (fun acc id -> id ::acc) [] in  

  (* Mise à jour des distances*)
  let maj_distances (dist : int array) (pred : int array) (s1 : int ) (s2 : int) ((cap,cost) : int*int) =
    if (cap >0 && dist.(s1) <> max_int && dist.(s2) > dist.(s1) + cost) then begin
      dist.(s2) <- dist.(s1) + cost;
      pred.(s2) <- s1
    end
  in

  (* Renvoie le sommet non visité avec la distance minimale *)
  let rec find_min (nodes : int list) (dist : int array) (visited : bool array) (min :int) : (int) =
    match nodes with 
      |[] -> min
      |x::rest ->
        if (visited.(x)) then (* On ne prend pas les sommets déjà visités *)
          find_min rest dist visited min
        else
          match min with 
            | -1 -> find_min rest dist visited x (* Aucun sommet selectionné pour le moment, je prends le sommet courant *)
            |_ -> 
              if (dist.(x)< dist.(min)) then (* On a déjà un sommet, je regarde si le courant est plus petit *)
                find_min rest dist visited x (* J'ai un nouveau sommet min *)
              else 
                find_min rest dist visited min (* Je garde l'ancien sommet *)
  in

  (* Boucle principale récursive *)
  let rec loop (q: int list) = 
    match q with (* Tant qu'il y a des sommets dans G *)
      |[] -> ()
      |_ -> 
        let a = find_min q dist visited (-1) in (* Je trouve le sommet a dont la distance est la plus proche *)
        if (a = -1) then ()
        else begin 
          visited.(a)<-true; (* Je passe le sommet comme étant visité *)
          let arcs = out_arcs g a in (* Je regarde les voisins de ce sommet min *)
          List.iter(fun arc -> maj_distances dist pred a arc.tgt arc.lbl) (arcs); (* Je mets à jour les distances *)
          loop (List.filter (fun x-> x<>a) q)
        end
  in
  loop q;

  (* Reconstruction du chemin *)
  let rec build_path v acc = 
    if (v = -1) then 
      acc
  else 
    build_path pred.(v) (v::acc)
  in 

  if (dist.(tgt) = max_int) then 
    []
  else 
    build_path tgt [] 
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


let rec find_value2 (g :(int*int) graph) (chemin :int list) (m : int) : (int) =
  match chemin with 
  | [] | _::[] -> m
  | t::y::rest -> begin match Graph.find_arc g t y with
      |None -> raise CasQuiExistePas
      |Some x ->
          let (cap, _) = x.lbl in 
          find_value2 g (y::rest) (min m cap)
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

let rec augmenter_chemin2 (g:(int*int) graph) (chemin: int list) (min: int) : ((int*int) graph) =
  match chemin with 
    | [] | _::[] -> g
    |x::y::rest -> begin match Graph.find_arc g x y with
      |None -> raise CasQuiExistePas
      |Some a -> 
        let (cap,cost) = a.lbl in
        let arc = {src = a.src; tgt = a.tgt; lbl = (cap - min, cost)} in
        let g1 = new_arc g arc in
        augmenter_chemin2 (g1) (y::rest) (min) (* augmenter_chemin (add_arc g a.src a.tgt (-min)) (y::rest) min *)
    end
;;


(*let arc_int_to_int_int (a : int arc) (lbl : int) : (int * int) arc = {src = a.src; tgt = a.tgt; lbl = (lbl + 2, a.lbl)};;
*)
let arc_int_to_int_int_v2 (a : int arc) : (int * int) arc = {src = a.src; tgt = a.tgt; lbl = (0, a.lbl)};;
let arc_int_to_int_int_v3 (a : int arc) (lbl : int) : (int * int) arc = {src = a.src; tgt = a.tgt; lbl = (lbl, a.lbl)};;

let arc_int_int_to_int_int_int (a : (int* int) arc) (lbl : int) : (int * int * int) arc = 
  let (cap,cost) = a.lbl in 
  {src = a.src; tgt = a.tgt; lbl = (cap-lbl,cap,cost)}
;;

let convert_residual_into_basic_graph (g_init : int graph) (g : int graph) : (int * int) graph =
  let convert_arc (acc : (int * int) graph) (a : int arc) = match Graph.find_arc g a.tgt a.src with (* a.tgt = B et a.src = A *) 
  (* Comme on part de g_init, on sait que le lien A-->B existe dans g, reste à vérifier si B-->A existe *)
    | None -> new_arc acc (arc_int_to_int_int_v2 a) (* Seulement A-->B existe dans g *)
    | Some b -> new_arc acc (arc_int_to_int_int_v3 a b.lbl) in (* A-->B existe et B-->A existe dans g *)
  Graph.e_fold g_init convert_arc (clone_nodes g_init)
;;

let convert_residual_into_basic_graph2 (g_init : (int*int) graph) (g : (int*int) graph) : (int*int*int) graph =
  let convert_arc (acc : (int * int * int) graph) (a : (int*int) arc) = match Graph.find_arc g a.src a.tgt with 
    | None -> raise CasQuiExistePas  
    | Some b -> 
      let (cap,_) = b.lbl in 
      new_arc acc (arc_int_int_to_int_int_int a cap) in 
  Graph.e_fold g_init convert_arc (clone_nodes g_init)
;;


(*let convert_residual_into_basic_graph (g_init : int graph) (g : int graph) : (int * int) graph =
  let convert_arc (acc : (int * int) graph) (a : int arc) = match Graph.find_arc g a.tgt a.src with
    | None -> raise CasQuiExistePas
    | Some b -> new_arc acc (arc_int_to_int_int a b.lbl) in
  Graph.e_fold g_init convert_arc empty_graph;;*)

let rec print_chemin = function
  | [] -> ()
  | t::q -> begin
    Printf.printf "%d " t;
    print_chemin q
    end;; 

let ford_fulkerson (g : int graph) (src : id) (tgt : id) : (int * int) graph =
  Printf.printf "FORD_FULKERSON \n";
  let rec loop g = 
    let chemin = algo_profondeur g [] src tgt in
      print_chemin chemin;
      Printf.printf "\n";
      match chemin with
        |[] -> g(* Recherche terminée*) 
        |_::_ -> 
          let value = find_value g chemin max_int in
          assert (value >= 0);
          let g1 = augmenter_chemin g chemin value in 
          loop g1
      in
  convert_residual_into_basic_graph g (loop g)
;;

let max_flow_min_cost(g : (int*int) graph) (src : id) (tgt : id) : (int * int * int) graph =
Printf.printf "MAX_FLOW_MIN_COST \n";
  let rec loop g = 
    let chemin = algo_dijkstra g src tgt in
      print_chemin chemin;
      Printf.printf "\n";
      match chemin with
        |[] -> g(* Recherche terminée*) 
        |_::_ -> 
          let value = find_value2 g chemin max_int in
          assert (value >= 0);
          let g1 = augmenter_chemin2 g chemin value in 
          loop g1
      in
  convert_residual_into_basic_graph2 g (loop g)
;;

let g_init (g : int graph) : (int * int) graph =
  (* Créer un graphe avec les mêmes nœuds *)
  let g1 = clone_nodes g in
  (*  Ajouter les arcs avec capacité = 1 *)
  Graph.e_fold g (fun acc arc -> let cost = arc.lbl in new_arc acc { src = arc.src; tgt = arc.tgt; lbl = (1, cost) }) g1
;;
