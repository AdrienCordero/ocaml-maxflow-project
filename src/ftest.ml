open Gfile
open Ford_fulkerson
open Tools


let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in

  (*Gfile.export "graph.dot" graph (fun x -> x);

  let g0 = empty_graph in
  let g1 = new_node g0 1 in
  let g2 = new_node g1 2 in
  let g3 = add_arc g2 1 2 10 in
  Gfile.export "g3.dot" g3 string_of_int;

  let g4 = new_node g3 4 in
  let g5 = new_node g4 5 in
  let g6 = add_arc g5 2 4 5 in
  let g7 = add_arc g6 2 5 6 in
  let g8 = add_arc g7 2 5 6 in
  Gfile.export "g8.dot" g8 string_of_int;*)

  let graph1 = from_file "graphs/graph1.txt" in
  Gfile.export "graph1.dot" graph1 (fun x -> x);

  let graph2 = from_file "graphs/graph2.txt" in
  Gfile.export "graph2.dot" graph2 (fun x -> x);

  let graph3 = from_file "graphs/graph3.txt" in 
  Gfile.export "graph3.dot" graph3 (fun x ->x);

  let graph4 = from_file "graphs/graph4.txt" in 
  Gfile.export "graph4.dot" graph4 (fun x ->x);

  let graph1_bip = from_file "graphs/graph1_bip.txt" in
  Gfile.export "graph1_bip.dot" graph1_bip (fun x -> x);

  let graph2_bip = from_file "graphs/graph2_bip.txt" in
  Gfile.export "graph2_bip.dot" graph2_bip (fun x -> x);

  (*let g1 = augmenter_chemin (gmap g int_of_string) [0; 3; 6;9;10] 1 in*)
  (*let g1 = augmenter_chemin (gmap g int_of_string) (algo_profondeur (gmap g int_of_string) [] 0 10) 1 in*)
  let int_int_to_string (a, b) = (string_of_int a) ^ "/" ^ (string_of_int b) in
  
  let rec print_chemin = function
    | [] -> ()
    | t::q -> begin
      Printf.printf "%d " t;
      print_chemin q
      end in
  let rec loop i g src dst= 
    if i = 10 then ()
    else begin
    let chemin = algo_profondeur g [] src dst in
    Printf.printf "\n%d : " i; print_chemin chemin;
    let valeur = find_value g chemin max_int in 
    let g1 = augmenter_chemin g chemin valeur in
    Gfile.export ("ite_"^(string_of_int i)^".dot") g (fun x -> string_of_int x);
    Gfile.export "converted_graph.dot" (convert_residual_into_basic_graph_v2 (gmap graph1 int_of_string) g) (fun x -> int_int_to_string x);
    loop (i+1) g1 src dst end in 
  loop 0 (gmap graph2_bip int_of_string) 0 11; 
  
  (* Gfile.export "g1.dot" g1 (fun x -> string_of_int x);
  Gfile.export "g2.dot" g2 (fun x -> string_of_int x); *)

  let ff_on_graph1 = ford_fulkerson (gmap graph1 int_of_string) 0 5 in
  Gfile.export "g1.dot" ff_on_graph1 (fun x -> int_int_to_string x);

  let ff_on_graph2 = ford_fulkerson (gmap graph2 int_of_string) 0 12 in
  Gfile.export "g2.dot" ff_on_graph2 (fun x -> int_int_to_string x);

  let ff_on_graph3 = ford_fulkerson (gmap graph3 int_of_string) 0 2 in
  Gfile.export "g3.dot" ff_on_graph3 (fun x -> int_int_to_string x);

  let ff_on_graph4 = ford_fulkerson (gmap graph4 int_of_string) 0 5 in
  Gfile.export "g4.dot" ff_on_graph4 (fun x -> int_int_to_string x);

  let ff_on_graph1_bip = ford_fulkerson (gmap graph1_bip int_of_string) 0 6 in
  Gfile.export "g1_bip.dot" ff_on_graph1_bip (fun x -> int_int_to_string x);

  let ff_on_graph2_bip = ford_fulkerson (gmap graph2_bip int_of_string) 0 11 in
  Gfile.export "g2_bip.dot" ff_on_graph2_bip (fun x -> int_int_to_string x);

  ()

