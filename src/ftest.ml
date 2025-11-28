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

  let g = from_file "graphs/graph1.txt" in
  let ff = ford_fulkerson (gmap g int_of_string) 1 6 in
  Gfile.export "graph1test.dot" ff (fun (a, b) -> Printf.sprintf "%d/%d" a b);

  let l = find_augmenting_path (gmap g int_of_string) 0 5 in
  Printf.printf "taille list : %d\nchemin : " (List.length l);
  List.iter (fun x -> Printf.printf "%d " x) l;

  ()

