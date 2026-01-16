open Graph

val ford_fulkerson : int graph -> int -> int -> (int * int) graph

val algo_profondeur : int graph -> int list -> int -> int -> int list

val find_value : int graph -> int list -> int -> int

val augmenter_chemin : int graph -> int list -> int -> int graph

val convert_residual_into_basic_graph : int graph -> int graph -> (int * int) graph 


val max_flow_min_cost : (int*int) graph -> int -> int -> (int * int * int) graph

val algo_dijkstra : (int*int) graph -> int -> int -> (int list)

val find_value2 : (int*int) graph -> int list -> int -> int

val augmenter_chemin2 : (int*int) graph -> int list -> int -> (int * int) graph

val convert_residual_into_basic_graph2 : (int * int) graph -> (int * int) graph -> (int * int * int) graph 

val g_init : int graph -> (int * int) graph