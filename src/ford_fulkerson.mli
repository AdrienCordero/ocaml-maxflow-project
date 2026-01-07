open Graph

val ford_fulkerson : int graph -> int -> int -> (int * int) graph

val algo_profondeur : int graph -> int list -> int -> int -> int list

val find_value : int graph -> int list -> int -> int

val augmenter_chemin : int graph -> int list -> int -> int graph

val convert_residual_into_basic_graph_v2 : int graph -> int graph -> (int * int) graph 