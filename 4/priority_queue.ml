(* Signature for priority queues. *)
module type PriorityQueueSig =
  sig
    exception Empty

    type t
    type queue

    val empty      : queue
    val is_empty   : queue  -> bool
    val insert     : queue  -> t -> queue
    val find_min   : queue  -> t
    val delete_min : queue  -> queue
    val from_list  : t list -> queue
  end


module PriorityQueue : (PriorityQueueSig with type t = int) =
  struct
    exception Empty

    type t = int
    type queue =
      | Leaf
      | Node of t * int * queue * queue

    (* val empty      : queue *)
    let empty = Leaf
    (* val is_empty   : queue  -> bool *)
    let is_empty q =
        match q with
        | Leaf              -> true
        | Node (_, _, _, _) -> false
    (* val merge     : queue  -> queue -> queue *)
    let rec merge a b =
        match a with
        | Leaf -> b
        | Node (va, ka, la, ra) -> begin
            let min x y = if x < y then x else y in
            match b with
            | Leaf                  -> a
            | Node (vb, kb, lb, rb) -> if va < vb
              then Node (va, (min ka kb) + 1, la, (merge ra b))
              else Node (vb, (min ka kb) + 1, lb, (merge a rb))
        end
    (* val insert     : queue  -> t -> queue *)
    let insert q x = merge q (Node (x, 1, Leaf, Leaf))
    (* val find_min   : queue  -> t *)
    let find_min q =
        match q with
        | Leaf              -> raise Empty
        | Node (v, _, _, _) -> v
    (* val delete_min : queue  -> queue *)
    let delete_min q =
        match q with
        | Leaf              -> raise Empty
        | Node (_, _, l, r) -> merge l r
    (* val from_list  : t list -> queue *)
    (* O(n) *)
    let from_list l =
        (* takes a descending order list *)
        let rec from_list' l z n =
            match l with
            | []        -> z
            | (x :: xs) -> from_list' xs (Node (x, n, z, Leaf)) (n + 1)
        in from_list' (List.rev (List.sort compare l)) Leaf 1
    (* O(n*log(n))
     * let from_list l =
     *     match q with
     *     | []        -> Leaf
     *     | (x :: xs) -> insert (from_list xs) x
     *)
  end


(* val heap_sort   : t list -> t list *)
let heap_sort l =
    let rec sort_heap q z =
        try
            sort_heap (PriorityQueue.delete_min q)
                      ((PriorityQueue.find_min q) :: z)
        with
        | PriorityQueue.Empty -> z
    in List.rev (sort_heap (PriorityQueue.from_list l) [])
;;

let list1 = heap_sort [];;
let list2 = heap_sort [1;5;6;2;3;4;8;2;6;1;7;8;2;9];;
let list3 = heap_sort [9;8;7;6;5;4;3;2;1];;
let list4 = heap_sort [1;2;3;4;5;6;7;8;9];;
