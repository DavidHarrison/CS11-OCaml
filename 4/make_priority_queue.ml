(* file: make_priority_queue.ml *)

(* Type for ordered comparisons. *)
type comparison = LessThan | Equal | GreaterThan


(* Signature for ordered objects. *)
module type OrderedSig =
  sig
    type t
    val compare: t -> t -> comparison
  end


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

module MakePriorityQueue (Elt : OrderedSig) 
  : (PriorityQueueSig with type t = Elt.t) =
  struct
    exception Empty

    type t = Elt.t
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
            | Node (vb, kb, lb, rb) -> begin
                match Elt.compare va vb with
                | Equal       -> Node (va, (min ka kb) + 1, la, (merge ra b))
                | LessThan    -> Node (va, (min ka kb) + 1, la, (merge ra b))
                | GreaterThan -> Node (vb, (min ka kb) + 1, lb, (merge a rb))
            end
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
        and comp x y =
            match Elt.compare x y with
            | Equal       ->  0
            | LessThan    -> -1
            | GreaterThan ->  1
        in from_list' (List.rev (List.sort comp l)) Leaf 1
    (* O(n*log(n))
     * let from_list l =
     *     match q with
     *     | []        -> Leaf
     *     | (x :: xs) -> insert (from_list xs) x
     *)
  end

module OrderedString =
  struct
    type t = string
    let compare x y = 
    if x = y then Equal else if x < y then LessThan else GreaterThan
  end

module StringPQ = MakePriorityQueue(OrderedString)

(* val heap_sort   : t list -> t list *)
let heap_sort l =
    let rec sort_heap q z =
        try
            sort_heap (StringPQ.delete_min q)
                      ((StringPQ.find_min q) :: z)
        with
        | StringPQ.Empty -> z
    in List.rev (sort_heap (StringPQ.from_list l) [])
;;


let list1 = heap_sort [];;
let list2 = heap_sort ["Hello";"World";"How";"Is";"It";"Going";"Today"];;
let list3 = heap_sort ["A";"B";"C";"D";"E";"F";"G";"H";"I"];;
let list4 = heap_sort ["I";"H";"G";"F";"E";"D";"C";"B";"A"];;
