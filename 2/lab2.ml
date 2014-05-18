(* file: lab2.ml *)

(* insertion_sort 'a list -> ('a -> int) -> 'a list *)
let rec insertion_sort l f =
    let rec insert v l =
        match l with
        | [] -> [v]
        | (x :: xs) -> if f v x = -1 then v :: l
                       else x :: (insert v xs)
    in
    match l with
    | [] -> []
    | (x :: xs) -> insert x (insertion_sort xs f)
;;

(* could be made polymorphic *)
type complex = {
    real : float;
    imag : float;
}

(* val complex_of_float : float -> complex *)
let complex_of_float f = { real = f; imag = 0.}
(* val complex_of_float : float -> complex *)
let complex_of_int i = complex_of_float (float_of_int i)

(* val compare_complex : complex -> complex -> int *)
let compare_complex a b =
    match a, b with
    | _, _ when a.real > b.real ->  1
    | _, _ when a.real < b.real -> -1
    | _, _ when a.imag > b.imag ->  1
    | _, _ when a.imag < b.imag -> -1
    | _, _                      ->  0
;;

type number =
    | Int of int
    | Float of float
    | Complex of complex
;;

let compare_number x y =
    match x with
    | Int a ->
            (match y with
             | Int b     -> compare a b
             | Float b   -> compare (float_of_int a) b
             | Complex b -> compare_complex (complex_of_int a) b
            )
    | Float a ->
            (match y with
             | Int b     -> compare a (float_of_int b)
             | Float b   -> compare a b
             | Complex b -> compare_complex (complex_of_float a) b
            )
    | Complex a ->
            (match y with
             | Int b     -> compare_complex a (complex_of_int b)
             | Float b   -> compare_complex a (complex_of_float b)
             | Complex b -> compare_complex a b
            )
;;

let all = [Int 1;Float 3.5;Complex (complex_of_float 5.8);Float 2.4;Int 5]
let sorted = insertion_sort all compare_number


(* quicksort : 'a list -> ('a -> int) -> 'a list *)
let rec quicksort l f =
    let rec split p xs ls gs =
        match xs with
        | [] -> (ls,gs)
        | (y :: ys) when f y p = -1 -> split p ys (y :: ls) gs
        | (y :: ys)                 -> split p ys ls (y :: gs)
    in
    match l with
    | [] -> []
    | (y :: ys) ->
            (match (split y ys [] []) with
             ls, gs -> (quicksort ls f) @ (y :: (quicksort gs f)))
;;



type side = Left | Right
let rec split vs ls rs s =
    match vs, s with
    | [], _ -> (ls,rs)
    | (x :: xs), Left  -> split xs (x :: ls) rs        Right
    | (x :: xs), Right -> split xs ls        (x :: rs) Left

(* mergesort: 'a list -> ('a -> int) -> 'a list *)
let rec mergesort l f =
    let rec merge_iter ls rs z =
        match ls, rs with
        | [], _ -> (List.rev rs) @ z
        | _, [] -> (List.rev ls) @ z
        | (x :: xs), (y :: ys) when f x y = -1 -> merge_iter xs rs (x :: z)
        | (x :: xs), (y :: ys)                 -> merge_iter ls ys (y :: z)
    in
    let merge ls rs = List.rev (merge_iter ls rs [])
    in
    match (split l [] [] Left) with
    | [], rs -> rs
    | ls, [] -> ls
    | ls, rs -> merge (mergesort ls f) (mergesort rs f)
;;

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* count_leaves : 'a tree -> int *)
let rec count_leaves t =
    match t with
    | Leaf -> 1
    | Node (_, l, r) -> (count_leaves l) + (count_leaves r)
;;

(* map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f t =
    match t with
    | Leaf -> Leaf
    | Node (v, l, r) -> Node ((f v), (map_tree f l), (map_tree f r))
;;

(* tree_of_list : 'a list -> 'a tree *)
let rec tree_of_list l =
    match l with
    | [] -> Leaf
    | (x :: xs) ->
            (match (split xs [] [] Left) with
             ls, rs -> Node (x, (tree_of_list ls), (tree_of_list rs))
            )
;;

(* print_tree : 'a tree -> string *)
let print_tree t =
    let indent = 2 in
    let rec print_tree_iter t n =
        match t with
        | Leaf -> (String.make (indent * n) ' ') ^ "Leaf"
        | Node (v, l, r) -> (String.make (indent * n) ' ')
                            ^ "Node " ^ (string_of_int v) ^ "\n"
                            ^ (print_tree_iter l (n + 1)) ^ "\n"
                            ^ (print_tree_iter r (n + 1))
    in
    Printf.printf "%s" ((print_tree_iter t 0) ^ "\n")
;;
