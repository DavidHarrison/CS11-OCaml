(* file: warmups.ml *)

(* val sqrt : float -> float -> float *)
let sqrt tol x =
    let rec sqrt_iter tol x y =
        if abs_float ((y ** 2.) -. x) < tol then y
        else sqrt_iter tol x ((y +. (x /. y)) /. 2.)
    in sqrt_iter tol x 1.
;;

(* val sqrt2 : float -> float -> float *)
let sqrt2 = sqrt 0.00001

(* val factorial1 : int -> int *)
let rec factorial1 x =
    if x = 0 then 1
    else x * (factorial1 (x - 1))
;;

(* val factorial2 : int -> int *)
let rec factorial2 x =
    match x with
    | 0 -> 1
    | _ -> x * (factorial2 (x - 1))
;;

(* val factorial3 : int -> int *)
let factorial3 x =
    let rec factorial_iter x z =
        match x with
        | 0 -> z
        | _ -> factorial_iter (x - 1) (x * z)
    in factorial_iter x 1
;;

(* val fibonacci : int -> int *)
let fibonacci x =
    let rec fib_iter x n z p =
        match n with
        | _ when n = x -> z
        | _ -> fib_iter x (n + 1) (z + p) z
    in fib_iter x 1 1 0
;;
(*
 * While on the 64-bit machine that I am using, the value does not
 * overflow the size of int (in my case 63 bits), it would on a 32-bit machine
 * (which has an int of 31 bits). When the int overflows, it cycles around
 * values, and so a value, albeit incorrect, is still given. On my machine, the
 * value overflows the int at `fibonacci 93`.
 *)

(* val rev : a' list -> a' list *)
let rev l =
    let rec rev_iter l z =
        match l with
        | [] -> z
        | (v :: vs) -> rev_iter vs (v :: z)
    in rev_iter l []
;;

(* val map : ('a -> 'b) -> 'a list -> 'b list *)
let rec map f l =
    match l with
    | [] -> []
    | (v :: vs) -> (f v) :: (map f vs)
;;

(* val map2 : ('a -> 'b) -> 'a list -> 'b list *)
let map2 f l =
    let rec map_iter f l z =
        match l with
        | [] -> z
        | (v :: vs) -> map_iter f vs ((f v) :: z)
    in rev (map_iter f l [])
;;

(* val range : int -> int -> int list *)
let rec range a b = if a > b then [] else a :: range (a + 1) b

(* val roots : float list *)
let roots = map sqrt2 (map float_of_int (range 1 20))
