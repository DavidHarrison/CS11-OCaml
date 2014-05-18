(*
 * sudoku.ml
 * Imperative Sudoku solver in OCaml
 *)

open Printf

(* The size of each side of the board. *)
let side_size = 9

(* The size of each side of a small box. *)
let box_size = 3

(* how many boxes fit accross the board *)
let boxes_across = side_size / box_size

(* The type of boards. *)
type board = int array array

(* Exception type for board solver. *)
exception Found of board

let min_num = 1
let max_num = 9


(*
 * Board representation:
 * -- a board is a 9x9 two-dimensional array of ints
 * -- each square contains an int between 0 and 9
 * -- 0 means that the board's real value is unknown.
 *)


(** Read in a sudoku board from a file.  Return the board. *)
let read_board filename =
    begin
        let b = Array.make_matrix side_size side_size 0 in
        let infile = open_in filename in
        for i = 0 to (side_size - 1)
        do
            let line = input_line infile in
            for j = 0 to (side_size - 1)
            do
                b.(i).(j) <- (int_of_char line.[j]) - (int_of_char '0')
            done;
        done;
        close_in infile;
        b
    end
;;


(** Print a sudoku board.  Return nothing. *)
let print_board chan board =
    begin
        for i = 0 to (side_size - 1)
        do
            for j = 0 to (side_size - 1)
            do
                fprintf chan "%d" board.(i).(j)
            done;
            fprintf chan "\n"
        done
    end
;;

let coord_valid b x y =
    let box_coords a b =
        let x = (a mod boxes_across) * box_size + (b mod box_size)
        and y = (a / boxes_across) * box_size + (b / box_size)
        in (x,y)
    in
    begin
        try
            for i = 0 to (side_size - 1)
            do
                let box = (x / 3) + (y / 3) * 3 in
                let (box_v_x,box_v_y) = box_coords box i in
                if b.(x).(y) <> 0 then begin
                    if i <> y && b.(x).(y) = b.(x).(i) then raise Exit;
                    if i <> x && b.(x).(y) = b.(i).(y) then raise Exit;
                    if box_v_x <> x && box_v_y <> y &&
                        b.(x).(y) = b.(box_v_x).(box_v_y)
                    then raise Exit
                end
            done;
            true
        with Exit -> false
    end
;;


(** Solve a sudoku board. 
    Return an option type giving the solution (a board)
    or None if no solution exists. *)
let rec solve_board board =
    begin
        try
            for i = 0 to (side_size - 1)
            do
                for j = 0 to (side_size - 1)
                do
                    if board.(i).(j) = 0 then begin
                        for k = min_num to max_num
                        do
                            board.(i).(j) <- k;
                            if coord_valid board i j = true then begin
                                match (solve_board board) with
                                | None -> ()
                                | (Some b) -> raise (Found b)
                            end;
                            board.(i).(j) <- 0
                        done;
                        raise Exit
                    end
                done
            done;
            raise (Found board)
        with
        | Exit -> None
        | (Found b) -> (Some b)
    end
;;
    

(** Solve a sudoku board taken from a file, and print the result. *)
let solve_board_from_file filename =
    let b = read_board filename in
    begin
        match solve_board b with
        | None   -> printf "Board has no solution.\n"
        | Some b -> print_board stdout b
    end


(** Entry point. *)
let _ = 
   if Array.length Sys.argv <> 2 then
      begin
         fprintf stderr "usage: %s board\n" Sys.argv.(0);
         exit 1
      end
   else
      begin
         solve_board_from_file Sys.argv.(1);
         exit 0
      end
