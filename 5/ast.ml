(*
 * ast.ml
 *
 *     Abstract syntax tree.
 *
 *)

type id = string

type expr =
   | Expr_unit
   | Expr_bool   of bool
   | Expr_int    of int
   | Expr_id     of id
   | Expr_define of id * expr
   | Expr_if     of expr * expr * expr
   | Expr_lambda of id list * expr list
   | Expr_apply  of expr * expr list

exception InvalidSexpr


let rec ast_of_sexpr sx =
    match sx with
    | Sexpr.Expr_atom a -> begin
        match a with
        | Sexpr.Atom_unit   -> Expr_unit
        | Sexpr.Atom_bool b -> Expr_bool b
        | Sexpr.Atom_int  i -> Expr_int  i
        | Sexpr.Atom_id   i -> Expr_id   i
      end
    | Sexpr.Expr_list sexprs -> begin
        match sexprs with
        | []      -> raise InvalidSexpr
        | x :: xs -> begin
            match x with
            | Sexpr.Expr_atom (Sexpr.Atom_id a) -> begin
                Printf.printf "%s\n" a;
                match a with
                | "define" -> begin
                    match xs with
                    | (Sexpr.Expr_atom (Sexpr.Atom_id b)) :: sexpr :: [] ->
                            Expr_define (b, (ast_of_sexpr sexpr))
                    | _ -> raise InvalidSexpr
                  end
                | "if" -> begin
                    match xs with
                    | cond :: a :: b :: [] -> Expr_if ( (ast_of_sexpr cond)
                                                      , (ast_of_sexpr a)
                                                      , (ast_of_sexpr b))
                    | _ -> raise InvalidSexpr
                  end
                | "lambda" -> begin
                    let get_id v =
                        match v with
                        | Sexpr.Expr_atom (Sexpr.Atom_id i) -> i
                        | _ -> raise InvalidSexpr
                    in
                    match xs with
                    | (Sexpr.Expr_list vars) :: sexprs ->
                            Expr_lambda ( (List.map get_id vars)
                                        , (List.map ast_of_sexpr sexprs))
                    | _ -> raise InvalidSexpr
                  end
                | "apply" -> begin
                    match xs with
                    | f :: sexprs ->
                            Expr_apply ( (ast_of_sexpr f)
                                       , (List.map ast_of_sexpr sexprs))
                    | _ -> raise InvalidSexpr
                  end
                | w -> Expr_apply ( (Expr_id w)
                                  , (List.map ast_of_sexpr xs))
              end
            | Sexpr.Expr_list _ -> Expr_apply ( (ast_of_sexpr x)
                                              , (List.map ast_of_sexpr xs))
          end
      end
                     

let string_of_ast ast =
   let sprintf  = Printf.sprintf in  (* to make the code cleaner *)
   let spaces n = String.make n ' ' in
   let rec string_of_ids id_lst = 
      match id_lst with
         | [] -> ""
         | [id] -> id
         | h :: t -> h ^ " " ^ (string_of_ids t)
   in

   let rec iter ast indent =
      let string_of_exprs e_list =
         (List.fold_left (^) ""
             (List.map
                 (fun e -> "\n" ^ iter e (indent + 2))
                 e_list))
      in
      match ast with
         | Expr_unit    -> sprintf "%sUNIT"       (spaces indent) 
         | Expr_bool b  -> sprintf "%sBOOL[ %b ]" (spaces indent) b
         | Expr_int  i  -> sprintf "%sINT[ %d ]"  (spaces indent) i
         | Expr_id   id -> sprintf "%sID[ %s ]"   (spaces indent) id
         | Expr_define (id, e) -> 
              sprintf "%sDEFINE[%s\n%s ]" 
                 (spaces indent) id (iter e (indent + 2))
         | Expr_if (test_clause, then_clause, else_clause) ->
              sprintf "%sIF[\n%s\n%s\n%s ]"
                 (spaces indent) 
                 (iter test_clause (indent + 2))
                 (iter then_clause (indent + 2))
                 (iter else_clause (indent + 2))
         | Expr_lambda (ids, body) ->
              sprintf "%sLAMBDA[(%s)%s ]"
                 (spaces indent)
                 (string_of_ids ids)
                 (string_of_exprs body)
         | Expr_apply (operator, operands) ->
              sprintf "%sAPPLY[\n%s%s ]"
                 (spaces indent)
                 (iter operator (indent + 2))
                 (string_of_exprs operands)
   in
      "\n" ^ iter ast 0 ^ "\n"


let ast_test infile =
   let lexbuf = Lexing.from_channel infile in
   let rec loop () =
      let sexpr  = Parser.parse Lexer.lex lexbuf in
         match sexpr with
            | None -> ()
            | Some s ->
                 let expr = ast_of_sexpr s in
                    Printf.printf "%s\n" (string_of_ast expr); 
                    flush stdout;
                    loop ()
   in
      loop ()
