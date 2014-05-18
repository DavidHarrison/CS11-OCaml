(*
 * eval.ml
 *
 *     Evaluator.
 *
 *)

open Env

exception Type_error of string

let rec eval ast env =
   match ast with
      | Ast.Expr_unit    -> Val_unit
      | Ast.Expr_bool b  -> Val_bool b
      | Ast.Expr_int  i  -> Val_int  i
      | Ast.Expr_id   id -> Env.lookup env id
      | Ast.Expr_define (id, e) -> begin
          Env.add env id (eval e env);
          Val_unit
        end
      | Ast.Expr_if (test_e, then_e, else_e) -> begin
              match (eval test_e env) with
              | Val_bool true  -> (eval then_e env)
              | Val_bool false -> (eval else_e env)
              | _              ->
                      raise (Type_error "Non-boolean value given as condition")
        end
      | Ast.Expr_lambda (ids, exprs) -> Val_lambda (env, ids, exprs)
      | Ast.Expr_apply (e, es) -> begin
           (* Evaluate all the operands (all arguments except the first. *)
           let operands = List.map (fun x -> eval x env) es
           (* Evaluate the function argument (the first argument). *)
           and f = eval e env in
              match f with
              | Val_prim f' -> f' operands
              | Val_lambda (envl, ids, exprs) -> begin
                  let envn = make (Some envl) in
                  Env.add_all envn ids operands;
                  List.hd (List.rev (List.map (fun x -> eval x envn) exprs))
                end
              | _ -> raise (Type_error "Cannot apply non-function")
        end
