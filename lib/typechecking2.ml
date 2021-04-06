open Complex
open Ast

(* The type that global contexts map idents to *)
type ident_lookup =
  | TypeIdent of (ident * typ) list
  | AssumedIdent of typ
  | DefinedIdent of typ * term

exception TypeCheckingError

(** Auxiliary functions for type-checking *)

(* Get an ordered list of pairs of elements of a list *)
let rec getpairs = function
  | [] -> []
  | a :: l -> (List.map (fun b -> (a,b)) l) @ getpairs l

(* Return the square norm of a non-empty list of complex numbers *)
let rec vect_norm2 = function
  | [] -> raise (Invalid_argument "Cannot calculate norm of a zero-dimensional vector")
  | [z] -> norm2 z
  | z :: v -> norm2 z +. vect_norm2 v

(* Parse a lambda abstraction from a pattern match *)
let lambda_from_pattern_match constr_name constr_tp pat tm =
  match pat with
  | [] -> raise TypeCheckingError
  | hd :: arg_names -> if hd <> constr_name then raise TypeCheckingError else
    let rec lambda_from_pattern_match_rec tp names =
      begin match tp, names with
      | Lolli (tp1, tp2), arg :: names' -> Lambda (arg, tp1, lambda_from_pattern_match_rec tp2 names')
      | ConstType _, [] -> tm
      | _, _ -> raise TypeCheckingError
      end
    in lambda_from_pattern_match_rec constr_tp arg_names

(* Look up what exactly this does to make better comment eep *)
let rec match_case_type matched_tp_name match_expr_tp constr_tp =
  match constr_tp with
  | ConstType c -> if c = matched_tp_name
                    then ConstType matched_tp_name
                    else (* shouldn't get here *) ConstType c
  | Lolli (tp1, tp2) -> Lolli (tp1, match_case_type matched_tp_name match_expr_tp tp2)

(**  Type Deduction *)

let ident_is_type env id =
  match Hashtbl.find_opt env id with
  | Some (TypeIdent _) -> true
  | _ -> false

(* Given valid global environment env, is tp a valid type *)
let rec valid_type env tp =
  match tp with
  | Lolli (tp1,tp2) -> valid_type env tp1 && valid_type env tp2
  | ConstType c -> ident_is_type env c

(* maps type A_1 -o ... -o A_n -o (ConstType c) to ([A_1; ...; A_n], c) *)
let rec get_constr_type_args = function
  | ConstType c -> ([], c)
  | Lolli (tp1, tp2) -> let (l,c) = get_constr_type_args tp2 in (tp1 :: l, c)

let rec is_constructor_type_of_ind_type env ind_tp_name constr_tp =
  match constr_tp with
  | ConstType c -> c = ind_tp_name && not (Hashtbl.mem env ind_tp_name)
  | Lolli (tp1, tp2) ->
      let (l, c) = get_constr_type_args tp1 in
      (c = ind_tp_name || valid_type env (ConstType c))
      && List.for_all (valid_type env) l
      && is_constructor_type_of_ind_type env ind_tp_name tp2

(* TODO: make sure names are unique lol *)
let valid_ind_def env ind_tp_name constructors =
  List.for_all (fun (_, constr_tp) ->
    is_constructor_type_of_ind_type env ind_tp_name constr_tp) constructors


let rec type_of_term env tm : typ =
  let ctx = Hashtbl.create 10 in
  let tp = type_of_term_ret env ctx tm in
  if Hashtbl.length ctx = 0
    then tp
    else raise TypeCheckingError

(* Given valid global environment env and valid local context ctx, return the type of tm? *)
(* Raises TypeCheckingError *)
(* ctx is an effectful argument, losing all variables used by tm *)
and type_of_term_ret env ctx tm : typ =
  match tm with
  | IdentTerm id ->
      begin try
        let tp = Hashtbl.find ctx id in
          Hashtbl.remove ctx id;
          tp
      with Not_found ->
        try
          begin match Hashtbl.find env id with
          | AssumedIdent tp -> tp
          | _ -> raise TypeCheckingError
          end
        with Not_found -> raise TypeCheckingError
      end
  | Lambda (x, tp, tm') ->
      Hashtbl.add ctx x tp;
      type_of_term_ret env ctx tm'
  | App (func_tm, arg_tm) ->
      let func_tp = type_of_term_ret env ctx func_tm in
      let arg_tp = type_of_term_ret env ctx arg_tm in
      begin match func_tp with
      | Lolli (tp1, tp2) -> if tp1 = arg_tp then tp2 else raise TypeCheckingError
      | _ -> raise TypeCheckingError
      end
  | LinComb [] -> raise TypeCheckingError
  | LinComb ((_, tm1) :: l' as l) ->
      if Hashtbl.length ctx <> 0 then raise TypeCheckingError
      else
        begin match type_of_term_ret env ctx tm1 with
        | Lolli _ -> raise TypeCheckingError
        | _ as tp ->
          if List.for_all (fun (_,tmi) -> type_of_term_ret env ctx tmi = tp) l'
          then
            if List.for_all (fun ((_,tmi),(_,tmj)) -> orth env ctx tmi tmj) (getpairs l)
            then
              if vect_norm2 (List.map fst l) = 1.
              then tp
              else raise TypeCheckingError
            else raise TypeCheckingError
          else raise TypeCheckingError
      end
  | Match (tm, tp, pat_match_list) ->
      begin match type_of_term_ret env ctx tm with
      | Lolli _ -> raise TypeCheckingError
      | (ConstType c) ->
        (* Don't have to catch Not_found because last type check was success *)
        begin match Hashtbl.find env c with
        | TypeIdent constructors ->
          let lam_list = List.map2 (fun (constr_name, constr_tp) (pat, tm) ->
            lambda_from_pattern_match constr_name constr_tp pat tm) constructors pat_match_list in
          begin match lam_list, constructors with
          | [], [] -> tp
          | f1 :: lam_list' as lam_list, (_, tp1) :: constructors' ->
            let ctx_copy = Hashtbl.copy ctx in
            let case_tp1 = type_of_term_ret env ctx f1 in
            if case_tp1 = match_case_type c tp tp1
            then
              if (List.for_all2
                   (fun fi (_, tpi) ->
                     let ctxi = Hashtbl.copy ctx_copy in
                     type_of_term_ret env ctxi fi = match_case_type c tp tpi
                     && ctx = ctxi)
                 lam_list' constructors')
              then
                let consumed_ctx = Hashtbl.copy ctx_copy in
                Hashtbl.iter (fun x _ -> Hashtbl.remove consumed_ctx x) ctx;
                if List.for_all (fun (fi,fj) -> orth env consumed_ctx fi fj) (getpairs lam_list)
                then tp
                else raise TypeCheckingError
              else raise TypeCheckingError
            else raise TypeCheckingError
          | _, _ -> raise TypeCheckingError
          end
        | _ -> (* Only get here if last type check ran incorrectly *) raise TypeCheckingError
        end
      end
(*
and orth env ctx s t =
  true *)
and orth _ _ _ _ = true

let valid_term env tp tm =
  type_of_term env tm = tp


(* Check everywhere that we are checking that we aren't doubling up on names *)












