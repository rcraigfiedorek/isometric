open Complex
open Ast

(*  type deduction etc. *)

let ident_is_type env id =
  match find_opt env id with
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
  | Lolli (tp1, tp2) -> let (l,c) = constr_type_argtype_sep tp2 in (tp1 :: l, c)

let rec is_constructor_type_of_ind_type env ind_tp_name constr_tp =
  match constr_tp with
  | ConstType c -> c = ind_tp_name && not (Hashtbl.mem env ind_tp_name)
  | Lolli (tp1, tp2) ->
      let (l, c) = get_constr_type_args tp1 in
      (c = ind_type_name || valid_type env (ConstType c))
      && List.for_all (valid_type env) l
      && is_constructor_type_of_ind_type env ind_tp_name tp2

Exception TypeCheckingError

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
  | VarTerm x ->
      try
        let tp = Hashtbl.find ctx x in
          Hashtbl.remove ctx x;
          tp
      with Not_found -> raise TypeCheckingError
  | ConstTerm c ->
      try
        let tp = Hashtbl.find env c in tp
      with Not_found -> raise TypeCheckingError
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
  | LinComb ((alpha1, tm1) :: l' as l) ->
      if Hashtbl.length ctx <> 0 then raise TypeCheckingError
      else
        begin match type_of_term_ret env ctx tm1 with
        | Lolli _ -> raise TypeCheckingError
        | _ as tp ->
          if List.for_all (fun (_,tmi) -> type_of_term_ret env ctx tmi = tp)
          then
            if List.for_all (fun ((_,tmi),(_,tmj)) -> orth env ctx tmi tmj) (getpairs l)
            then
              if vect_norm_sq (List.map fst l) = 1.
              then tp
              else raise TypeCheckingError
            else raise TypeCheckingError
          else raise TypeCheckingError
      end
  | Match (tm, tp, tm_list) ->
      begin match type_of_term_ret env ctx tm with
      | Lolli _ -> raise TypeCheckingError
      | (ConstType c) as match_tp ->
        (* Don't have to catch Not_found because last type check was success *)
        begin match Hashtbl.find env c with
        | TypeIdent constructors ->
          begin match tm_list, constructors with
          | [], [] -> tp
          | f1 :: tm_list', (_,tp1) :: constructors' ->
            let ctx1 = Hashtbl.copy ctx in
            let case_tp1 = tyupe_of_term_ret env ctx1 f1 in
            
          | _, _ -> raise TypeCheckingError
          end
        | _ -> (* Only get here if last type check ran incorrectly *) raise TypeCheckingError
        end
      end



      begin match type_of_term_ret gamma delta tm with
      | Some (delta', (ConstType c)) ->
          begin match get_constr_of_indtype c gamma with
          | Some constr_list ->
              begin match tm_list, constr_list with
              | [], [] -> Some (delta',tp)
              | f1 :: tm_list', (_,tp1) :: constr_list' ->
                  begin match type_of_term_ret gamma delta' f1 with
                  | None -> None
                  | Some (delta'', case_tp1) ->
                      if case_tp1 <> match_case_type c tp tp1 then None else
                      if List.length tm_list' <> List.length constr_list' then None else
                      if not (List.for_all2
                          (fun fi (_, tpi) ->
                            type_of_term_ret gamma delta' fi = Some (delta'', match_case_type c tp tpi))
                              tm_list' constr_list')
                      then None else
                      let case_resources = List.filter (fun p -> not (List.mem p delta'')) delta' in
                      if List.for_all (fun (fi,fj) -> orth gamma case_resources fi fj) (getpairs tm_list)
                      then
                        Some (delta'', tp)
                      else
                        None
                  end
              | _, _ -> None
              end
          | None -> None (* shouldn't get here *)
          end
      | _ -> None
      end

and orth gamma delta s t =
  true


let _ = print_string "Hello world.\n"











