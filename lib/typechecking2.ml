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



let rec type_of_term env tm : typ option =
  match type_of_term_ret env [] tm with
  | Some ([],tp) -> Some tp
  | _ -> None

and type_of_term_ret gamma delta tm : (local_ctx * typ) option =
  match tm with
  | VarTerm x ->
      begin match List.assoc_opt x delta with
      | Some tp ->
          if wf gamma delta then Some (List.remove_assoc x delta, tp) else None
      | None -> None
      end
  | ConstTerm c ->
      let rec type_of_constant = function
        | [] -> None
        | GlobalAssum (c',tp) :: gamma' -> if c = c' then Some tp else type_of_constant gamma'
        | GlobalDef (c',tp,_) :: gamma' -> if c = c' then Some tp else type_of_constant gamma'
        | GlobalIndDef (_,constr_list) :: gamma' ->
            match List.assoc_opt c constr_list with
            | Some tp as o -> o
            | None -> type_of_constant gamma'
      in begin match type_of_constant gamma with
         | Some tp -> if wf gamma delta then Some (delta, tp) else None
         | None -> None
         end
  | Lambda (x, tp, tm') -> type_of_term_ret gamma ((x,tp) :: delta) tm'
  | App (f, tm') ->
      begin match type_of_term_ret gamma delta f with
      | Some (delta', Lolli(tp1,tp2)) ->
          begin match type_of_term_ret gamma delta' tm' with
          | Some (delta'', tp) -> if tp = tp1 then Some (delta'', tp2) else None
          | None -> None
          end
      | _ -> None
      end
  | LinComb [] -> None
  | LinComb ((alpha, tm') :: l' as l) ->
      begin match delta with
      | _ :: _ -> None
      | [] ->
          begin match type_of_term_ret gamma [] tm' with
          | None -> None
          | Some (_, Lolli _) -> None
          | Some (_, tp) ->
              if List.for_all (fun (_,tmi) -> type_of_term_ret gamma [] tmi = Some ([],tp)) l'
              then
                if List.for_all (fun ((_,tmi),(_,tmj)) -> orth gamma [] tmi tmj) (getpairs l)
                then
                  if vect_norm2 (List.map fst l) = 1. then Some ([],tp) else None
                else None
              else None
          end
      end
    | Match (tm, tp, tm_list) ->
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











