open Complex
open Ast

(* auxiliary list functions *)

let rec list_uniq = function
  | [] -> true
  | hd :: tl -> (not (List.exists ((=) hd) tl)) && list_uniq tl

let rec getpairs = function
  | [] -> []
  | hd :: tl -> (List.map (fun x -> (hd, x)) tl) @ getpairs tl

exception Vector_length_zero

let rec vect_norm2 = function
  | [] -> raise Vector_length_zero
  | alpha :: l -> (norm2 alpha) +. (vect_norm2 l)

(* operations on syntax tree and related structures *)

let constr_list_to_ctx constr_list = List.map (fun (c,tp) -> GlobalAssum (c,tp)) constr_list

let rec local_names delta =
  match delta with
  | [] -> []
  | (x,_) :: delta' -> x :: local_names delta'

let rec global_names gamma =
  match gamma with
  | [] -> []
  | GlobalAssum (c,_) :: gamma' -> c :: global_names gamma'
  | GlobalDef (c,_,_) :: gamma' -> c :: global_names gamma'
  | GlobalIndDef (c, constr_list) :: gamma' ->
      c :: List.append (global_names (constr_list_to_ctx constr_list)) (global_names gamma')

let rec constr_type_argtype_sep = function
  | ConstType c -> ([], c)
  | Lolli (tp1, tp2) -> let (l,c) = constr_type_argtype_sep tp2 in (tp1 :: l,c)

let rec lolli_separate tp =
  match tp with
  | Lolli (tp1, tp2) -> tp1 :: lolli_separate tp2
  | _ -> [tp]

let get_constr_of_indtype c =
  List.find_map (fun x -> 
    match x with
    | GlobalIndDef (c',constr_list) -> if c = c' then Some constr_list else None
    | _ -> None
  )

let rec match_case_type matched_tp_name match_expr_tp constr_tp =
  match constr_tp with
  | ConstType c -> if c = matched_tp_name
                    then ConstType matched_tp_name
                    else (* shouldn't get here *) ConstType c
  | Lolli (tp1, tp2) -> Lolli (tp1, match_case_type matched_tp_name match_expr_tp tp2)

(*  type deduction etc. *)

let rec wf gamma delta =
  match delta with
  | (x,tp) :: delta' ->
      wf gamma delta' && valid_type gamma tp && not (List.mem x (local_names delta'))
  | [] ->
    match gamma with
    | [] -> true
    | GlobalAssum (c, tp) :: gamma' ->
        valid_type gamma tp && not (List.mem c (global_names gamma'))
    | GlobalDef (c, tm, tp) :: gamma' ->
        type_of_term gamma [] tm = Some tp && not (List.mem c (global_names gamma'))
    | GlobalIndDef (c, constr_list) :: gamma' ->
        wf gamma' []
          && List.for_all (fun (_,tp) -> is_constructor_type gamma' tp c) constr_list
          && list_uniq (c :: (List.map fst constr_list))
          && not (List.exists (fun ci -> List.mem ci (global_names gamma')) (List.map fst constr_list))

and valid_type gamma tp =
  match tp with
  | Lolli (tp1,tp2) -> valid_type gamma tp1 && valid_type gamma tp2
  | ConstType c ->
      wf gamma []
        && List.exists (fun decl -> match decl with GlobalIndDef(c',_) -> c = c' | _ -> false) gamma

and is_constructor_type gamma tp c =
  match tp with
  | ConstType c' -> c = c' && wf gamma [] && not (List.mem c (global_names gamma))
  | Lolli (tp1,tp2) ->
      let l,d = constr_type_argtype_sep tp1 in
      (c = d || valid_type gamma (ConstType d))
      && List.for_all (valid_type gamma) l
      && is_constructor_type gamma tp2 c

and type_of_term gamma delta tm : typ option =
  match type_of_term_ret gamma delta tm with
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
        | GlobalDef (c',_,tp) :: gamma' -> if c = c' then Some tp else type_of_constant gamma'
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











