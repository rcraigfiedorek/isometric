
type t = int array

let zero = [| 0 |]
let one = [| 1 |]

let degree p = Array.length p - 1

let init deg coeff_map =
  Array.init (deg + 1) coeff_map

(* evaluation of a polynomial at a real number (float) *)
let evalr p r =
 Array.fold_right (fun coeff prev -> r *. prev +. float_of_int coeff) 0. p

(* evaluation of a polynomial at a complex number *)
let evalc p z =
  Array.fold_right
    (fun coeff prev ->
      Complex.add (Complex.mul z prev) {re=float_of_int coeff; im=0.})
    Complex.zero p

let add p q =
  let d = max (degree p) (degree q) in
  init d (fun i -> p.(i) + q.(i))

(* Efficient polynomial multiplication here: *)
(* http://staff.ustc.edu.cn/~csli/graduate/algorithms/book6/chap32.htm *)
let mul p q =
  let out = Array.make (degree p + degree q + 1) 0 in
  for i = 0 to degree p do
    for j = 0 to degree q do
      out.(i + j) <- out.(i + j) + p.(i) * q.(j)
    done
  done;
  out




