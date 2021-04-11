
type t = int array

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
  let degPlusOne = max (length p) (length q) in
  Array.init degPlusOne (fun i -> p.(i) + q.(i))


