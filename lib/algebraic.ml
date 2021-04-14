open Poly

type t = {
  poly: Poly.t;
  lbound: float;
  ubound: float;
}

module Int_matrix = Matrix.Make (Int)
module Poly_matrix = Matrix.Make (Poly)

let companion_matrix p =
  let d = degree p in
  Int_matrix.init (d - 1) ( fun i j ->
    if i = j + 1 then p.(d) else
    if j = d - 1 then - p.(i) else
    0
  )

let char_poly mat =
  let d = Int_matrix.dim mat in
  let aux1 = ref Int_matrix.zero in
  let aux2 = ref Int_matrix.zero in
  let p = Array.make (d + 1) 1 in
  for k = 1 to Int_matrix.dim mat do
    aux1 := Int_matrix.(!aux2 + scalar_mul p.(d - k + 1) one);
    aux2 := Int_matrix.(mat * !aux1);
    p.(d - k) <- - (Int_matrix.tr !aux2) / k
  done; p

let zero = {poly=[| 0; 1 |]; lbound=-1.; ubound=1.;}
let one = {poly=[| -1; 1 |]; lbound=0.5; ubound=1.5;}

