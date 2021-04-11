open Polynomial
open Matrix

type t = {
  poly: Polynomial.t;
  lbound: float;
  ubound: float;
}

module Int_matrix = Matrix.Make (Int)
module Poly_matrix = Matrix.Make (Polynomial)



