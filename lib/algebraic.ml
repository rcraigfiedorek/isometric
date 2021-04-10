open Complex
open Algebraic

type t = {
  degree: int;
  coeff_map: int -> int;
  neighborhood: disk;
}

