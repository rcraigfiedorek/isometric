(* Square matrices over a ring *)

module type Ring =
  sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
    val mul: t -> t -> t
  end

module Make(R: Ring) =
  struct
    type base = R.t
    type t = (base array) array

    let init n f =
      Array.init n (fun i -> Array.init n (j -> f i j))

    let zero n = init (fun _ _ -> R.zero)

    let identity n = init (fun i j -> if i = j then R.one else R.zero)

    (* Validate and return the dimension of the matrices *)
    (* Assumes both matrices are square *)
    let validate_dim mat1 mat2 =
      let n = Array.length mat1 in
      if n = Array.length mat2 then n
      else raise Invalid_argument "Matrices must have matching dimension."

    let add mat1 mat2 =
      let n = validate_dim mat1 mat2 in
      init n (fun i j -> R.add mat1.(i).(j) mat2.(i).(j))

    let row i = Array.get i

    let col j = Array.map (Array.get j)

    let dot arr1 arr2 =
      let out = ref R.zero in
      Array.iter2 (fun x y -> out := R.add !out (R.mul x y)) arr1 arr2;
      !out

    let mul mat1 mat2 =
      let n = validate_dim mat1 mat2 in
      init n (fun i j -> dot (row i mat1) (col j mat2))





  end