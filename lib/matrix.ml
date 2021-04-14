(* Square matrices over a ring *)

module type Ring =
  sig
    type t
    val zero: t
    val one: t
    val neg: t -> t
    val add: t -> t -> t
    val mul: t -> t -> t
  end

module Frac(R : Ring) =
  struct
    type t = R.t * R.t
    let zero = (R.zero, R.one)
    let one = (R.one, R.one)
    let neg (a, b) = (R.neg a, b)
    let add (a, b) (c, d) = (R.add (R.mul a d) (R.mul b c), R.mul b d)
    let mul (a, b) (c, d) = (R.mul a c, R.mul b d)
    let div (a, b) (c, d) = (R.mul a d, R.mul b c)
  end

module Make(R: Ring) =
  struct
    type base = R.t
    type t = (base array) array

    let init n f =
      Array.init n (fun i -> Array.init n (j -> f i j))

    let zero n = init (fun _ _ -> R.zero)

    let identity n = init (fun i j -> if i = j then R.one else R.zero)

    let dim mat = Array.length mat

    (* Validate and return the dimension of the matrices *)
    (* Assumes both matrices are square *)
    let validate_dim mat1 mat2 =
      let n = dim mat1 in
      if n = dim mat2 then n
      else raise Invalid_argument "Matrices must have matching dimension."

    let neg mat =
      let n = dim mat in
      init n (fun i j -> R.neg mat.(i).(j))

    let add mat1 mat2 =
      let n = validate_dim mat1 mat2 in
      init n (fun i j -> R.add mat1.(i).(j) mat2.(i).(j))

    let row i = Array.get i

    let col j = Array.map (Array.get j)

    let dot arr1 arr2 =
      let out = ref R.zero in
      Array.iter2 (fun x y -> out := R.add !out (R.mul x y)) arr1 arr2;
      !out

    let scalar_mul c arr = Array.map (R.mul c) arr

    let outer_prod arr1 arr2 =
      let n = validate_dim arr1 arr2 in
      init n (fun i j -> R.mul arr1.(i) arr2.(j))

    (* Implement a faster matrix multiplication algorithm at some point *)
    let mul mat1 mat2 =
      let n = validate_dim mat1 mat2 in
      init n (fun i j -> dot (row i mat1) (col j mat2))

    let tr mat =
      let diag = Array.mapi (fun i row -> row.(i)) mat in
      fold_left R.add R.zero diag

    let tensor mat1 mat2 =
      let d1 = dim mat1 in
      let d2 = dim mat2 in
      let out = Array.init (R.mul d1 d2) (fun _ -> Array.make (R.mul d1 d2) R.zero)
      for i1 = 0 to d1 - 1 do
        for j1 = 0 to d1 - 1 do
          for i2 = 0 to d2 - 1 do
            for j2 = 0 to d2 - 1 do
              out.(i1 * d2 + i2).(j1 * d2 + j2) <- R.mul mat1.(i1).(j1) mat2.(i2).(j2)
            done
          done
        done
      done

    let (+) = add
    let (-) = neg
    let ( * ) = mul

  end