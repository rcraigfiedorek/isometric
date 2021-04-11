

let array_find arr pred =
  n = ref 0;
  while n < Array.length arr && not (pred arr.(!n)) do n := !n + 1 done;
  if n = Array.length then raise Not_found else n