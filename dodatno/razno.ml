(* List fold right and left *)

let rec fold_right f lst init =
    match lst with
    | [] -> init
    | x::xs -> f x (fold_right f xs init)

let fold_left f lst =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (f acc x) xs
    in aux [] lst
