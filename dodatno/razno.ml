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

(* functions we can implement using fold left and right *)

let length l = fold_left (fun a x -> a + 1) l
let rev l = fold_left (fun a x -> x::a) l
let map f l = fold_right (fun x a -> (f x) :: a) l []
let filter p l = fold_right (fun x a -> if p x then x::a else a) l []
