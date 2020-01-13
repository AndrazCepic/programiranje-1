(* 1. naloga *)

let razlika_kvadratov a b = (a + b) * (a + b) - (a * a + b * b)

let uporabi_na_paru f (a, b) = (f a, f b)

let rec ponovi_seznam n list =
    if n <= 0 then 
        []
    else
        list @ ponovi_seznam (n - 1) list

let razdeli list = 
    let rec aux (acc1, acc2) = function
        | [] -> (acc1, acc2)
        | x :: xs ->
            if x < 0 then
                aux (x :: acc1, acc2) xs
            else 
                aux (acc1, x :: acc2) xs
    in aux ([], []) list 


(* 2. naloga *)

type 'a tree = 
    | Empty
    | Node of 'a tree * 'a * 'a tree

let vecja_pot pot1 pot2 = 
    if List.length pot1 >= List.length pot2 then
        pot1
    else
        pot2

let rec padajoca ma = function
    | Empty -> []
    | Node (l, x, r) -> 
        if x >= ma then []
        else
            x :: (vecja_pot (padajoca x l) (padajoca x r))

let rec narascajoca mi = function
    | Empty -> []
    | Node (l, x, r) -> 
        if x <= mi then []
        else
            x :: (vecja_pot (narascajoca x l) (narascajoca x r))

let rec monotona_pot = function
    | Empty -> []
    | Node (levi, a, desni) ->
        let lpot = monotona_pot levi in 
        let dpot = monotona_pot desni in
        let lpot_c = (narascajoca a levi) @ [a] @ (padajoca a desni) in
        let dpot_c = (padajoca a levi) @ [a] @ (narascajoca a desni) in
        vecja_pot lpot (vecja_pot dpot (vecja_pot lpot_c dpot_c))

(* 3. naloga *)

type 'a veriga = 
    | Filter of ('a -> bool) * 'a list * 'a veriga
    | Ostalo of 'a list

let f1 a = a < 0
let f2 a = a < 10

let test = (f1, [], Filter (f2, [], Ostalo []))

let rec vstavi a = function
    | Ostalo list -> Ostalo a :: list
    | Filter (f, list, v) -> 
        if (f a) then Filter (f, a :: list, v)
        else Filter (f, list, vstavi a v)

let rec poisci a = function
    | Ostalo list -> List.mem a list
    | Filter (f, _ , v) -> 
        if f a then true
        else poisci a v
