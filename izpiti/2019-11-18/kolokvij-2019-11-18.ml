(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = y == x*x

let pack3 x y z = (x, y, z)

let sum_if_not pred list =
    let rec aux acc pred list =
        match list with
        | [] -> acc
        | x :: xs -> 
            if pred x then
                aux acc pred xs
            else
                aux (x + acc) pred xs
    in aux 0 pred list

let apply f_list list =
    let rec aux acc f_list list =
        match list with
        | [] -> acc
        | x :: xs ->
            let rec aux1 acc1 f_list y =
                match f_list with
                | [] -> acc1
                | f :: fs -> aux1 ((f y) :: acc1) fs y
            in aux ((List.rev (aux1 [] f_list x))::acc) f_list xs
    in List.rev (aux [] f_list list)
(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = Predavanja | Vaje

type srecanje = {predmet: string; vrsta: vrsta_srecanja; trajanje: int}
type urnik = Urnik of (srecanje list) list

let vaje = {predmet= "Analiza 2a"; vrsta= Vaje; trajanje= 3}
let predavanje = {predmet= "Programiranje 1"; vrsta= Predavanja; trajanje= 2}

let urnik_profesor = [[{}]] (* Napisal bi seznam seznamov določenih srečanj v dnevu; Če je dan prazen seznam, potem je prost *))

let je_preobremenjen urnik_prof = 
    let rec aux urnik_prof =
        match urnik_prof with
        | [] -> true
        | x :: xs ->
            let aux1 acc1 dan =
                match dan with
                | [] -> acc1
                | {pr= pr; vr= vr; tr= tr} :: ys -> 
                    aux1 (acc1 + tr) ys
            in 
                if (aux1 0 x) > 4 then false
                else aux xs
    in aux urnik_prof

let bogastvo () = failwith "dopolni me"
