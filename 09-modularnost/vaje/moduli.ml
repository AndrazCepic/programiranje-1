(* ========== Vaja 8: Moduli  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Definirajte signaturo [NAT], ki določa strukturo naravnih števil. Ima osnovni 
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov [int] tip.

 Opomba: Funkcije za pretvarjanje ponavadi poimenujemo [to_int] and [of_int],
 tako da skupaj z imenom modula dobimo ime [NAT.of_int], ki nam pove, da 
 pridobivamo naravno število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq   : t -> t -> bool
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mult : t -> t -> t

  val zero : t
  val one  : t
  
  val to_int : t -> int
  val of_int : int -> t
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo modula [Nat_int], ki zgradi modul s signaturo [NAT],
 kjer kot osnovni tip uporablja OCamlov tip [int].

 Namig: Dokler ne implementirate vse funkcij v [Nat_int] se bo OCaml pritoževal.
 Temu se lahko izognete tako, da funkcije, ki še niso napisane nadomestite z 
 [failwith "later"], vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct
  type t = int
  let eq x y = (x = y)
  let zero = 0
  let one = 1

  let add x y = x + y
  let sub x y = x - y
  let mult x y = x * y

  let to_int x = x
  let of_int x = x
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo [NAT], ki temelji na Peanovih aksiomih:
 https://en.wikipedia.org/wiki/Peano_axioms
   
 Osnovni tip modula definirajte kot vsotni tip, ki vsebuje konstruktor za ničlo
 in konstruktor za naslednika nekega naravnega števila.
 Večino funkcij lahko implementirate s pomočjo rekurzije. Naprimer, enakost
 števil [k] in [l] določimo s hkratno rekurzijo na [k] in [l], kjer je osnoven
 primer [Zero = Zero].

[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = 
    | Zero 
    | Succ of t

  let eq x y = (x = y)
  let zero = Zero
  let one = Succ(zero)

  let rec add x y =
    match x with
    | Zero -> y
    | Succ(z) -> Succ (add z y)

  let rec sub x y = 
    match x with
    | Zero -> Zero
    | Succ (n) -> 
      match y with 
      | Zero -> n
      | Succ (k) -> sub n k

  let rec mult x y = 
    match x with
    | Zero -> Zero
    | Succ (n) -> add (mult n y) y

  let of_int x = 
    let rec aux acc y =
      if y = 0 then acc
      else aux (Succ(acc)) (y-1)
    in aux Zero x

  let to_int x = 
    let rec aux acc = function
      | Zero -> acc
      | Succ (n) -> aux (acc + 1) n
    in aux 0 x

end

(*----------------------------------------------------------------------------*]
 V OCamlu lahko module podajamo kot argumente funkcij, z uporabo besede
 [module]. Funkcijo, ki sprejme modul torej definiramo kot

 # let f (module M : M_sig) = ...

 in ji podajamo argumente kot 

 # f (module M_implementation);;

 Funkcija [sum_nat_100] sprejme modul tipa [NAT] in z uporabo modula sešteje
 prvih 100 naravnih števil. Ker funkcija ne more vrniti rezultata tipa [NAT.t]
 (saj ne vemo, kateremu od modulov bo pripadal, torej je lahko [int] ali pa
  variantni tip) na koncu vrnemo rezultat tipa [int] z uporabo metode [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) = 
  let n = Nat.of_int 100 in
  let m = Nat.add n Nat.one
  in
  (Nat.to_int (Nat.mult n m)) / 2

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo modula kompleksnih števil.
 Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
 negacijo, konjugacijo, seštevanje in množenje. 
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  
  val eq : t -> t -> bool
  val neg : t -> t
  val conj : t -> t
  val add : t -> t -> t
  val mult : t -> t -> t

  val zero : t
  val one : t
  val i : t
end

(*----------------------------------------------------------------------------*]
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq {re=re1; im=im1} {re=re2; im=im2} = 
    re1 = re2 && im1 = im2

  let neg {re=re; im=im} = {re = -.re; im = -.im}
  let conj {re=re; im=im} = {re = re; im = -.im}
  let add  {re=re1; im=im1} {re=re2; im=im2} = 
    {re = re1 +. re2; im = im1 +. im2}
  let mult {re=re1; im=im1} {re=re2; im=im2} = 
    {re = re1 *. re2 -. im1 *. im2; im = re1 *. im2 +. re2 *. im1}

  let zero = {re = 0.0; im = 0.0}
  let one = {re = 1.0; im = 0.0}
  let i = {re = 0.0; im = 1.0} 

end

(*----------------------------------------------------------------------------*]
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument).
   
 Priporočilo: Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga 
 pustite za konec (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let eq {magn=magn1; arg=arg1} {magn=magn2; arg=arg2} =
    magn1 = magn2 && arg1 = arg2 
  let neg {magn=magn; arg=arg} = 
    if arg < pi then {magn=magn; arg = arg +. pi}
    else {magn=magn; arg = arg -. pi}
  let conj {magn=magn; arg=arg} = 
    if arg = 0.0 then {magn=magn; arg = 0.0}
    else {magn=magn; arg = 2.0*.pi -. arg}
  let mult {magn=magn1; arg=arg1} {magn=magn2; arg=arg2} =
    if arg1 +. arg2 < 2.0*.pi then 
      {magn=magn1 *. magn2; arg = arg1 +. arg2}
    else 
      {magn=magn1 *. magn2; arg = arg1 +. arg2 -. 2.0*.pi}
  let add {magn=magn1; arg=arg1} {magn=magn2; arg=arg2} =
    failwith "later"
  
  let zero = {magn = 0.0; arg = 0.0}
  let one = {magn = 1.0; arg = 0.0}
  let i = {magn = 1.0; arg = pi /. 2.0}

end

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Na vajah z iskalnimi drevesi smo definirali tip slovarjev 
 [('key, 'value) dict], ki je implementiral [dict_get], [dict_insert] in
 [print_dict]. Napišite primerno signaturo za slovarje [DICT] in naredite
 implementacijo modula z drevesi (kot na prejšnjih vajah). 
 
 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)

module type DICT = sig
  type ('a, 'b) dict
  
  val empty : ('a, 'b) dict
  val dict_get : 'a -> ('a, 'b) dict -> 'b option
  val dict_insert : 'a -> 'b -> ('a, 'b) dict -> ('a, 'b) dict
end

type 'a tree = 
  | Empty 
  | Node of 'a tree * 'a * 'a tree

let leaf a = Node (Empty, a, Empty)

module Dict : DICT = struct
  type ('a, 'b) dict = ('a * 'b) tree

  let empty = Empty 

  let print_dict (d : (string, int) dict) = 
    let rec aux = function
        | Empty -> ""
        | Node (l, (key, value), r) -> 
              (aux l) ^ (key ^ " : " ^ (string_of_int value) ^ "\n") ^ (aux r)
    in print_string (aux d)

  let rec dict_get key d = 
    match d with 
    | Empty -> None
    | Node (l, (k, v), r) ->
      if key = k then Some v
      else if key < k then dict_get key l
      else dict_get key r

  let rec dict_insert key value dict = 
     match dict with
     | Empty -> leaf (key, value)
     | Node (l, (k, v), r) -> 
          if key = k then 
               Node (l, (key, value), r)
          else if key < k then
               Node (dict_insert key value l, (k, v), r)
          else
               Node (l, (k, v), dict_insert key value  r)
end

(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) (list : string list) =
  let rec aux1 (d : (string, int) Dict.dict) = function
    | [] -> d
    | x::xs -> 
      match Dict.dict_get x d with
      | None ->
        let rec aux2 c = function
          | [] -> c
          | y::ys -> 
            if y = x then aux2 (c + 1) ys
            else aux2 c ys
        in 
        let v = aux2 0 list
        in aux1 (Dict.dict_insert x v d) xs
      | Some(_) -> aux1 d xs
  in 
  Dict.print_dict (aux1 Dict.empty list)