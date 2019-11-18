(* -------- 1 -------- *)


(* -------- 2 -------- *)
let rec urejen list = 
    match list with
    | x :: y :: xs -> 
        if x <= y then
            urejen y :: xs
        else 
            false
    | _ -> true
(* -------- 3 -------- *)

(* -------- 4 -------- *)

(* -------- 5 -------- *)
type priority = Top | Group of int
type status = Staff | Passenger of priority
type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)

(* -------- 7 -------- *)
