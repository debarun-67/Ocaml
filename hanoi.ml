let rec hanoi n ini itm fnl acc = 
  if n>0 then (
    hanoi (n-1) ini fnl itm (acc+1);
    Printf.printf "Move disk #%d from tower %c to tower %c %s\n" n ini fnl (string_of_int (acc+1));
    hanoi (n-1) itm ini fnl (acc+1)
  )
(* else print_endline (string_of_int acc) *)

let t n = hanoi n 'A' 'C' 'B' 0
(* 
let rec count n =
  if n=1 then 1
  else 2 * count (n-1) + 1

let rec count_optimised n acc = if n=1 then acc else count_optimised (n-1) (2 * acc +1);;

let count_opt n = count_optimised n 1;; *)

(* let rf = List.init 50 (fun x -> x+1);;

let result () = let lst1 = List.map (fun x -> count x) rf 
in let lst2 = List.map (fun x -> count_opt x) rf
in assert (lst1 = lst2);; *)