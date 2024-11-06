let cch (amount:int) (denominations: int list): int list list =
  let rec helper a d acc =
    if a < 0 then failwith "Invalid Input"
    else if a = 0 then [acc]
    else match d with
    | [] -> [[]]
    | h::t ->
      if a >= h then (helper (a - h) d (h :: acc)) @ (helper a t acc)
      else helper a t acc
  in helper amount denominations [] |> List.filter (fun x -> x <> [])

let rec stack2list s =
  match Stack.pop_opt s with
  | None -> []
  | Some v -> v :: stack2list s

let cchstack (amount:int) (denominations:int list): int list list =
  let res = Stack.create () in
  let rec cch a d acc =
    if a < 0 then failwith "Invalid Input"
    else if a = 0 then Stack.push acc res
    else match d with
    | [] -> ()
    | h::t ->
      if a >= h then (cch (a - h) d (h :: acc); cch a t acc)
      else cch a t acc
  in cch amount denominations []; stack2list res

let cchstack2 amount denominations =
  let res: (int * int) list Stack.t = Stack.create () in
  let rec cch a d acc =
    if a < 0 then failwith "Invalid Input"
    else if a = 0 then Stack.push acc res
    else
      match d with
      | [] -> ()
      | h::t ->
        let q = a / h in
        for i = 1 to q do
          cch (a - (h * i)) t ((h, i) :: acc)
        done;
        cch a t acc
  in cch amount denominations []; stack2list res

let scch amount denominations =
  let rec helper a d acc =
    if a < 0 then failwith "Invalid Input"
    else if a = 0 then acc
    else
      match d with
      | [] -> [] (* solution not possible *)
      | h::t ->
        if a >= h then helper (a mod h) t ((h, a / h) :: acc)
        else helper a t acc
  in helper amount denominations []

type 'a writer = { value : 'a; log : string list }

let return a = { value = a; log = [] }

let bind m f =
  let { value = a; log = l1 } = m in
  let { value = b; log = l2 } = f a in
  { value = b; log = l1 @ l2 }

let tell msg = { value = (); log = [msg] }

(* Modified time function using the writer monad *)
let time_monad f x y =
  let start = Sys.time () in
  let stop = ignore (f x y); Sys.time () in
  { value = stop -. start; log = ["Execution time: " ^ string_of_float (stop -. start) ^ " seconds"] }

(* Test cases *)
let () =
  let test_cch () =
    cch 42 [1; 2; 5; 10; 20; 50] |> ignore;
    tell "cch function executed"
  in
  let test_cchstack () =
    cchstack 42 [1; 2; 5; 10; 20; 50] |> ignore;
    tell "cchstack function executed"
  in
  let test_cchstack2 () =
    cchstack2 42 [1; 2; 5; 10; 20; 50] |> ignore;
    tell "cchstack2 function executed"
  in

  let result_cch = bind (time_monad test_cch ()) (fun () -> return ()) in
  let result_cchstack = bind (time_monad test_cchstack ()) (fun _ -> return ()) in
  let result_cchstack2 = bind (time_monad test_cchstack2 ()) (fun _ -> return ()) in

  (* Printing results *)
  Printf.printf "Result of cch: %f seconds\n" result_cch.value;
  List.iter print_endline result_cch.log;

  Printf.printf "Result of cchstack: %f seconds\n" result_cchstack.value;
  List.iter print_endline result_cchstack.log;

  Printf.printf "Result of cchstack2: %f seconds\n" result_cchstack2.value;
  List.iter print_endline result_cchstack2.log