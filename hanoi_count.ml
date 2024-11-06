(*tower of hanoi*)

let rec hanoi_steps n ini itm fnl steps = 
  if n > 0 then (
    steps := !steps + 1;
    hanoi_steps (n - 1) ini fnl itm steps;
    (* Printf.printf "Move disk #%d from tower %c to tower %c\n" n ini fnl ; *)
    hanoi_steps (n - 1) itm ini fnl steps
  )

let hanoi n ini itm fnl =
  let steps = ref 0 in
  hanoi_steps n ini itm fnl steps;
  !steps

let t n = hanoi n 'A' 'B' 'C'

let l num_disks =
  Printf.printf "Number of steps to solve Hanoi Tower with %d disks: %d\n" num_disks (t num_disks)

(*linier*)