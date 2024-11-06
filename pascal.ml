let next_pline line =
  let rec next_pline_helper acc prev_line = match prev_line with
    | [] -> (1::acc) |> List.rev
    | hd::next::tl -> next_pline_helper ((hd + next)::acc) (next::tl)
    | _ -> (1::acc) |> List.rev
  in
  next_pline_helper [] (0::line);;

let pascal_lines n =
  let rec generate_lines n current_line acc =
    if n = 0 then
      List.rev acc
    else
      let next_line = next_pline current_line in
      generate_lines (n-1) next_line (current_line::acc)
  in
  generate_lines n [1] [];;

let int_list_to_string lst =
  List.fold_right (fun x acc -> string_of_int x ^ " " ^ acc) lst "";;

let int_list_list_to_string lst_lst =
  List.fold_right (fun lst acc -> int_list_to_string lst ^ "\n" ^ acc) lst_lst "";;

let print_pascal_triangle n =
  let triangle = pascal_lines n in
  let triangle_str = int_list_list_to_string triangle in
  print_endline triangle_str;;

let accumulate f n x  = let rec accumulator f n x acc =  if n = 0 then acc
  else accumulator f (n-1) (f x) (f x::acc) in accumulator f n x [];;

let accumulate_fold f n x =
  let base = [x] in
  let lst = List.init (n-1) (fun _ -> ()) in 
  List.fold_left (fun acc _-> (f (List.hd acc)) :: acc) base lst
;;

let nxt_pl lst  = let rec add pre acc =  function
[] -> acc
|h::[] -> (h+0)::(h+pre)::acc
|h::t ->  add h ((h+pre)::acc) t  in 
add 0 [] lst;;

let pascal_lines n  = List.rev ( accumulate_fold (nxt_pl) n [1]);;