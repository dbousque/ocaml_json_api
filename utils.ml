

let list_merge lst1 lst2 =
  let rec _list_merge lst1 lst2 acc =
    match lst1,lst2 with
    | [],_ -> acc
    | _,[] -> acc
    | h1 :: tl1, h2 :: tl2 -> _list_merge tl1 tl2 ((h1, h2) :: acc)
  in
  List.rev (_list_merge lst1 lst2 [])