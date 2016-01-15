module File = struct

  let open_in filename fn =
    let ch = open_in filename in
    try
      fn ch;
      close_in ch
    with e ->
      close_in ch;
      raise e
  ;;

end

module String = struct
  include String

  let to_list s =
    let rec build n l =
      if n = 0 then l
      else build (n - 1) (s.[n - 1] :: l)
    in
    build (String.length s) []
  ;;

end


module Stream = struct
  include Stream

  let of_chars = Stream.of_channel
  ;;

  let of_lines ch =
    Stream.from (fun _ ->
      try Some (input_line ch)
      with End_of_file -> None
    )
  ;;

  let map f stream =
    let rec next i =
      try Some (f @@ Stream.next stream)
      with Stream.Failure -> None
    in
    Stream.from next
  ;;

  let find f stream =
    let rec search v =
      if f v then v
      else search @@ Stream.next stream
    in
    try
      search @@ Stream.next stream
    with Stream.Failure -> raise Not_found
  ;;

  let fold f init stream =
    let result = ref init in
    Stream.iter (fun x -> result := f x !result) stream;
    !result
  ;;

  let filter p stream =
    let rec next i =
      try
        let value = Stream.next stream in
        if p value then Some value else next i
      with Stream.Failure -> None
    in
    Stream.from next
  ;;

end

module List = struct
  include List

  let keep = List.filter
  ;;

  let min items =
    let rec search items =
      match items with
      | [] -> failwith "min requires a non-empty list"
      | [x] -> x
      | x :: tail -> min x @@ search tail
    in
    search items
  ;;

  let max items =
    let rec search items =
      match items with
      | [] -> failwith "max requires a non-empty list"
      | [x] -> x
      | x :: tail -> max x @@ search tail
    in
    search items
  ;;

  let of_string = String.to_list
  ;;
end

module Option = struct
  let value_of o =
    match o with
    | Some x -> x
    | None -> failwith "no value"
  ;;

  let is_some o =
    match o with
    | Some _ -> true
    | None -> false
  ;;

  let is_none o = not @@ is_some o
  ;;
end
