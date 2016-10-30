module Math = struct

  let rec fact n =
    if n = 0 then 1
    else n * (fact (n - 1))
  ;;

  let rec pow x n =
    if n = 0 then 1
    else x * (pow x (n - 1))
  ;;

end

let ( **. ) = Pervasives.( ** );;
let ( ** ) = Math.pow;;
let ( !! ) = Math.fact;;


module Seq = struct

  type 'a seq =
    | Empty
    | Element of 'a * (unit -> 'a seq)
  ;;

  let empty = Empty;;

  let rec unfold f state =
    match f state with
    | None -> Empty
    | Some(e, state') -> Element(e, fun () -> unfold f state')
  ;;

  let of_list = function
    | [] -> Empty
    | lst -> unfold (function [] -> None | hd :: tl -> Some (hd, tl)) lst
  ;;

	let range s e =
		unfold (function n when n <= e -> Some (n, n + 1) | _ -> None) s
	;;

  let to_list =
    let rec to_list lst = function
      | Empty -> lst |> List.rev
      | Element (e, seq) -> to_list (e :: lst) (seq ())
    in
    to_list []
  ;;

	let rec flatten = function
		| Empty -> Empty
		| Element (seq, next) ->
			let rec unwind = function
			| Empty -> flatten (next ())
			| Element (e, next') -> Element (e, fun () -> unwind (next' ()))
			in unwind seq
	;;
  
  let hd = function
    | Empty -> failwith "hd - empty sequence"
    | Element(e, _) -> e
  ;;

  let tl = function
    | Empty -> failwith "tl - empty sequence"
    | Element(_, seq) -> seq ()
  ;;

  let rec iter f = function
    | Empty -> ()
    | Element(e, seq) -> f e; iter f (seq ())
  ;;

  let rec map f = function
    | Empty -> Empty
    | Element(e, seq) -> Element(f e, fun () -> map f (seq ()))
  ;;

  let rec filter p = function
    | Empty -> Empty
    | Element(e, seq) ->
      if p e then Element(e, fun () -> filter p (seq ()))
      else filter p (seq ())
  ;;

  let rec find p = function
    | Empty -> None
    | Element(e, seq) ->
      if p e then Some(e)
      else find p (seq ())
  ;;

  let rec take_while p = function
    | Empty -> Empty
    | Element(e, seq) ->
      if p e then Element(e, fun () -> take_while p (seq ()))
      else Empty
  ;;

  let rec take n = function
    | Empty -> Empty
    | Element(e, seq) ->
      if n = 0 then Empty
      else Element(e, fun () -> take (n - 1) (seq ()))
  ;;

  let rec skip_while p = function
    | Empty -> Empty
    | Element(e, seq) as elm ->
      if not (p e) then elm
      else skip_while p (seq ())
  ;;

  let rec skip n = function
    | Empty -> Empty
    | Element(e, seq) as elm ->
      if n = 0 then elm
      else skip (n - 1) (seq ())
  ;;

  let min seq =
    let min = ref (hd seq) in
    let minf x = if x < !min then min := x in
    iter minf (seq |> tl);
    !min
  ;;

  let max seq =
    let max = ref (hd seq) in
    let maxf x = if x > !max then max := x in
    iter maxf (seq |> tl);
    !max
  ;;

end


module File = struct

  let open_in filename fn =
    let ch = open_in filename in
    try
      let res = fn ch in
      close_in ch;
      res
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

  let from_list chars =
    String.init (List.length chars) (fun i -> List.nth chars i)
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
      try Some (f (Stream.next stream))
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

  let max stream =
    let rec search mx =
      match Stream.peek stream with
      | None    -> mx
      | Some(x) ->
        Stream.junk stream; search (Pervasives.max mx x)
    in
    search (Stream.next stream)
  ;;

  let maxf f stream =
    let maxf a b = if f a > f b then a else b in
    let rec search mx =
      match Stream.peek stream with
      | None    -> mx
      | Some(x) -> Stream.junk stream; search (maxf mx x)
    in
    search (Stream.next stream)
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

  let collect p stream =
    let rec next bag i =
      try
        let item = Stream.next stream in
        if p item then next (item :: bag) i
        else
          match bag with
          | [] -> next bag i
          | _ -> Some(List.rev bag)
      with Stream.Failure -> None
    in
    Stream.from (next [])
  ;;

  let to_list stream =
    let list = ref [] in
    Stream.iter (fun v -> list := !list @ [v]) stream;
    !list
  ;;

  let to_array stream =
    stream |> to_list |> Array.of_list
  ;;

  let skip n stream =
    for _ = 1 to n do
      Stream.junk stream
    done;
    stream
  ;;

  let take n stream =
    let rec take i =
      if i = n then None
      else Some(Stream.next stream)
    in
    Stream.from take
  ;;

  let concat s1 s2 =
    let rec concat i =
      begin match Stream.peek s1 with
      | None ->
        begin match Stream.peek s2 with
        | None -> None
        | Some _ -> Some(Stream.next s2)
        end
      | Some _ -> Some(Stream.next s1)
      end
    in
    Stream.from concat
  ;;

  let flatten streams =
    let lst = ref [] in
    let rec flatten i =
      match !lst with
      | [] ->
        begin match Stream.peek streams with
        | None -> None
        | Some _ -> lst := Stream.next streams; flatten i
        end
      | h :: t -> lst := t; Some(h)
    in
    Stream.from flatten
  ;;

end


module List = struct
  include List

  let keep = List.filter
  ;;

  let filteri p items =
    let rec search i matching = function
      | [] -> matching
      | x :: items' ->
        if p i x then search (i + 1) (x :: matching) items'
        else search (i + 1) matching items'
    in
    search 0 [] items |> List.rev
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
    let rec search = function
      | [] -> failwith "a non-empty list is required"
      | [x] -> x
      | x :: tail -> Pervasives.max x (search tail)
    in
    search items
  ;;

  let minf f xs =
    let minf a b = if f a < f b then a else b in
    let rec search mx = function
      | [] -> mx
      | x :: xs -> search (minf mx x) xs
    in
    match xs with
    | [] -> failwith "a non-empty list is required"
    | x :: xs -> search x xs
  ;;

  let maxf f xs =
    let maxf a b = if f a > f b then a else b in
    let rec search mx = function
      | [] -> mx
      | x :: xs -> search (maxf mx x) xs
    in
    match xs with
    | [] -> failwith "a non-empty list is required"
    | x :: xs -> search x xs
  ;;

  let of_string = String.to_list
  ;;

  let skip n items =
    let rec build n s =
      if n = 0 then s
      else build (n-1) (List.tl s)
    in
    build n items
  ;;

  let reduce f init items =
    let rec build acc = function
      | [] -> acc
      | h :: t -> build (f h acc) t
    in
    build init items
  ;;

  let rec remove xs x =
    match xs with
    | [] -> []
    | x' :: xs' ->
      if x = x' then xs'
      else x' :: (remove xs' x)
  ;;

  let rol = function
    | [] -> []
    | x :: xs ->
      let rec rol = function
        | [] -> [x]
        | hd :: tl -> hd :: rol tl
      in
      rol xs
  ;;

end

let ( -- ) = List.remove;;

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


module Map = struct

  module type S = sig
    include Map.S
    val keys: 'a t -> key list
    val from_list: (key * 'a) list -> 'a t
    val from_stream: (key * 'a) Stream.t -> 'a t
  end

  module Make (Ord : Map.OrderedType) : S with type key = Ord.t = struct
    module Map = Map.Make(Ord)
    include Map

    let keys m = Map.fold (fun k _ acc -> k :: acc) m [];;

    let from_list l =
      let rec build l m =
        match l with
        | [] -> m
        | (k,v) :: t -> build t (Map.add k v m)
      in
      build l (Map.empty)
    ;;

    let from_stream s =
      let rec build s m =
        try
          let (k,v) = Stream.next s in
          build s (Map.add k v m)
        with Stream.Failure -> m
      in
      build s (Map.empty)
    ;;
  end

end

module Json = struct
  module Lexer = struct
    type token =
      | String of string
      | Number of float
      | True | False
      | Null
      | BraceL | BraceR
      | BracketL | BracketR
      | Colon
    ;;

    let tokenize chars =
      let parse_string chars =
        let rec collect s =
          match Stream.npeek 2 chars with
          | [] -> failwith "Unexpected end of json string data"
          | ['\\'; c] ->
            chars |> Stream.skip 2 |> ignore;
            let ec = match c with
              | '"' -> '"'
              | '\\' -> '\\'
              | '/' -> '/'
              | 'b' -> '\b'
              | 'f' -> '\x0c'
              | 'n' -> '\n'
              | 'r' -> '\r'
              | 't' -> '\t'
              | _ -> failwith (Printf.sprintf "Unxpected escape sequence: \\%c" c)
            in
            collect (ec :: s)
          | '"' :: _ -> chars |> Stream.skip 1 |> ignore; s |> List.rev |> String.from_list
          | c :: _ -> chars |> Stream.skip 1 |> ignore; collect (c :: s)
        in
        collect []
      in
      let parse_number chars =
        let rec collect digits chars =
          match Stream.peek chars with
          | None -> digits |> List.rev |> String.from_list |> float_of_string
          | Some(d) ->
            match d with
            | '0'..'9' | '-' | '+' | '.' | 'e' | 'E' -> chars |> Stream.skip 1 |> collect (d :: digits)
            | _ -> digits |> List.rev |> String.from_list |> float_of_string
        in
        collect [] chars
      in
      let rec tokenize tokens chars =
        match Stream.peek chars with
        | None -> tokens |> List.rev
        | Some(c) ->
          match c with
          | '"' -> tokenize (String(chars |> Stream.skip 1 |> parse_string) :: tokens) chars
          | ',' | ' ' | '\t' | '\n' -> chars |> Stream.skip 1 |> tokenize tokens
          | 't' -> chars |> Stream.skip 4 |> tokenize (True :: tokens)
          | 'f' -> chars |> Stream.skip 5 |> tokenize (False :: tokens)
          | 'n' -> chars |> Stream.skip 4 |> tokenize (Null :: tokens)
          | '0'..'9' | '-' -> tokenize (Number(chars |> parse_number) :: tokens) chars
          | '[' -> chars |> Stream.skip 1 |> tokenize (BracketL :: tokens)
          | ']' -> chars |> Stream.skip 1 |> tokenize (BracketR :: tokens)
          | '{' -> chars |> Stream.skip 1 |> tokenize (BraceL :: tokens)
          | '}' -> chars |> Stream.skip 1 |> tokenize (BraceR :: tokens)
          | ':' -> chars |> Stream.skip 1 |> tokenize (Colon :: tokens)
          | _ -> failwith (Printf.sprintf "Unexpected char: %c" c)
      in
      chars |> tokenize [] |> Stream.of_list
    ;;

    let to_string = function
      | String(str) -> Printf.sprintf "\"%s\"" str
      | Number(num) -> Printf.sprintf "%g" num
      | True -> "true"
      | False -> "false"
      | Null -> "null"
      | Colon -> ":"
      | BraceL -> "{"
      | BraceR -> "}"
      | BracketL -> "["
      | BracketR -> "]"
    ;;
  end

  type json =
    | Object of (string * json) list
    | Array of json list
    | String of string
    | Number of float
    | True
    | False
    | Null
  ;;

  let parse_stream stream =
    let rec parse obj arr tokens =
      match tokens |> Stream.npeek 3 with
      | [Lexer.String(key); Lexer.Colon; token] ->
        Stream.skip 3 tokens |> ignore;
        begin match token with
          | Lexer.String(str) -> tokens |> parse ((key, String(str)) :: obj) []
          | Lexer.Number(num) -> tokens |> parse ((key, Number(num)) :: obj) []
          | Lexer.True        -> tokens |> parse ((key, True) :: obj) []
          | Lexer.False       -> tokens |> parse ((key, False) :: obj) []
          | Lexer.Null        -> tokens |> parse ((key, Null) :: obj) []
          | Lexer.BraceL | Lexer.BracketL ->
            let element = (key, tokens |> parse [] []) in
            begin match tokens |> Stream.peek with
              | Some(_) -> tokens |> parse (element :: obj) []
              | None    -> Object(element :: obj |> List.rev)
            end
          | _ -> failwith (Printf.sprintf "unexpected token encountered: %s" (Lexer.to_string token))
        end
      | token :: _ ->
        Stream.skip 1 tokens |> ignore;
        begin match token with
          | Lexer.String(str) -> tokens |> parse [] (String(str) :: arr)
          | Lexer.Number(num) -> tokens |> parse [] (Number(num) :: arr)
          | Lexer.True        -> tokens |> parse [] (True :: arr)
          | Lexer.False       -> tokens |> parse [] (False :: arr)
          | Lexer.Null        -> tokens |> parse [] (Null :: arr)
          | Lexer.BraceL | Lexer.BracketL ->
            let element = tokens |> parse [] [] in
            begin match tokens |> Stream.peek with
              | Some(_) -> tokens |> parse [] (element :: arr)
              | None    -> element
            end
          | Lexer.BraceR      -> Object(obj |> List.rev)
          | Lexer.BracketR    -> Array(arr |> List.rev)
          | _ -> failwith (Printf.sprintf "unexpected token encountered: %s" (Lexer.to_string token))
        end
      | [] -> failwith "unexpected end of json structure"
    in
    stream |> Lexer.tokenize |> parse [] []
  ;;

  let parse str = str |> Stream.of_string |> parse_stream;;

  let rec to_string json =
    let join f arr =
      let rec join acc = function
        | [] -> acc
        | h :: t -> join (acc ^ "," ^ f h) t
      in
      match arr with
      | [] -> ""
      | [h] -> f h
      | h :: t -> join (f h) t
    in
    match json with
    | Array(arr) -> arr |> List.map to_string |> join (fun s -> s) |> Printf.sprintf "[%s]"
    | Object(obj) -> obj |> List.map (fun (k, o) -> (k, to_string o)) |> join (fun (k, v) -> Printf.sprintf "\"%s\":%s" k v) |> Printf.sprintf "{%s}"
    | String(str) -> Printf.sprintf "\"%s\"" str
    | Number(num) -> Printf.sprintf "%g" num
    | True -> "true"
    | False -> "false"
    | Null -> "null"
  ;;


  module Tests = struct
    let expect p m =
      let actual, expected = p () in
      if actual = expected then Printf.printf "passed: %s\n" m
      else Printf.printf "failed: %s\n expected: %s\n      was: %s\n" m (expected |> to_string) (actual |> to_string)
    ;;

    let to_string () =
      "[\"a\",\"b\",true,false,\"c\",null,42,[1,2,3]]"
      |> parse
      |> to_string
      |> print_endline
    ;;

    let all () =
      to_string ();
      "array: empty" |> expect (fun () -> "[]" |> parse, Array([]));
      "array: with values" |> expect (fun () -> "[1,true,null,[]]" |> parse, Array([Number(1.); True; Null; Array([])]));
      "array: with an array" |> expect (fun () -> "[[1],[[2]],3]" |> parse, Array([Array([Number(1.)]); Array([Array([Number(2.)])]); Number(3.)]));
      "array: with an object" |> expect (fun () -> "[{\"key\":42}]" |> parse, Array([Object(["key", Number(42.)])]));
      "object: object" |> expect (fun () -> "{}" |> parse, Object([]));
      "object: with a key" |> expect (fun () -> "{\"key\":42}" |> parse, Object(["key",Number(42.)]));
      "object: with an array" |> expect (fun () -> "{\"key\":[42]}" |> parse, Object(["key", Array([Number(42.)])]));
      "object: with an object" |> expect (fun () -> "{\"key\":{\"nested\":[42]}}" |> parse, Object(["key", Object(["nested", Array([Number(42.)])])]));
      "string: with escapes" |> expect (fun () -> "[\"a\\\"b\\\"c\"]" |> parse, Array([String("a\"b\"c")]));
    ;;
  end

end
