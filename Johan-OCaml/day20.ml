exception HouseFound of int * int;;

let find_first_house delivery_strategy_for =
  let number_of_presents = 36_000_000 in
  let found_house h p : unit = raise (HouseFound (h, p)) in
  let street = Array.make number_of_presents 0 in
  try
    for elf = 1 to number_of_presents do
      delivery_strategy_for elf street;
      let presents = street.(elf - 1) in
      if presents >= number_of_presents then found_house elf presents
    done;
    (number_of_presents, number_of_presents)
  with
    | HouseFound (h, p) -> (h, p)
;;

let deliver_to_all presents_for stride street =
  let house = ref (stride - 1)
  and last_house = Array.length street in
  while !house < last_house do
    street.(!house) <- street.(!house) + (presents_for stride);
    house := !house + stride;
  done
;;

let deliver_to_first_50 presents_for stride street =
  let house = ref (stride - 1)
  and last_house = Array.length street
  and deliveries = ref 0 in
  while !house < last_house && !deliveries < 50 do
    street.(!house) <- street.(!house) + (presents_for stride);
    house := !house + stride;
    deliveries := !deliveries + 1;
  done
;;


let part1 () =
  let ten = ( * ) 10 in
  find_first_house (deliver_to_all ten)
  |> (fun (h, p) -> Printf.printf "part1: %d presents for house #%d\n%!" p h)
;;

let part2 () =
  let eleven = ( * ) 11 in
  find_first_house (deliver_to_first_50 eleven)
  |> (fun (h, p) -> Printf.printf "part2: %d presents for house #%d\n%!" p h)
;;

part1 ();;
part2 ();;
