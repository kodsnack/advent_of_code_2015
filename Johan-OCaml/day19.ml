#use "./extensions.ml";;
#load "str.cma";;

let rec interleave x = function
  | [] -> []
  | [hd] -> [hd]
  | hd :: tl -> hd :: x :: interleave x tl
;;

let replace source (s,r) =
  let rec replace hd tl =
    match tl with
    | [] -> []
    | x :: tl' ->
      begin match hd with
      | [] -> replace (x :: hd) tl'
      | _ -> ((interleave s (hd |> List.rev)) @ r :: interleave s tl) :: replace (x :: hd) tl'
      end
  in
  replace [] (Str.split_delim (Str.regexp s) source)
  |> List.map (String.concat "")
;;

let make_distinct_molecules initial rules =
  rules
  |> List.map (replace initial)
  |> List.flatten
  |> List.sort_uniq compare
;;


let test_rules = [
  ("H","HO");
  ("H","OH");
  ("O","HH");
];;

let test1 () =
  let res1 = test_rules |> make_distinct_molecules "HOH" in
  assert ((List.length res1) = 4);
  res1 |> List.iter (Printf.printf "'%s'\n");

  let res2 = test_rules |> make_distinct_molecules "HOHOHO" in
  assert ((List.length res2) = 7);
;;

let input_rules = [
  ("Al","ThF");
  ("Al","ThRnFAr");
  ("B","BCa");
  ("B","TiB");
  ("B","TiRnFAr");
  ("Ca","CaCa");
  ("Ca","PB");
  ("Ca","PRnFAr");
  ("Ca","SiRnFYFAr");
  ("Ca","SiRnMgAr");
  ("Ca","SiTh");
  ("F","CaF");
  ("F","PMg");
  ("F","SiAl");
  ("H","CRnAlAr");
  ("H","CRnFYFYFAr");
  ("H","CRnFYMgAr");
  ("H","CRnMgYFAr");
  ("H","HCa");
  ("H","NRnFYFAr");
  ("H","NRnMgAr");
  ("H","NTh");
  ("H","OB");
  ("H","ORnFAr");
  ("Mg","BF");
  ("Mg","TiMg");
  ("N","CRnFAr");
  ("N","HSi");
  ("O","CRnFYFAr");
  ("O","CRnMgAr");
  ("O","HP");
  ("O","NRnFAr");
  ("O","OTi");
  ("P","CaP");
  ("P","PTi");
  ("P","SiRnFAr");
  ("Si","CaSi");
  ("Th","ThCa");
  ("Ti","BP");
  ("Ti","TiTi");
  ("e","HF");
  ("e","NAl");
  ("e","OMg");
];;
let input_initial = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl";;

let part1 () =
  input_rules
  |> make_distinct_molecules input_initial
  |> List.length
  |> Printf.printf "part1: %d distinct molecules possible\n%!"
;;

test1 ();;
part1 ();;
