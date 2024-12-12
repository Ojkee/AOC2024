let hor content pos text stride =
  if (pos mod stride) + String.length text > stride then 0
  else
    let c = String.starts_with ~prefix:text content in
    Bool.to_int c

let rec ver0 content pos text i stride =
  if i >= String.length text then 1
  else if String.length content < stride * (i + 1) then 0
  else if content.[i * stride] = text.[i] then
    ver0 content pos text (i + 1) stride
  else 0

let rec ver1 content pos text i stride =
  if i >= String.length text then 1
  else if String.length content < (stride + 1) * (i + 1) then 0
  else if content.[i * (stride + 1)] = text.[i] then
    ver1 content pos text (i + 1) stride
  else 0

let rec find_inter content pos stride acc =
  if String.length content < stride then acc
  else if content.[pos mod stride] = 'X' then (
    let h = hor content pos "XMAS" stride in
    let v0 = ver0 content pos "XMAS" 0 stride in
    let v1 = ver1 content pos "XMAS" 0 stride in
    Printf.printf "X POS: %d h = %d v0 = %d v1 = %d\n" pos h v0 v1;
    find_inter
      (String.sub content 1 (String.length content - 1))
      (pos + 1) stride
      (acc + h + v0 + v1))
  else if content.[pos mod stride] = 'S' then (
    let h = hor content pos "SAMX" stride in
    let v0 = ver0 content pos "SAMX" 0 stride in
    let v1 = ver1 content pos "SAMX" 0 stride in
    Printf.printf "S POS: %d h = %d v0 = %d v1 = %d\n" pos h v0 v1;
    find_inter
      (String.sub content 1 (String.length content - 1))
      (pos + 1) stride
      (acc + h + v0 + v1))
  else
    find_inter
      (String.sub content 1 (String.length content - 1))
      (pos + 1) stride acc

let part1 () =
  let ic = open_in "data/day4_test.txt" in
  let line_len = String.length (input_line ic) in
  seek_in ic 0;
  let content = In_channel.input_all ic in
  let content_no_newline = Str.(global_replace (regexp "\n") "" content) in
  close_in ic;
  let retVal = find_inter content_no_newline 0 line_len 0 in
  Printf.printf "Result: %d\n" retVal
