open Bigarray
open Core
open Alcotest
module Tcompact = Thrift_compact

let buf = Iobuf.create ~len:(1 lsl 29)

let gen_mixed_a_char n =
  let a = Array1.create Bigarray.int32 c_layout n in
  let a' = Array1.create Bigarray.char c_layout n in
  let len = Array1.dim a in
  let int32_of_char c = Char.to_int c |> Int32.of_int_exn in
  let rec loop i =
    let rem = len - i in
    (* Format.printf "loop %d " rem ; *)
    match Random.bool () with
    | true ->
        let len = 8 + Random.int 100 in
        let x = Random.char () in
        let x32 = int32_of_char x in
        let len = min len rem in
        for j = 0 to -1 + len do
          Array1.set a (i + j) x32 ;
          Array1.set a' (i + j) x
        done ;
        if rem > 0 then loop (i + len)
    | false ->
        let len = Random.int 100 in
        let len = min len rem in
        for j = 0 to -1 + len do
          let x = Random.char () in
          Array1.set a (i + j) (int32_of_char x) ;
          Array1.set a' (i + j) x
        done ;
        if rem > 0 then loop (i + len) in
  loop 0 ; (a, a')

let gen_mixed_a ~bw n =
  let maxv = (1 lsl bw) - 1 in
  let a = Array1.create Bigarray.int c_layout n in
  let len = Array1.dim a in
  let rec loop i =
    let rem = len - i in
    (* Format.printf "loop %d " rem ; *)
    match Random.bool () with
    | true ->
        let len = 8 + Random.int 100 in
        let xxxx = Random.int maxv in
        let len = min len rem in
        for j = 0 to -1 + len do
          Array1.set a (i + j) xxxx
        done ;
        if rem > 0 then loop (i + len)
    | false ->
        let len = Random.int 100 in
        let len = min len rem in
        for j = 0 to -1 + len do
          Array1.set a (i + j) (Random.int maxv)
        done ;
        if rem > 0 then loop (i + len) in
  loop 0 ; a

let gen_mixed_aa ~bw n =
  let maxv = (1 lsl bw) - 1 in
  let a = Array.create ~len:n 0 in
  let len = Array.length a in
  let rec loop i =
    let rem = len - i in
    (* Format.printf "loop %d " rem ; *)
    match Random.bool () with
    | true ->
        let len = 8 + Random.int 100 in
        let xxxx = Random.int maxv in
        let len = min len rem in
        for j = 0 to -1 + len do
          Array.set a (i + j) xxxx
        done ;
        if rem > 0 then loop (i + len)
    | false ->
        let len = 8 * (1 + Random.int 10) in
        let len = min len rem in
        for j = 0 to -1 + len do
          Array.set a (i + j) (Random.int maxv)
        done ;
        if rem > 0 then loop (i + len) in
  loop 0 ; a

let gensec (type t) (module M : Int_intf.S with type t = t) (a : t) (b : t) =
  let open M in
  let open Seq in
  let rec loop x () =
    if x = b then Cons (x, empty) else Cons (x, loop (M.succ x)) in
  loop a

let gensec32 = gensec (module Int32)
let gensec64 = gensec (module Int64)

let genrd32 n =
  let open Seq in
  let rd () =
    let x = Random.int32 Int32.max_value in
    if Random.bool () then Int32.neg x else x in
  let rec loop n () =
    if n = 0 then Cons (rd (), empty) else Cons (rd (), loop (pred n)) in
  loop n

let genrd64 n =
  let open Seq in
  let rd () =
    let x = Random.int64 Int64.max_value in
    if Random.bool () then Int64.neg x else x in
  let rec loop n () =
    if n = 0 then Cons (rd (), empty) else Cons (rd (), loop (pred n)) in
  loop n

let uleb128_32 seq =
  test_case "uleb128_32" `Quick (fun () ->
      Seq.iter
        (fun x ->
          Iobuf.reset buf ;
          Tcompact.write_int32 buf x ;
          Iobuf.flip_lo buf ;
          let x' = Thrift_compact.read_int32 buf in
          check int32 (Int32.to_string x) x x' )
        seq )

let uleb128_64 seq =
  test_case "uleb128_64" `Quick (fun () ->
      Seq.iter
        (fun x ->
          Iobuf.reset buf ;
          Tcompact.write_int64 buf x ;
          Iobuf.flip_lo buf ;
          let x' = Tcompact.read_int64 buf in
          check int64 (Int64.to_string x) x x' )
        seq )

let rdtrip testable e seq =
  test_case "uleb128_64" `Quick (fun () ->
      Seq.iter
        (fun x ->
          Iobuf.reset buf ;
          Tcompact.write e x buf ;
          Iobuf.flip_lo buf ;
          let x' = Tcompact.read e buf in
          check testable "" x x' )
        seq )

let int_list xs = rdtrip (array int32) Thrift.(list int32) (Caml.List.to_seq xs)

let int_list_in_struct xs =
  rdtrip (array int32)
    Thrift.(obj1 (req 1 (list int32) "x"))
    (Caml.Array.to_seq xs)

let uleb128_32 = [uleb128_32 (gensec32 0l 128l); uleb128_32 (genrd32 10000)]
let uleb128_64 = [uleb128_64 (gensec64 0L 128L); uleb128_64 (genrd64 10000)]
let int_list xs = [int_list xs]
let int_list_in_struct xs = [int_list_in_struct xs]

let rdtrip testable e xs =
  let open Tcompact in
  Iobuf.reset buf ;
  Array.iter xs ~f:(fun x -> write e x buf) ;
  Iobuf.flip_lo buf ;
  Format.printf "%S@." (Iobuf.to_string buf) ;
  Array.iter xs ~f:(fun x ->
      let x' = read e buf in
      check testable "" x x' )

let range f n = Array.init n ~f:(fun i -> f i)

let seq_of_a1 a =
  Seq.unfold
    (fun i -> if i < Array1.dim a then Some (a.{i}, succ i) else None)
    0

let a1_testable ~pp ~compare =
  testable
    (fun ppf a ->
      Format.pp_print_seq ~pp_sep:Format.pp_print_space pp ppf (seq_of_a1 a) )
    (fun x y ->
      try
        Array1.dim x = Array1.dim y
        &&
        ( for i = 0 to Array1.dim x - 1 do
            match compare x.{i} y.{i} with 0 -> () | _ -> raise Exit
          done ;
          true )
      with Exit -> false )

let rg n start step = Array.init n ~f:(fun i -> start + (i * step))

let rgf n start step =
  Array.init n ~f:(fun i -> start +. (Float.of_int i *. step))

let tcompact =
  [ test_case "int8" `Quick (fun () ->
        rdtrip int Thrift.int8 (range (fun x -> x - (1 lsl 7)) (1 lsl 8)) );
    test_case "int16" `Quick (fun () ->
        rdtrip int Thrift.int16 (range (fun x -> x - (1 lsl 15)) (1 lsl 16)) );
    test_case "obj1" `Quick (fun () ->
        rdtrip int32 Thrift.(obj1 (req 1 int32 "x")) [|1l; 2l; 3l|] );
    test_case "int list" `Quick (fun () ->
        rdtrip (array int32) Thrift.(list int32) [|[|1l; 2l; 3l|]|] );
    test_case "obj1 list" `Quick (fun () ->
        rdtrip (array int32)
          Thrift.(list (obj1 (req 1 int32 "x")))
          [|[|1l; 2l; 3l|]|] ) ]

let () =
  run "thrift"
    [ ("uleb128_32", uleb128_32); ("uleb128_64", uleb128_64);
      ("int_list", int_list [[|1l; 2l|]]);
      ("int_list_in_struct", int_list [[|1l; 2l|]]); ("tcompact", tcompact) ]
