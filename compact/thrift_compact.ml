open Core
open Thrift

module Int = struct
  include Int

  let zig x = shift_left x 1 lxor shift_right x 31
  let zag x = shift_right_logical x 1 lxor neg (x land one)
end

module Int32 = struct
  include Int32

  let zig x = shift_left x 1 lxor shift_right x 31
  let zag x = shift_right_logical x 1 lxor neg (x land one)
end

module Int64 = struct
  include Int64

  let zig x = shift_left x 1 lxor shift_right x 63
  let zag x = shift_right_logical x 1 lxor neg (x land one)
end

let uleb128_r (type t) (module M : Int_intf.S with type t = t) buf =
  let rec aux shift acc =
    let x = Iobuf.Consume.uint8 buf in
    let acc = M.(acc lor (M.(of_int_exn x land of_int_exn 0x7f) lsl shift)) in
    if x land 0x80 = 0 then acc else aux (shift + 7) acc in
  aux 0 M.zero

let uleb128_r_peak (type t) (module M : Int_intf.S with type t = t) buf ~pos =
  let rec aux shift acc pos =
    let x = Iobuf.Peek.uint8 buf ~pos in
    let acc = M.(acc lor (M.(of_int_exn x land of_int_exn 0x7f) lsl shift)) in
    if x land 0x80 = 0 then (succ pos, acc) else aux (shift + 7) acc (succ pos)
  in
  aux 0 M.zero pos

let uleb128_w (type t) (module M : Int_intf.S with type t = t) buf x =
  let nbits = if M.(x = zero) then 0 else M.num_bits - M.clz x in
  let ngroups =
    match (nbits mod 7, nbits / 7) with 0, 0 -> 1 | 0, n -> n | _, n -> n + 1
  in
  let rec loop shift n =
    match n with
    | 0 -> Iobuf.Fill.uint8_trunc buf M.(x lsr shift |> M.to_int_exn)
    | _ ->
        Iobuf.Fill.uint8_trunc buf
          ( 0x80
          lor (0x7f land M.((x lsr shift) land of_int_exn 0xff |> to_int_exn))
          ) ;
        loop (shift + 7) (pred n) in
  loop 0 (pred ngroups)

let rec to_int : type a. a t -> int = function
  | Conv (_, _, x) -> to_int x
  | Atom x -> int_of_atom x
  | List (false, _) -> 9
  | List (true, _) -> 10
  | Map _ | Nil -> 11
  | Field _ | Fields _ | Union _ -> 12

and int_of_atom : type a. a atom -> int = function
  | Bool -> 2
  | Int8 -> 3
  | Int16 -> 4
  | Int32 -> 5
  | Int64 -> 6
  | Int32e -> 5
  | Int64e -> 6
  | Double -> 7
  | Bytes -> 8

let to_int_v : type a. a t -> a -> int =
 fun x v ->
  match (x, v) with Atom Bool, true -> 1 | Nil, _ -> 12 | _ -> to_int x

let read_int32 x = uleb128_r (module Int32) x
let read_int64 x = uleb128_r (module Int64) x
let write_int32 x = uleb128_w (module Int32) x
let write_int64 x = uleb128_w (module Int64) x
let read_int x = uleb128_r (module Int) x
let write_int x = uleb128_w (module Int) x
let peak_int x = uleb128_r_peak (module Int) x

let read_field_header curr buf =
  let hdr = Iobuf.Peek.uint8 buf ~pos:0 in
  match hdr land 0xf with
  | 0 -> None
  | typ -> (
    match hdr lsr 4 with
    | 0 ->
        let consumed, id = peak_int buf ~pos:1 in
        let id = Int.(id |> zag) in
        Some (typ, consumed, id)
    | delta ->
        let id = curr + delta in
        Some (typ, 1, id) )

let rec read : type a. ?typ:int -> a t -> ([> read], Iobuf.seek) Iobuf.t -> a =
 fun ?typ e buf ->
  match e with
  | Atom Bool -> (
    match typ with
    | Some 1 -> true
    | Some 2 -> false
    | _ -> ( match Iobuf.Consume.int8 buf with 0 -> false | _ -> true ) )
  | Atom Int8 -> Iobuf.Consume.int8 buf
  | Atom Int16 -> read_int buf |> Int.zag
  | Atom Int32 -> read_int32 buf |> Int32.zag
  | Atom Int64 -> read_int64 buf |> Int64.zag
  | Atom Int32e -> read_int buf |> Int.zag
  | Atom Int64e -> read_int buf |> Int.zag
  | Atom Double -> Int64.float_of_bits (Iobuf.Consume.int64_t_le buf)
  | Atom Bytes ->
      let len = read_int64 buf |> Int64.to_int_exn in
      Iobuf.Consume.string ~str_pos:0 ~len buf
  | List (_isSet, x) ->
      let hdr = Iobuf.Consume.uint8 buf in
      let len = hdr lsr 4 in
      let len = if len = 15 then Int32.to_int_exn (read_int32 buf) else len in
      Array.init len ~f:(fun _ -> read x buf)
  | Nil ->
      if Iobuf.Consume.uint8 buf <> 0 then
        failwith "read: nil expected, but read something else"
  | Field _ | Fields _ ->
      let _i, x = read_field 0 e buf in
      (* read stop field *)
      read nil buf ; x
  | Map (kt, vt) ->
      let len = read_int32 buf |> Int32.to_int_exn in
      Array.init len ~f:(fun _ ->
          let k = read kt buf in
          let v = read vt buf in
          (k, v) )
  | Union cases -> (
    match read_field_header 0 buf with
    | None -> failwith "read: union"
    | Some (typ, len, id) -> (
        let _discard = Iobuf.Consume.string ~str_pos:0 ~len buf in
        match List.nth cases (pred id) with
        | None -> failwith "read: bad union"
        | Some (Case {e; inj; _}) ->
            let x = inj (read ~typ e buf) in
            (* read stop field *)
            read nil buf ; x ) )
  | Conv (_proj, inj, x) -> inj (read x buf)

and read_field : type a. int -> a t -> ([> read], Iobuf.seek) Iobuf.t -> int * a
    =
 fun curr e buf ->
  match e with
  | Field field -> (
    match read_field_header curr buf with
    | None -> (
      match field with
      | Req {name; _} ->
          failwithf "read_field: required field %s, but end of struct read" name
            ()
      | Opt _ -> (curr, None)
      | Dft {x; _} -> (curr, x) )
    | Some (typ, consumed, id) -> (
      match field with
      | Req {i; e; name} ->
          (* Format.printf "Field %s@." name ; *)
          let _discard = Iobuf.Consume.string ~str_pos:0 ~len:consumed buf in
          let x = read ~typ e buf in
          if i <> id then failwithf "%s; expected %d, read %d" name i id () ;
          (id, x)
      | Opt {i; e; _} when i = id ->
          (* Format.printf "Field %s@." name ; *)
          let _discard = Iobuf.Consume.string ~str_pos:0 ~len:consumed buf in
          (i, Some (read ~typ e buf))
      | Dft {i; e; _} when i = id ->
          (* Format.printf "Field %s@." name ; *)
          let _discard = Iobuf.Consume.string ~str_pos:0 ~len:consumed buf in
          (i, read ~typ e buf)
      | Opt _ -> (curr, None)
      | Dft {x; _} -> (curr, x) ) )
  | Fields (a, b) ->
      let id, x = read_field curr a buf in
      let id', y = read_field id b buf in
      (id', (x, y))
  | Conv (_proj, inj, x) ->
      let i, x = read_field curr x buf in
      (i, inj x)
  | _ -> assert false

let read e x = read e x

let rec write : type a. a t -> a -> (read_write, Iobuf.seek) Iobuf.t -> unit =
 fun e x buf ->
  match e with
  | Conv (proj, _inj, e') -> write e' (proj x) buf
  | Atom Int8 -> Iobuf.Fill.int8_trunc buf x
  | Atom Int16 -> Int.zig x |> write_int buf
  | Atom Int32 -> write_int32 buf (Int32.zig x)
  | Atom Int64 -> write_int64 buf (Int64.zig x)
  | Atom Int32e -> Int.zig x |> write_int buf
  | Atom Int64e -> Int.zig x |> write_int buf
  | Atom Bytes ->
      let len = String.length x in
      write_int64 buf (Int64.of_int len) ;
      Iobuf.Fill.string ~str_pos:0 ~len buf x
  | Atom Double -> Iobuf.Fill.int64_t_be buf (Int64.bits_of_float x)
  | Atom Bool -> write (Atom Int8) (if x then 1 else 0) buf
  | Nil -> Iobuf.Fill.uint8_trunc buf 0
  | List (_isSet, typ) ->
      let len = Array.length x in
      Iobuf.Fill.uint8_trunc buf (to_int typ lor (min len 15 lsl 4)) ;
      if len > 14 then write_int32 buf (Int32.of_int_exn len) ;
      Array.iter x ~f:(fun x -> write typ x buf)
  | Map (hk, hv) ->
      let len = Array.length x in
      write_int32 buf (Int32.of_int_exn len) ;
      let kt = to_int hk in
      let vt = to_int hv in
      Iobuf.Fill.uint8_trunc buf ((kt lsl 4) lor vt) ;
      Array.iter x ~f:(fun (k, v) -> write hk k buf ; write hv v buf)
  | Field _ | Fields _ ->
      let _ign = write_field 0 e x buf in
      write nil () buf
  | Union cases -> (
    match
      List.fold_left ~init:0 cases ~f:(fun i -> function
        | Case {e; proj; name; _} -> (
            let id = succ i in
            match proj x with
            | None -> id
            | Some x ->
                write (obj1 (req id e name)) x buf ;
                raise Exit ) )
    with
    | exception Exit -> ()
    | _ -> invalid_arg "write: bad union" )

and write_field :
    type a. int -> a t -> a -> (read_write, Iobuf.seek) Iobuf.t -> int =
 fun cur e x buf ->
  match e with
  | Conv (proj, _, e) -> write_field cur e (proj x) buf
  | Field (Opt {i; e; name}) ->
      Option.value_map ~default:cur x ~f:(fun x ->
          write_field cur (Field (req i e name)) x buf )
  | Field (Dft {i; e; x= dft; name}) ->
      if Poly.(x <> dft) then write_field cur (Field (req i e name)) x buf
      else cur
  | Fields (a, b) ->
      let cur = write_field cur a (fst x) buf in
      write_field cur b (snd x) buf
  (* Cases above redirect here. *)
  | Field (Req {i; e; _}) when i - cur < 16 ->
      let delta = i - cur in
      let typ = to_int_v e x in
      let hdr = typ lor (delta lsl 4) in
      assert (delta > 0) ;
      assert (hdr land 0xf > 0) ;
      Iobuf.Fill.uint8_trunc buf hdr ;
      if typ > 2 then write e x buf ;
      i
  (* Mostly never called. *)
  | Field (Req {i; e; _}) ->
      Iobuf.Fill.uint8_trunc buf (to_int_v e x) ;
      write (Atom Int16) i buf ;
      write e x buf ;
      i
  | _ -> assert false
