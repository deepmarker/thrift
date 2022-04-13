type _ t =
  | Atom : 'a atom -> 'a t
  | List : bool * 'a t -> 'a array t
  | Map : 'a t * 'b t -> ('a * 'b) array t
  | Nil : unit t (* empty map / end of struct *)
  | Field : 'a field -> 'a t
  | Fields : 'a t * 'b t -> ('a * 'b) t
  | Union : 'a case list -> 'a t
  | Conv : ('a -> 'b) * ('b -> 'a) * 'b t -> 'a t

and _ atom =
  | Bool : bool atom
  | Int8 : int atom
  | Int16 : int atom
  | Int32 : int32 atom
  | Int64 : int64 atom
  | Int32e : int atom
  | Int64e : int atom
  | Double : float atom
  | Bytes : string atom

and _ case =
  | Case :
      {e: 'a t; proj: 't -> 'a option; inj: 'a -> 't; name: string}
      -> 't case

and _ field =
  | Req : {name: string; i: int; e: 'a t} -> 'a field
  | Opt : {name: string; i: int; e: 'a t} -> 'a option field
  | Dft : {name: string; i: int; e: 'a t; x: 'a} -> 'a field

let bool = Atom Bool
let int8 = Atom Int8
let int16 = Atom Int16
let int32 = Atom Int32
let int64 = Atom Int64
let int32e = Atom Int32e
let int64e = Atom Int64e
let double = Atom Double
let bytes = Atom Bytes
let nil = Nil
let list x = List (false, x)
let set x = List (true, x)
let map k v = Map (k, v)
let req i e name = Req {name; i; e}
let opt i e name = Opt {name; i; e}
let dft i e name x = Dft {name; i; e; x}
let field x = Field x
let case e proj inj name = Case {e; proj; inj; name}
let union cases = Union cases

let rec is_field : type a. a t -> bool = function
  | Conv (_, _, x) -> is_field x
  | Field _ | Fields _ -> true
  | _ -> false

let merge_objs x y =
  if not (is_field x && is_field y) then invalid_arg "merge_objs" ;
  Fields (x, y)

let conv proj inj x = Conv (proj, inj, x)
let obj1 x = field x
let obj2 x y = merge_objs (field x) (field y)
let obj3 x y z = merge_objs (field x) (merge_objs (field y) (field z))

let obj3 x y z =
  conv
    (fun (x, y, z) -> (x, (y, z)))
    (fun (x, (y, z)) -> (x, y, z))
    (obj3 x y z)

let obj4 x y z t =
  merge_objs (field x) (merge_objs (field y) (merge_objs (field z) (field t)))

let obj4 x y z t =
  conv
    (fun (x, y, z, t) -> (x, (y, (z, t))))
    (fun (x, (y, (z, t))) -> (x, y, z, t))
    (obj4 x y z t)

let obj5 x y z t a =
  merge_objs (field x)
    (merge_objs (field y)
       (merge_objs (field z) (merge_objs (field t) (field a))) )

let obj5 x y z t a =
  conv
    (fun (x, y, z, t, a) -> (x, (y, (z, (t, a)))))
    (fun (x, (y, (z, (t, a)))) -> (x, y, z, t, a))
    (obj5 x y z t a)

let obj6 x y z t a b =
  merge_objs (field x)
    (merge_objs (field y)
       (merge_objs (field z)
          (merge_objs (field t) (merge_objs (field a) (field b))) ) )

let obj6 x y z t a b =
  conv
    (fun (x, y, z, t, a, b) -> (x, (y, (z, (t, (a, b))))))
    (fun (x, (y, (z, (t, (a, b))))) -> (x, y, z, t, a, b))
    (obj6 x y z t a b)

let obj7 x y z t a b c =
  merge_objs (field x)
    (merge_objs (field y)
       (merge_objs (field z)
          (merge_objs (field t)
             (merge_objs (field a) (merge_objs (field b) (field c))) ) ) )

let obj7 x y z t a b c =
  conv
    (fun (x, y, z, t, a, b, c) -> (x, (y, (z, (t, (a, (b, c)))))))
    (fun (x, (y, (z, (t, (a, (b, c)))))) -> (x, y, z, t, a, b, c))
    (obj7 x y z t a b c)

let obj8 x y z t a b c d =
  merge_objs (field x)
    (merge_objs (field y)
       (merge_objs (field z)
          (merge_objs (field t)
             (merge_objs (field a)
                (merge_objs (field b) (merge_objs (field c) (field d))) ) ) ) )

let obj8 x y z t a b c d =
  conv
    (fun (x, y, z, t, a, b, c, d) -> (x, (y, (z, (t, (a, (b, (c, d))))))))
    (fun (x, (y, (z, (t, (a, (b, (c, d))))))) -> (x, y, z, t, a, b, c, d))
    (obj8 x y z t a b c d)

let obj9 x y z t a b c d e =
  merge_objs (field x)
    (merge_objs (field y)
       (merge_objs (field z)
          (merge_objs (field t)
             (merge_objs (field a)
                (merge_objs (field b)
                   (merge_objs (field c) (merge_objs (field d) (field e))) ) ) ) ) )

let obj9 x y z t a b c d e =
  conv
    (fun (x, y, z, t, a, b, c, d, e) ->
      (x, (y, (z, (t, (a, (b, (c, (d, e)))))))) )
    (fun (x, (y, (z, (t, (a, (b, (c, (d, e)))))))) ->
      (x, y, z, t, a, b, c, d, e) )
    (obj9 x y z t a b c d e)

exception Index of int

let enum xs =
  let int_of_x xs x =
    match List.iteri (fun i y -> if x = y then raise (Index i)) xs with
    | exception Index i -> i
    | () -> raise Not_found in
  let x_of_int xs i = List.nth xs i in
  conv (int_of_x xs) (x_of_int xs) int16
