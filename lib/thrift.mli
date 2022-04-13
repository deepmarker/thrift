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

(* *)
val bool : bool t
val int8 : int t
val int16 : int t
val int32 : int32 t
val int64 : int64 t
val int32e : int t
val int64e : int t
val double : float t
val bytes : string t
val nil : unit t
val list : 'a t -> 'a array t
val set : 'a t -> 'a array t
val map : 'a t -> 'b t -> ('a * 'b) array t
val enum : 'a list -> 'a t

(* *)
val union : 'a case list -> 'a t
val case : 'a t -> ('b -> 'a option) -> ('a -> 'b) -> string -> 'b case
val req : int -> 'a t -> string -> 'a field
val opt : int -> 'a t -> string -> 'a option field
val dft : int -> 'a t -> string -> 'a -> 'a field
val obj1 : 'a field -> 'a t
val obj2 : 'a field -> 'b field -> ('a * 'b) t
val obj3 : 'a field -> 'b field -> 'c field -> ('a * 'b * 'c) t
val obj4 : 'a field -> 'b field -> 'c field -> 'd field -> ('a * 'b * 'c * 'd) t

val obj5 :
  'a field ->
  'b field ->
  'c field ->
  'd field ->
  'e field ->
  ('a * 'b * 'c * 'd * 'e) t

val obj6 :
  'a field ->
  'b field ->
  'c field ->
  'd field ->
  'e field ->
  'f field ->
  ('a * 'b * 'c * 'd * 'e * 'f) t

val obj7 :
  'a field ->
  'b field ->
  'c field ->
  'd field ->
  'e field ->
  'f field ->
  'g field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g) t

val obj8 :
  'a field ->
  'b field ->
  'c field ->
  'd field ->
  'e field ->
  'f field ->
  'g field ->
  'h field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t

val obj9 :
  'a field ->
  'b field ->
  'c field ->
  'd field ->
  'e field ->
  'f field ->
  'g field ->
  'h field ->
  'i field ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t

val conv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t
val merge_objs : 'a t -> 'b t -> ('a * 'b) t
