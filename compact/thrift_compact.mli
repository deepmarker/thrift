open Core
open Thrift

val read : 'a t -> ([> read], Iobuf.seek) Iobuf.t -> 'a
val write : 'a t -> 'a -> (read_write, Iobuf.seek) Iobuf.t -> unit

(**/*)

val read_int : ([> read], Iobuf.seek) Iobuf.t -> int
val read_int32 : ([> read], Iobuf.seek) Iobuf.t -> int32
val read_int64 : ([> read], Iobuf.seek) Iobuf.t -> int64
val write_int : (read_write, Iobuf.seek) Iobuf.t -> int -> unit
val write_int32 : (read_write, Iobuf.seek) Iobuf.t -> int32 -> unit
val write_int64 : (read_write, Iobuf.seek) Iobuf.t -> int64 -> unit
