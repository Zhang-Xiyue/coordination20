(**
 * Why3 leaves a set of unimplemented parts in the extracted codes, i.e. which number library to use
 * for big numbers, etc. This file implements these holes.
 *)

open Big_int;;

module Z = struct
    type t = big_int

    let of_int = big_int_of_int
    let to_int = int_of_big_int

    let zero = of_int 0
    let uint256_ubound = (power_big_int_positive_int (of_int 2) 256)

    let add a b = mod_big_int (add_big_int a b) uint256_ubound
    let sub a b = mod_big_int (sub_big_int a b) uint256_ubound
    let mul a b = mod_big_int (mult_big_int a b) uint256_ubound
    let div = div_big_int
    let ediv = div

    let zmod = mod_big_int
    let erem = zmod

    let eq = eq_big_int
    let gt = gt_big_int
    let ge = ge_big_int
    let le = le_big_int
    let lt = lt_big_int

    let negate a = sub (of_int 0) a

    let to_bytes (value: t) : string =
    let ret: string list ref = ref [] in
    let v = ref value in
    while gt !v zero do
        let rest = zmod !v (of_int 256) in
        ret := (Printf.sprintf "%02X" (to_int rest)) :: !ret;
        v := div !v (of_int 256)
    done;
    String.concat "" !ret

end

let power = power_big_int_positive_big_int

module Storage = struct

    module StringMap = Map.Make(String)

    type t = Z.t StringMap.t

    let update_storage storage (key: Z.t) (value: Z.t) : t =
        let str_key = Z.to_bytes key in
        StringMap.add str_key value storage

end
