open Big_int;;

module Z = struct
    type t = big_int

    let of_int = big_int_of_int
    let to_int = int_of_big_int

    let zero = of_int 0

    let add = add_big_int
    let sub = sub_big_int
    let mul = mult_big_int
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

    let update_storage (key: Z.t) (value: Z.t) storage : t =
        let str_key = Z.to_bytes key in
        StringMap.add str_key value storage

end
type byte = Z.t

type address = Z.t

type machword = Z.t

type storage = Storage.t

type memory_content =
  | Item8 of Z.t
  | Item256 of Z.t

type memory = (Z.t) -> (memory_content option)

type stack = (Z.t) list

type error =
  | OutOfBounds
  | OutOfGas
  | OutOfStack
  | OutOfCode
  | OutOfData
  | InvalidOpcode

type return_type =
  | Normal of Z.t
  | Create of Z.t
  | Revert

type vmstatus =
  | Running
  | Error of error
  | Finish of return_type

type st_num = (Z.t) * (Z.t)

type sign_arith_inst =
  | SDIV
  | SMOD
  | SLT
  | SGT
  | SIGNEXTEND

let sign_arith_inst_opcode (inst: sign_arith_inst) : Z.t =
  begin match inst with
  | SDIV -> (Z.of_int 5)
  | SMOD -> (Z.of_int 7)
  | SLT -> (Z.of_int 18)
  | SGT -> (Z.of_int 19)
  | SIGNEXTEND -> (Z.of_int 11)
  end

let sign_arith_inst_num (inst: sign_arith_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | SDIV -> ((Z.of_int 2), (Z.of_int 1))
  | SMOD -> ((Z.of_int 2), (Z.of_int 1))
  | SLT -> ((Z.of_int 2), (Z.of_int 1))
  | SGT -> ((Z.of_int 2), (Z.of_int 1))
  | SIGNEXTEND -> ((Z.of_int 2), (Z.of_int 1))
  end

type arith_inst =
  | ADD
  | MUL
  | SUB
  | DIV
  | MOD
  | ADDMOD
  | MULMOD
  | EXP
  | LT
  | GT
  | EQ
  | ISZERO
  | SHA3

let arith_inst_opcode (inst: arith_inst) : Z.t =
  begin match inst with
  | ADD -> (Z.of_int 1)
  | MUL -> (Z.of_int 2)
  | SUB -> (Z.of_int 3)
  | DIV -> (Z.of_int 4)
  | MOD -> (Z.of_int 6)
  | ADDMOD -> (Z.of_int 8)
  | MULMOD -> (Z.of_int 9)
  | EXP -> (Z.of_int 10)
  | LT -> (Z.of_int 16)
  | GT -> (Z.of_int 17)
  | EQ -> (Z.of_int 20)
  | ISZERO -> (Z.of_int 21)
  | SHA3 -> (Z.of_int 32)
  end

let arith_inst_num (inst: arith_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | ADD -> ((Z.of_int 2), (Z.of_int 1))
  | MUL -> ((Z.of_int 2), (Z.of_int 1))
  | SUB -> ((Z.of_int 2), (Z.of_int 1))
  | DIV -> ((Z.of_int 2), (Z.of_int 1))
  | MOD -> ((Z.of_int 2), (Z.of_int 1))
  | ADDMOD -> ((Z.of_int 3), (Z.of_int 1))
  | MULMOD -> ((Z.of_int 3), (Z.of_int 1))
  | EXP -> ((Z.of_int 2), (Z.of_int 1))
  | LT -> ((Z.of_int 2), (Z.of_int 1))
  | GT -> ((Z.of_int 2), (Z.of_int 1))
  | EQ -> ((Z.of_int 2), (Z.of_int 1))
  | ISZERO -> ((Z.of_int 1), (Z.of_int 1))
  | SHA3 -> ((Z.of_int 2), (Z.of_int 1))
  end

type bits_inst =
  | AND
  | OR
  | XOR
  | NOT
  | BYTE

let bit_inst_opcode (inst: bits_inst) : Z.t =
  begin match inst with
  | AND -> (Z.of_int 22)
  | OR -> (Z.of_int 23)
  | XOR -> (Z.of_int 24)
  | NOT -> (Z.of_int 25)
  | BYTE -> (Z.of_int 26)
  end

let bit_inst_num (inst: bits_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | AND -> ((Z.of_int 2), (Z.of_int 1))
  | OR -> ((Z.of_int 2), (Z.of_int 1))
  | XOR -> ((Z.of_int 2), (Z.of_int 1))
  | NOT -> ((Z.of_int 1), (Z.of_int 1))
  | BYTE -> ((Z.of_int 2), (Z.of_int 1))
  end

type info_inst =
  | ADDRESS
  | BALANCE
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATASIZE
  | CODESIZE
  | GASPRICE
  | EXTCODESIZE
  | RETURNDATASIZE
  | RETURNDATACOPY
  | BLOCKHASH
  | COINBASE
  | TIMESTAMP
  | NUMBER
  | DIFFICULTY
  | GASLIMIT
  | GAS

let info_inst_opcode (inst: info_inst) : Z.t =
  begin match inst with
  | ADDRESS -> (Z.of_int 48)
  | BALANCE -> (Z.of_int 49)
  | ORIGIN -> (Z.of_int 50)
  | CALLER -> (Z.of_int 51)
  | CALLVALUE -> (Z.of_int 52)
  | CALLDATASIZE -> (Z.of_int 54)
  | CODESIZE -> (Z.of_int 56)
  | GASPRICE -> (Z.of_int 58)
  | EXTCODESIZE -> (Z.of_int 59)
  | RETURNDATASIZE -> (Z.of_int 61)
  | RETURNDATACOPY -> (Z.of_int 62)
  | BLOCKHASH -> (Z.of_int 64)
  | COINBASE -> (Z.of_int 65)
  | TIMESTAMP -> (Z.of_int 66)
  | NUMBER -> (Z.of_int 67)
  | DIFFICULTY -> (Z.of_int 68)
  | GASLIMIT -> (Z.of_int 69)
  | GAS -> (Z.of_int 90)
  end

let info_inst_num (inst: info_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | ADDRESS -> ((Z.of_int 0), (Z.of_int 1))
  | BALANCE -> ((Z.of_int 1), (Z.of_int 1))
  | ORIGIN -> ((Z.of_int 0), (Z.of_int 1))
  | CALLER -> ((Z.of_int 0), (Z.of_int 1))
  | CALLVALUE -> ((Z.of_int 0), (Z.of_int 1))
  | CALLDATASIZE -> ((Z.of_int 0), (Z.of_int 1))
  | CODESIZE -> ((Z.of_int 0), (Z.of_int 1))
  | GASPRICE -> ((Z.of_int 0), (Z.of_int 1))
  | EXTCODESIZE -> ((Z.of_int 1), (Z.of_int 1))
  | RETURNDATASIZE -> ((Z.of_int 0), (Z.of_int 1))
  | RETURNDATACOPY -> ((Z.of_int 3), (Z.of_int 0))
  | BLOCKHASH -> ((Z.of_int 1), (Z.of_int 1))
  | COINBASE -> ((Z.of_int 0), (Z.of_int 1))
  | TIMESTAMP -> ((Z.of_int 0), (Z.of_int 1))
  | NUMBER -> ((Z.of_int 0), (Z.of_int 1))
  | DIFFICULTY -> ((Z.of_int 0), (Z.of_int 1))
  | GASLIMIT -> ((Z.of_int 0), (Z.of_int 1))
  | GAS -> ((Z.of_int 0), (Z.of_int 1))
  end

type memory_inst =
  | MLOAD
  | MSTORE
  | MSTORE8
  | MSIZE
  | CALLDATACOPY
  | CODECOPY
  | EXTCODECOPY

let memory_inst_opcode (inst: memory_inst) : Z.t =
  begin match inst with
  | MLOAD -> (Z.of_int 81)
  | MSTORE -> (Z.of_int 82)
  | MSTORE8 -> (Z.of_int 83)
  | MSIZE -> (Z.of_int 89)
  | CALLDATACOPY -> (Z.of_int 55)
  | CODECOPY -> (Z.of_int 57)
  | EXTCODECOPY -> (Z.of_int 60)
  end

let memory_inst_num (inst: memory_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | MLOAD -> ((Z.of_int 1), (Z.of_int 1))
  | MSTORE -> ((Z.of_int 2), (Z.of_int 0))
  | MSTORE8 -> ((Z.of_int 2), (Z.of_int 0))
  | MSIZE -> ((Z.of_int 0), (Z.of_int 1))
  | CALLDATACOPY -> ((Z.of_int 3), (Z.of_int 0))
  | CODECOPY -> ((Z.of_int 3), (Z.of_int 0))
  | EXTCODECOPY -> ((Z.of_int 4), (Z.of_int 0))
  end

type storage_inst =
  | SLOAD
  | SSTORE

let storage_inst_opcode (inst: storage_inst) : Z.t =
  begin match inst with
  | SLOAD -> (Z.of_int 84)
  | SSTORE -> (Z.of_int 85)
  end

let storage_inst_num (inst: storage_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | SLOAD -> ((Z.of_int 1), (Z.of_int 1))
  | SSTORE -> ((Z.of_int 2), (Z.of_int 0))
  end

type pc_inst =
  | JUMP
  | JUMPI
  | PC
  | JUMPDEST

let pc_inst_opcode (inst: pc_inst) : Z.t =
  begin match inst with
  | JUMP -> (Z.of_int 86)
  | JUMPI -> (Z.of_int 87)
  | PC -> (Z.of_int 88)
  | JUMPDEST -> (Z.of_int 91)
  end

let pc_inst_num (inst: pc_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | JUMP -> ((Z.of_int 1), (Z.of_int 0))
  | JUMPI -> ((Z.of_int 2), (Z.of_int 0))
  | PC -> ((Z.of_int 0), (Z.of_int 1))
  | JUMPDEST -> ((Z.of_int 0), (Z.of_int 0))
  end

type stack_inst =
  | POP
  | PUSH of Z.t
  | CALLDATALOAD

exception PushError

let stack_inst_opcode (inst: stack_inst) : (Z.t) list =
  begin match inst with
  | POP -> (Z.of_int 80) :: []
  | CALLDATALOAD -> (Z.of_int 53) :: []
  | _ -> assert false (* absurd *)
  end

let stack_inst_num (inst: stack_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | POP -> ((Z.of_int 1), (Z.of_int 0))
  | PUSH _ -> ((Z.of_int 0), (Z.of_int 1))
  | CALLDATALOAD -> ((Z.of_int 1), (Z.of_int 1))
  end

type dup_inst = Z.t

let dup_inst_opcode (inst: Z.t) : Z.t = Z.add (inst) (Z.of_int 128)

let dup_inst_num (inst: Z.t) : (Z.t) * (Z.t) =
  (inst, Z.add (inst) (Z.of_int 1))

type swap_inst = Z.t

let swap_inst_opcode (inst: Z.t) : Z.t = Z.add (inst) (Z.of_int 144)

let swap_inst_num (inst: Z.t) : (Z.t) * (Z.t) =
  (Z.add (inst) (Z.of_int 1), Z.add (inst) (Z.of_int 1))

type log_inst =
  | LOG0
  | LOG1
  | LOG2
  | LOG3
  | LOG4

let log_inst_opcode (inst: log_inst) : Z.t =
  begin match inst with
  | LOG0 -> (Z.of_int 160)
  | LOG1 -> (Z.of_int 161)
  | LOG2 -> (Z.of_int 162)
  | LOG3 -> (Z.of_int 163)
  | LOG4 -> (Z.of_int 164)
  end

let log_inst_num (inst: log_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | LOG0 -> ((Z.of_int 2), (Z.of_int 0))
  | LOG1 -> ((Z.of_int 3), (Z.of_int 0))
  | LOG2 -> ((Z.of_int 4), (Z.of_int 0))
  | LOG3 -> ((Z.of_int 5), (Z.of_int 0))
  | LOG4 -> ((Z.of_int 6), (Z.of_int 0))
  end

type system_inst =
  | STOP
  | CREATE
  | CALL
  | CALLCODE
  | RETURN
  | DELEGATECALL
  | STATICCALL
  | REVERT
  | SELFDESTRUCT

let system_inst_opcode (inst: system_inst) : Z.t =
  begin match inst with
  | STOP -> (Z.of_int 0)
  | CREATE -> (Z.of_int 240)
  | CALL -> (Z.of_int 241)
  | CALLCODE -> (Z.of_int 242)
  | RETURN -> (Z.of_int 243)
  | DELEGATECALL -> (Z.of_int 244)
  | STATICCALL -> (Z.of_int 250)
  | REVERT -> (Z.of_int 253)
  | SELFDESTRUCT -> (Z.of_int 255)
  end

let system_inst_num (inst: system_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | STOP -> ((Z.of_int 0), (Z.of_int 0))
  | CREATE -> ((Z.of_int 3), (Z.of_int 1))
  | CALL -> ((Z.of_int 7), (Z.of_int 1))
  | CALLCODE -> ((Z.of_int 7), (Z.of_int 1))
  | RETURN -> ((Z.of_int 2), (Z.of_int 0))
  | DELEGATECALL -> ((Z.of_int 6), (Z.of_int 1))
  | STATICCALL -> ((Z.of_int 6), (Z.of_int 1))
  | REVERT -> ((Z.of_int 2), (Z.of_int 0))
  | SELFDESTRUCT -> ((Z.of_int 1), (Z.of_int 0))
  end

type instruction =
  | Invalid of Z.t
  | Arith of arith_inst
  | Sarith of sign_arith_inst
  | Bits of bits_inst
  | Info of info_inst
  | Memory of memory_inst
  | Storage of storage_inst
  | Pc of pc_inst
  | Stack of stack_inst
  | Dup of Z.t
  | Swap of Z.t
  | Log of log_inst
  | System of system_inst

let inst_opcode (inst: instruction) : (Z.t) list =
  begin match inst with
  | Invalid byte1 -> (byte1) :: []
  | Arith a -> (arith_inst_opcode a) :: []
  | Sarith s -> (sign_arith_inst_opcode s) :: []
  | Bits b -> (bit_inst_opcode b) :: []
  | Info i -> (info_inst_opcode i) :: []
  | Memory m -> (memory_inst_opcode m) :: []
  | Storage s -> (storage_inst_opcode s) :: []
  | Pc p -> (pc_inst_opcode p) :: []
  | Stack s -> stack_inst_opcode s
  | Dup d -> (dup_inst_opcode d) :: []
  | Swap s -> (swap_inst_opcode s) :: []
  | Log l -> (log_inst_opcode l) :: []
  | System m -> (system_inst_opcode m) :: []
  end

let inst_stack_num (inst: instruction) : (Z.t) * (Z.t) =
  begin match inst with
  | Invalid _ -> ((Z.of_int 0), (Z.of_int 0))
  | Arith a -> arith_inst_num a
  | Sarith s -> sign_arith_inst_num s
  | Bits b -> bit_inst_num b
  | Info i -> info_inst_num i
  | Memory m -> memory_inst_num m
  | Storage s -> storage_inst_num s
  | Pc p -> pc_inst_num p
  | Stack s -> stack_inst_num s
  | Dup d -> dup_inst_num d
  | Swap s -> swap_inst_num s
  | Log l -> log_inst_num l
  | System m -> system_inst_num m
  end

let gas_of_Wzero (inst: instruction) : Z.t =
  begin match inst with
  | System STOP -> (Z.of_int 0)
  | System RETURN -> (Z.of_int 0)
  | System REVERT -> (Z.of_int 0)
  | _ -> assert false (* absurd *)
  end

let gas_of_Wbase (inst: instruction) : Z.t =
  begin match inst with
  | Info ADDRESS -> (Z.of_int 2)
  | Info ORIGIN -> (Z.of_int 2)
  | Info CALLER -> (Z.of_int 2)
  | Info CALLVALUE -> (Z.of_int 2)
  | Info CALLDATASIZE -> (Z.of_int 2)
  | Info CODESIZE -> (Z.of_int 2)
  | Info GASPRICE -> (Z.of_int 2)
  | Info COINBASE -> (Z.of_int 2)
  | Info TIMESTAMP -> (Z.of_int 2)
  | Info NUMBER -> (Z.of_int 2)
  | Info DIFFICULTY -> (Z.of_int 2)
  | Info GASLIMIT -> (Z.of_int 2)
  | Info RETURNDATASIZE -> (Z.of_int 2)
  | Stack POP -> (Z.of_int 2)
  | Pc PC -> (Z.of_int 2)
  | Memory MSIZE -> (Z.of_int 2)
  | Info GAS -> (Z.of_int 2)
  | _ -> assert false (* absurd *)
  end

let gas_of_Wlow (inst: instruction) : Z.t =
  begin match inst with
  | Arith MUL -> (Z.of_int 5)
  | Arith DIV -> (Z.of_int 5)
  | Sarith SDIV -> (Z.of_int 5)
  | Arith MOD -> (Z.of_int 5)
  | Sarith SMOD -> (Z.of_int 5)
  | Sarith SIGNEXTEND -> (Z.of_int 5)
  | _ -> assert false (* absurd *)
  end

type machine_state = {
  mac_stack: (Z.t) list;
  mac_memory: (Z.t) -> (memory_content option);
  mac_storage: storage;
  mac_pc: Z.t;
  mac_status: vmstatus;
  mac_memory_usage: Z.t;
  mac_gas: Z.t;
  mac_insts: instruction list;
  mac_jumpmap: (Z.t) -> (Z.t);
  }

let update_memory_content (m: (Z.t) -> (memory_content option)) (idx: Z.t)
                          (cont: memory_content option) : (Z.t) -> (memory_content option)
  =
  fun (addr: Z.t) ->
    if Z.eq addr idx then begin cont end else begin m addr end

let push_stack (s: (Z.t) list) (ele: Z.t) : (Z.t) list = ele :: s

let pop_stack (s: (Z.t) list) : ((Z.t) list) * ((Z.t) option) =
  begin match s with
  | x :: t -> (t, Some x)
  | _ -> (s, None)
  end

let rec fetch (lst: (Z.t) list) (n: Z.t) : (Z.t) list =
  if (Z.gt n (Z.of_int (List.length lst))) || (Z.eq n (Z.of_int 0)) then
    begin [] end
  else
  begin
    begin match lst with
    | [] -> []
    | x :: r -> x :: (fetch r (Z.sub n (Z.of_int 1)))
    end end

let rec drop (lst: (Z.t) list) (n: Z.t) : (Z.t) list =
  if Z.eq n (Z.of_int 0) then begin lst end
  else
  begin
    List.rev (fetch (List.rev lst) (Z.sub (Z.of_int (List.length lst)) n)) end

let swap_stack (s: (Z.t) list) (i: Z.t) : ((Z.t) list) option =
  begin match (List.nth_opt s (Z.to_int (Z.of_int 0)),
              List.nth_opt s (Z.to_int (Z.sub i (Z.of_int 1)))) with
  | (Some ele_0, Some ele_i_prev) ->
    Some (List.append (ele_i_prev :: (fetch (drop s (Z.of_int 1))
                                        (Z.sub i (Z.of_int 2)))) (ele_0 :: (
    drop s i)))
  | (_, _) -> None
  end

let update_storage_from_address (storage1: storage) (addr1: Z.t)
                                (v: Z.t) : storage =
  Storage.update_storage addr1 v storage1

let update_stack (st: (Z.t) list) (m: machine_state) : machine_state =
  { mac_stack = st; mac_memory = (m.mac_memory); mac_storage =
    (m.mac_storage); mac_pc = (m.mac_pc); mac_status = (m.mac_status);
    mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
    mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }

let update_memory (memo: (Z.t) -> (memory_content option)) (m: machine_state) : machine_state
  =
  { mac_stack = (m.mac_stack); mac_memory = memo; mac_storage =
    (m.mac_storage); mac_pc = (m.mac_pc); mac_status = (m.mac_status);
    mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
    mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }

let update_pc (pc: Z.t) (m: machine_state) : machine_state =
  { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
    (m.mac_storage); mac_pc = pc; mac_status = (m.mac_status);
    mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
    mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }

let update_status (vst: vmstatus) (m: machine_state) : machine_state =
  { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
    (m.mac_storage); mac_pc = (m.mac_pc); mac_status = vst;
    mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
    mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }

let inc_pc (m: machine_state) : machine_state =
  update_pc (Z.add (m.mac_pc) (Z.of_int 1)) m

let get_inst (mac_st: machine_state) : instruction option =
  let (pc, insts) = (mac_st.mac_pc, mac_st.mac_insts) in
  List.nth_opt insts (Z.to_int pc)

let interpreter (m: machine_state) : machine_state =
  let inst = get_inst m in
  begin match inst with
  | Some System STOP ->
    { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
      (m.mac_storage); mac_pc = (m.mac_pc); mac_status =
      (Finish (Normal ((Z.of_int 0)))); mac_memory_usage =
      (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts = (m.mac_insts);
      mac_jumpmap = (m.mac_jumpmap) }
  | Some Arith ADD ->
    let (stqt, a1) = pop_stack (m.mac_stack) in let (stqtqt, b) =
    pop_stack stqt in
    begin match (a1, b) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt (Z.add aqt bqt)); mac_memory =
        (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (Z.sub (m.mac_gas) (Z.of_int 3)); mac_insts = (m.mac_insts);
        mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith MUL ->
    let (stqt1, a2) = pop_stack (m.mac_stack) in let (stqtqt1, b1) =
    pop_stack stqt1 in
    begin match (a2, b1) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt1 (Z.mul aqt bqt)); mac_memory =
        (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith SUB ->
    let (stqt2, a3) = pop_stack (m.mac_stack) in let (stqtqt2, b2) =
    pop_stack stqt2 in
    begin match (a3, b2) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt2 (Z.sub aqt bqt)); mac_memory =
        (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith DIV ->
    let (stqt3, a4) = pop_stack (m.mac_stack) in let (stqtqt3, b3) =
    pop_stack stqt3 in
    begin match (a4, b3) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt3 (Z.div aqt bqt)); mac_memory =
        (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Sarith SDIV ->
    let (stqt4, a5) = pop_stack (m.mac_stack) in let (stqtqt4, b4) =
    pop_stack stqt4 in
    begin match (a5, b4) with
    | (Some aqt, Some bqt) ->
      let usa = aqt in
      let usb = bqt in
      if Z.eq usb (Z.of_int 0) then begin
        { mac_stack = (push_stack stqtqt4 ((Z.of_int 0))); mac_memory =
          (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
          (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
          mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
          mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) } end
      else
      begin
        if
          (Z.eq usa (Z.negate (power (Z.of_int 2) (Z.of_int 255)))) && (Z.eq usb (Z.of_int (-1)))
          then begin
          { mac_stack =
            (push_stack stqtqt4
               ((Z.negate (power (Z.of_int 2) (Z.of_int 255)))));
            mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
            mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
            (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
            mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
            (m.mac_jumpmap) } end
        else
        begin
          if Z.gt usa (Z.of_int 0) then begin
            if Z.gt usb (Z.of_int 0) then begin
              { mac_stack = (push_stack stqtqt4 ((Z.ediv usa usb)));
                mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
                mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
                (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
                mac_gas = (m.mac_gas); mac_insts = (m.mac_insts);
                mac_jumpmap = (m.mac_jumpmap) } end
            else
            begin
              { mac_stack =
                (push_stack stqtqt4
                   ((Z.mul (Z.of_int (-1)) (Z.ediv usa (Z.mul (Z.of_int (-1)) usb)))));
                mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
                mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
                (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
                mac_gas = (m.mac_gas); mac_insts = (m.mac_insts);
                mac_jumpmap = (m.mac_jumpmap) } end end
          else
          begin
            if Z.gt usb (Z.of_int 0) then begin
              { mac_stack =
                (push_stack stqtqt4
                   ((Z.mul (Z.of_int (-1)) (Z.ediv (Z.mul (Z.of_int (-1)) usa) usb))));
                mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
                mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
                (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
                mac_gas = (m.mac_gas); mac_insts = (m.mac_insts);
                mac_jumpmap = (m.mac_jumpmap) } end
            else
            begin
              { mac_stack =
                (push_stack stqtqt4
                   ((Z.ediv (Z.mul (Z.of_int (-1)) usa) (Z.mul (Z.of_int (-1)) usb))));
                mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
                mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
                (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
                mac_gas = (m.mac_gas); mac_insts = (m.mac_insts);
                mac_jumpmap = (m.mac_jumpmap) } end end end end
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith MOD ->
    let (stqt5, a6) = pop_stack (m.mac_stack) in let (stqtqt5, b5) =
    pop_stack stqt5 in
    begin match (a6, b5) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt5 (Z.zmod aqt bqt)); mac_memory =
        (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Sarith SMOD ->
    let (stqt6, a7) = pop_stack (m.mac_stack) in let (stqtqt6, b6) =
    pop_stack stqt6 in
    begin match (a7, b6) with
    | (Some aqt, Some bqt) ->
      let usa = aqt in
      let usb = bqt in
      if Z.eq usb (Z.of_int 0) then begin
        { mac_stack = (push_stack stqtqt6 ((Z.of_int 0))); mac_memory =
          (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
          (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
          mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
          mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) } end
      else
      begin
        if Z.gt usa (Z.of_int 0) then begin
          if Z.gt usb (Z.of_int 0) then begin
            { mac_stack = (push_stack stqtqt6 ((Z.erem usa usb)));
              mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
              mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
              (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
              mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
              (m.mac_jumpmap) } end
          else
          begin
            { mac_stack =
              (push_stack stqtqt6
                 ((Z.mul (Z.of_int (-1)) (Z.erem usa (Z.mul (Z.of_int (-1)) usb)))));
              mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
              mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
              (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
              mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
              (m.mac_jumpmap) } end end
        else
        begin
          if Z.gt usb (Z.of_int 0) then begin
            { mac_stack =
              (push_stack stqtqt6
                 ((Z.mul (Z.of_int (-1)) (Z.erem (Z.mul (Z.of_int (-1)) usa) usb))));
              mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
              mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
              (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
              mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
              (m.mac_jumpmap) } end
          else
          begin
            { mac_stack =
              (push_stack stqtqt6
                 ((Z.erem (Z.mul (Z.of_int (-1)) usa) (Z.mul (Z.of_int (-1)) usb))));
              mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
              mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
              (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
              mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
              (m.mac_jumpmap) } end end end
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith ADDMOD ->
    let (stqt7, a8) = pop_stack (m.mac_stack) in let (stqtqt7, b7) =
    pop_stack stqt7 in let (stqtqtqt, c) = pop_stack stqtqt7 in
    begin match (a8, b7, c) with
    | (Some aqt, Some bqt, Some cqt) ->
      let usa = aqt in
      let usb = bqt in
      let usc = cqt in
      if Z.eq usc (Z.of_int 0) then begin
        { mac_stack = (push_stack stqtqtqt ((Z.of_int 0))); mac_memory =
          (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
          (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
          mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
          mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) } end
      else
      begin
        { mac_stack = (push_stack stqtqtqt ((Z.erem (Z.add usa usb) usc)));
          mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
          mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
          (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
          (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
          (m.mac_jumpmap) } end
    | (_, _, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith MULMOD ->
    let (stqt8, a9) = pop_stack (m.mac_stack) in let (stqtqt8, b8) =
    pop_stack stqt8 in let (stqtqtqt1, c1) = pop_stack stqtqt8 in
    begin match (a9, b8, c1) with
    | (Some aqt, Some bqt, Some cqt) ->
      let usa = aqt in
      let usb = bqt in
      let usc = cqt in
      if Z.eq usc (Z.of_int 0) then begin
        { mac_stack = (push_stack stqtqtqt1 ((Z.of_int 0))); mac_memory =
          (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
          (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
          mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
          mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) } end
      else
      begin
        { mac_stack = (push_stack stqtqtqt1 ((Z.erem (Z.mul usa usb) usc)));
          mac_memory = (m.mac_memory); mac_storage = (m.mac_storage);
          mac_pc = (Z.add (m.mac_pc) (Z.of_int 1)); mac_status =
          (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
          (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
          (m.mac_jumpmap) } end
    | (_, _, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith EXP ->
    let (stqt9, a10) = pop_stack (m.mac_stack) in let (stqtqt9, b9) =
    pop_stack stqt9 in
    begin match (a10, b9) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt9 ((power (aqt) (bqt)))); mac_memory =
        (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith LT ->
    let (stqt10, a11) = pop_stack (m.mac_stack) in let (stqtqt10, b10) =
    pop_stack stqt10 in
    begin match (a11, b10) with
    | (Some aqt, Some bqt) ->
      { mac_stack =
        (push_stack stqtqt10
           (if Z.lt aqt bqt then begin (Z.of_int 1) end
            else
            begin
              (Z.of_int 0) end)); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith GT ->
    let (stqt11, a12) = pop_stack (m.mac_stack) in let (stqtqt11, b11) =
    pop_stack stqt11 in
    begin match (a12, b11) with
    | (Some aqt, Some bqt) ->
      { mac_stack =
        (push_stack stqtqt11
           (if Z.gt aqt bqt then begin (Z.of_int 1) end
            else
            begin
              (Z.of_int 0) end)); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Storage SSTORE ->
    let (stqt12, a13) = pop_stack (m.mac_stack) in let (stqtqt12, b12) =
    pop_stack stqt12 in
    begin match (a13, b12) with
    | (Some aqt, Some bqt) ->
      { mac_stack = stqtqt12; mac_memory = (m.mac_memory); mac_storage =
        (update_storage_from_address (m.mac_storage) aqt bqt); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (Z.sub (m.mac_gas) (Z.of_int 20000)); mac_insts = (m.mac_insts);
        mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Sarith SLT ->
    let (stqt13, a14) = pop_stack (m.mac_stack) in let (stqtqt13, b13) =
    pop_stack stqt13 in
    begin match (a14, b13) with
    | (Some aqt, Some bqt) ->
      let usa = aqt in
      let usb = bqt in
      { mac_stack =
        (push_stack stqtqt13
           (if Z.lt usa usb then begin (Z.of_int 1) end
            else
            begin
              (Z.of_int 0) end)); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Sarith SGT ->
    let (stqt14, a15) = pop_stack (m.mac_stack) in let (stqtqt14, b14) =
    pop_stack stqt14 in
    begin match (a15, b14) with
    | (Some aqt, Some bqt) ->
      let usa = aqt in
      let usb = bqt in
      { mac_stack =
        (push_stack stqtqt14
           (if Z.gt usa usb then begin (Z.of_int 1) end
            else
            begin
              (Z.of_int 0) end)); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Stack POP ->
    let (stqt15, a16) = pop_stack (m.mac_stack) in
    begin match a16 with
    | Some _ ->
      { mac_stack = stqt15; mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Stack PUSH a17 ->
    inc_pc ({ mac_stack = (push_stack (m.mac_stack) a17); mac_memory =
              (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
              (m.mac_pc); mac_status = (m.mac_status); mac_memory_usage =
              (m.mac_memory_usage); mac_gas =
              (Z.sub (m.mac_gas) (Z.of_int 3)); mac_insts = (m.mac_insts);
              mac_jumpmap = (m.mac_jumpmap) })
  | Some Dup i ->
    let ele = List.nth_opt (m.mac_stack) (Z.to_int (Z.sub (i) (Z.of_int 1))) in
    begin match ele with
    | Some a17 ->
      { mac_stack = (push_stack (m.mac_stack) a17); mac_memory =
        (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Swap i ->
    let s = swap_stack (m.mac_stack) (i) in
    begin match s with
    | Some uss ->
      { mac_stack = uss; mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Pc JUMP ->
    let (sqt, dest) = pop_stack (m.mac_stack) in
    begin match dest with
    | Some usdest ->
      { mac_stack = sqt; mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = ((m.mac_jumpmap) usdest); mac_status =
        (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (m.mac_pc); mac_status =
        (Error OutOfStack); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    end
  | Some Pc JUMPI ->
    let (sqt1, dest1) = pop_stack (m.mac_stack) in let (sqtqt, con) =
    pop_stack sqt1 in
    begin match (dest1, con) with
    | (Some usdest, Some uscon) ->
      if Z.eq uscon ((Z.of_int 0)) then begin
        inc_pc ({ mac_stack = sqtqt; mac_memory = (m.mac_memory);
                  mac_storage = (m.mac_storage); mac_pc = (m.mac_pc);
                  mac_status = (m.mac_status); mac_memory_usage =
                  (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
                  (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }) end
      else
      begin
        { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory);
          mac_storage = (m.mac_storage); mac_pc = ((m.mac_jumpmap) usdest);
          mac_status = (m.mac_status); mac_memory_usage =
          (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
          (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) } end
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (m.mac_pc); mac_status =
        (Error OutOfStack); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    end
  | Some Pc JUMPDEST -> inc_pc m
  | Some Memory MSTORE ->
    let (sqt2, offset) = pop_stack (m.mac_stack) in let (sqtqt1, cont) =
    pop_stack sqt2 in
    begin match (offset, cont) with
    | (Some o, Some c2) ->
      { mac_stack = sqtqt1; mac_memory =
        (update_memory_content (m.mac_memory) o (Some (Item256 c2)));
        mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Memory MSTORE8 ->
    let (sqt3, offset1) = pop_stack (m.mac_stack) in let (sqtqt2, cont1) =
    pop_stack sqt3 in
    begin match (offset1, cont1) with
    | (Some o, Some c2) ->
      { mac_stack = sqtqt2; mac_memory =
        (update_memory_content (m.mac_memory) o (Some (Item8 c2)));
        mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Stack CALLDATALOAD ->
    let (sqt4, offset2) = pop_stack (m.mac_stack) in
    begin match offset2 with
    | Some _ ->
      { mac_stack = (push_stack sqt4 ((Z.of_int 1))); mac_memory =
        (m.mac_memory); mac_storage = (m.mac_storage); mac_pc =
        (Z.add (m.mac_pc) (Z.of_int 1)); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
        (m.mac_storage); mac_pc = (Z.add (m.mac_pc) (Z.of_int 1));
        mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | _ -> assert false (* absurd *)
    end
  | _ ->
    { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_storage =
      (m.mac_storage); mac_pc = (m.mac_pc); mac_status =
      (Error InvalidOpcode); mac_memory_usage = (m.mac_memory_usage);
      mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
      (m.mac_jumpmap) }
  end

