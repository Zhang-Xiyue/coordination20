open Evm;;


let empty_vm () : machine_state = {
    mac_stack = [];
    mac_storage = Storage.StringMap.empty;
    mac_memory = (fun _ -> None);
    mac_pc = Z.zero;
    mac_status = Running;
    mac_memory_usage = Z.zero;
    mac_gas = Z.zero;
    mac_insts = [];
    mac_jumpmap = (fun _ -> Z.zero)
}

let init_vm insts jumpmap : machine_state = {
    mac_stack = [];
    mac_storage = Storage.StringMap.empty;
    mac_memory = (fun _ -> None);
    mac_pc = Z.zero;
    mac_status = Running;
    mac_memory_usage = Z.zero;
    mac_gas = Z.zero;
    mac_insts = insts;
    mac_jumpmap = jumpmap
}

let print_vm (vm: machine_state) : unit =
    Printf.printf "VM [%d Instructions]\n" (List.length vm.mac_insts)

let run_vm (vm: machine_state) : machine_state =
    let rec run vm =
        let next = (interpreter vm) in
        match (next.mac_status) with
        | Running -> (run next)
        | _ -> next
    in
    let final = run vm in
    final

let json_of_storage (storage: Storage.t) : Yojson.Basic.json = 
    let lst = Storage.StringMap.fold (fun key value prev -> begin
            let rawval = (Z.to_bytes value) in
            let retval = if String.length rawval < 64 then begin
                (String.make (64 - (String.length rawval)) '0') ^ rawval
            end else
                String.sub rawval ((String.length rawval) - 64) 64
            in
            List.cons (
                (if key == "" then key else "00"),
                `String retval
                ) prev
        end) storage []
    in
    `Assoc lst

let json_of_vm (vm: machine_state) : Yojson.Basic.json =
    let open Yojson.Basic in
    `Assoc [
        ("storage", json_of_storage vm.mac_storage);
        ("gas", `String (Big_int.string_of_big_int vm.mac_gas));
    ]