(**
  One module per QEMU device

  Record fields are all the qemu parameters that change visible hardware inside the guest, except the PCI address.
  The PCI address is stored in the parent map

  When converting to QEMU command-line arguments more flags can be added (or even changed between XAPI versions) as long as they don't influence visible hardware,
  e.g. suppress-vmdesc=on

*)

open Sexplib.Std

let flag_of_ocaml = function
  | '_' -> '-'
  | c -> c

let rec cli_of_rpc =
 let open Rpc in
 function
  | Int i64 -> Printf.sprintf "%Lu" i64
  | Int32 i32 -> Printf.sprintf "0x%04lx" i32
  | Bool true -> "on" 
  | Bool false -> "off"
  | Float f -> Printf.sprintf "%.16g" f
  | String s | DateTime s -> s
  | Enum lst -> lst |> List.map cli_of_rpc |> String.concat ","
  | Dict lst -> lst |> List.map (fun (k,v) ->
       let k = k |> String.map flag_of_ocaml in
       Printf.sprintf "%s=%S" k (cli_of_rpc v)
      ) |> String.concat "," 
  | Base64 _ -> failwith "Binary data cannot be passed to QEMU command-line"
  | Null -> ""


let to_cli ?(extra=[]) ?(extra_kv=[]) modname typ t =
  let open Rpclib in
  let flag = "-" ^ modname |> String.lowercase_ascii |> String.map flag_of_ocaml in
  let extra =
     List.concat
      [ extra; extra_kv |> List.map (fun (k,v) -> Printf.sprintf "%s=%S" k v) ]
     |> String.concat "," in
  [flag; extra ^ (t |> Rpcmarshal.marshal typ |> cli_of_rpc)]

type guest_pci_address =
  { bus: int option (** PCI bus, we only support bus 0 for now *)
  ; slot: int (** PCI device number *)
  ; fn: int option (** optional PCI function number (defaults to 0) *)
  }  [@@deriving rpcty, sexp]

let string_of_guest_pci_address addr =
  let bus = Option.value addr.bus ~default:0 in
  let fn = Option.value addr.fn ~default:0 in
  if bus <> 0 then
    Fmt.invalid_arg "Only bus 0 is supported, got %04x:%02x.%x" bus addr.slot fn;
  Printf.sprintf "%02u.%u" addr.slot (Option.value addr.fn ~default:0)

let guest_pci_address_of_string str =
  Scanf.sscanf str "%02x:%02x.%x" (fun bus slot fn ->
      { bus = Some bus; slot; fn = Some fn}
  )
  
let device ~addr:(addr, multifunction) ?id ?(extra=[]) driver =
   let extra = ("addr", string_of_guest_pci_address addr) :: ("multifunction", if multifunction then "on" else "off") :: extra in
   let extra_kv = match id with None -> extra | Some id -> ("id", id) :: extra in
   to_cli ~extra:[driver] ~extra_kv "device"
 
module Usb_tablet = struct
  type t = {
    port: int
  }
  [@@deriving rpcty, sexp]

  let to_cli = device __MODULE__ typ_of
end

module Xen_platform = struct
  type t =
  { device_id: int32
  ; revision: int32 option
  ; class_id: int32 option
  ; subvendor_id: int32 option
  ; subsystem_id: int32 option
  }
  [@@deriving rpcty, sexp]

  let to_cli = device __MODULE__ typ_of
end

module XenPV = struct
  type t =
  { device_id: int32  
  }
  [@@deriving rpcty, sexp]

  let to_cli = device __MODULE__ typ_of
end

module Net = struct
  type nic_type = RTL8139 | E1000
  [@@deriving rpcty, sexp]

  type t =
  { driver: nic_type
  }
  [@@deriving rpcty, sexp]

  (* TODO: use devid *)
  let to_cli ~devid:_ = device __MODULE__ typ_of
end

(** A virtual device id {!type:Vif.id} or {!type:Vgpu.id} *)
type device_id = string * string [@@deriving rpcty, sexp]

module EmulatedVGPU = struct
  type t = unit
  [@@deriving rpcty, sexp]

  let to_cli ~devid:_ _t = ["-vgpu"] (* TODO *)
end

module NVME = struct
  type t = unit
  [@@deriving rpcty, sexp]

  let id = "nvme0"

  let to_cli = device ~id ~extra:["serial", id] __MODULE__ typ_of
end

module USB_EHCI = struct
  type t = unit

  [@@deriving rpcty, sexp]

  let id = "ehci"

  let to_cli = device ~id  __MODULE__ typ_of
end

module USB_XHCI = struct
  type t = unit

  [@@deriving rpcty, sexp]

  let id = "xhci"

  let to_cli = device ~id  __MODULE__ typ_of
end

type pci_function =
  | EmulatedVGPU of device_id * EmulatedVGPU.t (** emulated VGPU, NOT SRIOV *)
  | HostPCI of Xcp_pci.address (** pass-through host PCI device/function *)
  | Net of device_id * Net.t (** emulated NIC, NOT SRIOV *)
  | NVME of NVME.t (** emulated NVME controller *)
  | XenPV of XenPV.t (** PCI device for has-vendor-device *)
  | XenPlatform of Xen_platform.t (** PCI device for unplugging emulated devices and logging *)
  | EHCI of USB_EHCI.t (** USB controller *)
  | XHCI of USB_XHCI.t (** USB controller, not implemented *)
  [@@deriving rpcty, sexp]

let cli_of_pci_function ~addr = function
  | EmulatedVGPU (devid, e) -> EmulatedVGPU.to_cli ~devid e
  | HostPCI _addr -> [] (* TODO: this is QMP *)
  | Net (devid, e) -> Net.to_cli ~addr ~devid e
  | NVME nvme -> NVME.to_cli ~addr nvme
  | XenPV d -> XenPV.to_cli ~addr d
  | XenPlatform d -> Xen_platform.to_cli ~addr d
  | EHCI d -> USB_EHCI.to_cli ~addr d
  | XHCI d -> USB_XHCI.to_cli ~addr d

type pcie_function =
  | HostPCIe of Xcp_pci.address (* not implemented *)
  [@@deriving rpcty, sexp]

let cli_of_pcie_function ~addr:_ = function
  | HostPCIe _addr -> [] (* TODO: this is QMP *)

open TypeCombinators

(** {2 PCI(e) Slots} *)

(** PCI(e) device number (slot) *)
module Slot : sig
  (** a PCI device number slot. 0 to 0x1f *)
  type t = private int

  val of_int : int -> t
  (** [of_int slot] validates that [slot] is between [0] and [0x1f], inclusive.
  @raises Invalid_argument otherwise *)

  val to_int : t -> int
  (** [to_int t] converts [t] back to an integer. *)

  include OrderedType with type t := t
end = struct
  type t = int

  let of_int t =
    if t >= 0 && t <= 0x1f then
      t
    else
      invalid_arg
        (Printf.sprintf "PCI device number %d must be >= 0 and <= 0x1f" t)

  let to_int t = t

  let compare = Int.compare

  let t, sexp_of_t, t_of_sexp =
    let test_data = [0; 0x1f] in
    abstract ~name:"Slot" ~test_data of_int to_int Rpc.Types.int sexp_of_int
      int_of_sexp

  let typ_of = t.ty
end

module SlotMap = MakeMap (Slot)

(** a PCI(e) bus contains 32 {type:Slot.t} *)
type 'a bus32 = 'a SlotMap.t [@@deriving sexp]
    (** {2 PCI(e) Functions} *)

    (** PCI(e) function number *)
    module Function : sig
      (** a PCI function 0 to 7 *)
      type t = private int

      val of_int : int -> t
      (** [of_int fn] validates that [fn] is between [0] and [7], inclusive.
      @raises Invalid_argument otherwise *)

      val to_int : t -> int
      (** [to_int t] converts [t] back to an integer. *)

      include OrderedType with type t := t
    end = struct
      type t = int

      let of_int t =
        if t >= 0 && t <= 7 then
          t
        else
          invalid_arg (Printf.sprintf "PCI function %d must be >= 0 and <= 7" t)

      let to_int t = t

      let compare = Int.compare

      let t, sexp_of_t, t_of_sexp =
        let test_data = [0; 7] in
        abstract ~name:"Function" ~test_data of_int to_int Rpc.Types.int
          sexp_of_int int_of_sexp

      let typ_of = t.ty
    end

    module FunctionMap = MakeMap (Function)

    (** Map of PCI function to the PCI device that we emulate or pass-through in that function.
      Function 0 is always present, ensured by the tuple type.
   *)
    type 'a function_map = 'a * 'a FunctionMap.t [@@deriving sexp]

    (* deriving rpcty doesn't know how to do this on its own *)
    let function_map ?name t = pair ?name (t, FunctionMap.t t)

    (** {2 PCI functions, devices and buses *)

    (** A single or multi-function PCI device *)
    type pci_device = pci_function function_map [@@deriving sexp]

    (* deriving rpcty doesn't know how to do this on its own *)
    let pci_device = function_map pci_function

    let typ_of_pci_device = pci_device.ty

    (** PCI devices that can be placed directly on a host bridge *)
    type pci =
      | Device of pci_device
          (** potentially multifunction device, with function 0 always be present *)
      | I440FX  (** i440FX host bridge *)
      | PIIX3  (** i440FX PIIX3 device, containing ISA/IDE/USB functions *)
      | VGA
          (** video card, type is already stored in [video] field of {!type:Vm.t} *)
      | MCH (* Q35 host bridge, not implemented *)
      | ICH9  (** Q35 ICH9 with ISA/IDE functions, not implemented *)
    [@@deriving rpcty, sexp]

    let cli_of_device ~slot cli_of_fn (f0, other) : string list =
     let addr = {bus = None; slot; fn = Some 0 }in
      if FunctionMap.is_empty other then cli_of_fn ~addr:(addr,false) f0
      else
        let f0 = cli_of_fn ~addr:(addr,true) f0 in
        let other = other |> FunctionMap.to_seq |> Seq.map (fun (fn, dev) ->
          let addr = { addr with fn = Some (Function.to_int fn)} in
          cli_of_fn  ~addr:(addr,true) dev
        ) |> List.of_seq in
        List.concat (f0 :: other)

    let cli_of_pci ~slot = function
      | Device d -> cli_of_device ~slot cli_of_pci_function d
      | VGA -> ["-vga"]
      | I440FX | PIIX3 | MCH | ICH9 -> [] (* default devices *)

    (** a PCI bus containing PCI devices *)
    type pci_bus = pci bus32 [@@deriving sexp]

    let pci_bus = SlotMap.t pci

    let typ_of_pci_bus = pci_bus.ty

    (** {2 PCIe functions, devices and buses (not implemented yet) *)
    (** A single or multi-function PCIe device *)
    type pcie_device = pcie_function function_map [@@deriving sexp]

    (* deriving rpcty doesn't know how to do this on its own *)
    let pcie_device = function_map pcie_function

    let typ_of_pcie_device = pcie_device.ty

    (** PCIe devices that can be placed directly on a host bridge *)
    type pcie =
      | Device of
          pcie_device (* integrated root complex function, not implemented *)
      | PCIeRootPort of pcie_device (* not implemented *)
      | PCI of pci_device (** PCI device plugged as integrated endpoint *)
    [@@deriving rpcty, sexp]

    let cli_of_pcie ~slot = function
      | Device d -> cli_of_device ~slot cli_of_pcie_function d
      | PCI pci -> cli_of_device ~slot cli_of_pci_function pci

    (** a PCIe bus containing PCI devices *)
    type pcie_bus = pcie bus32 [@@deriving sexp]

    let pcie_bus = SlotMap.t pcie

    let typ_of_pcie_bus = pcie_bus.ty


module Machine = struct
  (** QEMU machine type, to ensure backward compatibility we explicitly store the qemu version,
       so that when qemu is upgraded we know which version each VM came from.
      QEMU already has backwards compatibility code that ensures emulated hardware stays compatible with that version

      This is the root of the PCI(e) topology
  *)
  type machine =
    | PC_010 of pci_bus [@name "pc-0.10"] (** deprecated, for qemu-upstream-compat and qemu-upstream-uefi *)
    | PC_I440FX_4_2 of pci_bus [@name "pc-i440fx-4.2"]  (** i440FX hardware for current QEMU version *)
    | PC_Q35_4_2 of pcie_bus [@name "pc-q35-4.2"] (** not implemented, Q35 hardware for current QEMU version *)
    [@@deriving rpcty, sexp]

  let cli_of_slotmap cli_of map =
    map |> SlotMap.to_seq |> Seq.map (fun (slot, dev) ->
      cli_of ~slot:(Slot.to_int slot) dev
    ) |> List.of_seq

  let cli_of_bus = function
    | (PC_010 b | PC_I440FX_4_2 b) -> cli_of_slotmap cli_of_pci b
    | PC_Q35_4_2 b -> cli_of_slotmap cli_of_pcie b

  type machine_flags =
  { trad_compat: bool
  ; usb: bool
  (* TODO: max-ram-below-4g is calculated by xenguest *)
  }
    [@@deriving rpcty, sexp]

  type t = machine * machine_flags [@@deriving rpcty, sexp]

  (** all functions on the device are reserved for emulated HW belonging to the emulated machine *)
  let reserved_pci_devices = function
    | PC_010 _ | PC_I440FX_4_2 _ -> [0, I440FX; 1, PIIX3]
    | PC_Q35_4_2 _ -> failwith "Q35 is not implemented yet"


  let to_cli (machine, flags) =
    let open Rpclib in
    let flags = flags |> Rpcmarshal.marshal typ_of_machine_flags |> cli_of_rpc in
    let bus_cli = cli_of_bus machine in
    let machine =
      match machine |> Rpcmarshal.marshal typ_of_machine with
      | Rpc.Dict [(key, _)] -> key
      | _ -> assert false
    in
    let machine = ["-machine"; String.concat "," [machine; flags]] in
    machine :: bus_cli
    |> List.concat
    
end

