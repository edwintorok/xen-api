open Sexplib.Std
open Xcp_pci
open Uuidm_rpc_type

module TopLevel = struct
  type power_state = Running | Halted | Suspended | Paused
  [@@deriving sexp, rpcty]

  type disk =
    | Local of string  (** path to a local block device *)
    | VDI of string  (** typically "SR/VDI" *)
  [@@deriving sexp, rpcty]
end

module Vgpu = struct
  type gvt_g = {
      physical_pci_address: address option  (** unused; promoted to Vgpu.t *)
    ; low_gm_sz: int64
    ; high_gm_sz: int64
    ; fence_sz: int64
    ; monitor_config_file: string option
  }
  [@@deriving sexp, rpcty]

  (** Example for nvidia: { physical_pci_address : None config_file: None
      virtual_pci_address : Some {domain; bus; device; fn} type_id: 45 uuid:
      aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee extra_args:key1=v1,key2=v2,key3=v3 } *)
  type nvidia = {
      physical_pci_address: address option  (** unused; promoted to Vgpu.t *)
    ; config_file: string option
    ; virtual_pci_address: address
          [@default {domain= 0000; bus= 0; dev= 11; fn= 0}]
    ; type_id: string option
    ; uuid: string option
    ; vclass: string option  (** from vgpu: Compute, NVS, Quadro *)
    ; extra_args: string [@default ""]
          (** string is passed on as is and no structure is assumed *)
  }
  [@@deriving sexp, rpcty]

  type mxgpu = {
      physical_function: address option  (** unused; promoted to Vgpu.t *)
    ; vgpus_per_pgpu: int64
    ; framebufferbytes: int64
  }
  [@@deriving sexp, rpcty]
end

module Nvram_uefi_variables = struct
  type onboot = Persist | Reset [@@deriving rpcty, sexp]

  type t = {
      on_boot: onboot [@default Persist]
    ; backend: string [@default "xapidb"]
  }
  [@@deriving rpcty, sexp]

  let default_t =
    match Rpcmarshal.unmarshal t.Rpc.Types.ty Rpc.(Dict []) with
    | Ok x ->
        x
    | Error (`Msg m) ->
        failwith
          (Printf.sprintf "Error creating Nvram_uefi_variables.default_t: %s" m)
end

module PCITopology = struct
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
      let test_data = List.init 32 Fun.id in
      abstract ~name:"Slot" ~test_data of_int to_int Rpc.Types.int sexp_of_int int_of_sexp

    let typ_of = t.ty
  end

  module SlotMap = MakeMap (Slot)

  (** a PCI(e) bus contains 32 {type:Slot.t} *)
  type 'a bus = 'a SlotMap.t [@@deriving sexp]

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
      let test_data = List.init 8 Fun.id in
      abstract ~name:"Function" ~test_data of_int to_int Rpc.Types.int sexp_of_int
        int_of_sexp

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

  (** A virtual device id {!type:Vif.id} or {!type:Vgpu.id} *)
  type device_id = string * string [@@deriving rpcty, sexp]

  (** emulated network card type *)
  type nic_type = E1000 | RTL8139 [@@deriving rpcty, sexp]

  (** emulated or pass-through PCI functions *)
  type pci_function =
    | EmulatedNIC of nic_type * device_id (* emulated network card, NOT SRIOV *)
    | EmulatedVGPU of device_id (* emulated vGPU, NOT SRIOV *)
    | NVME
        (** emulated NVME controller (used by qemu-upstream-uefi device model) *)
    | XenPV
    | XenPlatform
    | HostPCI of Xcp_pci.address  (** pass-through host PCI device function *)
  [@@deriving rpcty, sexp]

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

  (** a PCI bus containing PCI devices *)
  type pci_bus = pci bus [@@deriving sexp]

  let pci_bus = SlotMap.t pci

  let typ_of_pci_bus = pci_bus.ty

  (** {2 PCIe functions, devices and buses (not implemented yet) *)

  (** emulated or pass-through PCIe functions *)
  type pcie_function =
    | EHCI (* USB, not implemented *)
    | UHCI (* USB, not implemented *)
    | HostPCIe of Xcp_pci.address (* not implemented *)
  [@@deriving rpcty, sexp]

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
  [@@deriving rpcty, sexp]

  (** a PCIe bus containing PCI devices *)
  type pcie_bus = pcie bus [@@deriving sexp]

  let pcie_bus = SlotMap.t pcie

  let typ_of_pcie_bus = pcie_bus.ty

  (** {2 PCI(e) host bridge} *)

  (** The root of a PCI(e) topology *)
  type t =
    | PCI_host_i440FX of pci_bus  (** an i440FX machine, emulated by QEMU *)
    | PCI_host_q35 of pcie_bus
        (** a Q35 machine, emulated by QEMU. Not implemented. *)
  [@@deriving rpcty, sexp]
end

module Vm = struct
  type igd_passthrough = GVT_d [@@deriving rpcty, sexp]

  type video_card =
    | Cirrus
    | Standard_VGA
    | Vgpu
    | IGD_passthrough of igd_passthrough
  [@@default Cirrus] [@@deriving rpcty, sexp]

  type firmware_type = Bios | Uefi of Nvram_uefi_variables.t
  [@@deriving rpcty, sexp]

  let default_firmware = Bios [@@deriving rpcty]

  type tpm = Vtpm of Uuidm.t [@@deriving rpcty, sexp]

  type hvm_info = {
      hap: bool [@default true]
    ; shadow_multiplier: float [@default 1.0]
    ; timeoffset: string [@default ""]
    ; video_mib: int [@default 4]
    ; video: video_card [@default Cirrus]
    ; acpi: bool [@default true]
    ; serial: string option [@default None]
    ; keymap: string option [@default None]
    ; vnc_ip: string option [@default None]
    ; pci_emulations: string list [@default []]
    ; pci_passthrough: bool [@default false]
    ; boot_order: string [@default ""]
    ; qemu_disk_cmdline: bool [@default false]
    ; qemu_stubdom: bool [@default false]
    ; firmware: firmware_type [@default default_firmware]
    ; tpm: tpm option [@default None]
  }
  [@@deriving rpcty, sexp]

  type pv_direct_boot = {
      kernel: string [@default ""]
    ; cmdline: string [@default ""]
    ; ramdisk: string option [@default None]
  }
  [@@deriving rpcty, sexp]

  type pv_indirect_boot = {
      bootloader: string [@default ""]
    ; extra_args: string [@default ""]
    ; legacy_args: string [@default ""]
    ; bootloader_args: string [@default ""]
    ; devices: TopLevel.disk list [@default []]
  }
  [@@deriving rpcty, sexp]

  type pv_boot = Direct of pv_direct_boot | Indirect of pv_indirect_boot
  [@@deriving rpcty, sexp]

  type pv_info = {
      boot: pv_boot
    ; framebuffer: bool [@default true]
    ; framebuffer_ip: string option [@default None]
    ; vncterm: bool [@default true]
    ; vncterm_ip: string option [@default None]
    ; pci_passthrough: bool [@default false]
  }
  [@@deriving rpcty, sexp]

  type builder_info =
    | HVM of hvm_info
    | PV of pv_info
    | PVinPVH of pv_info
    | PVH of pv_info
  [@@deriving rpcty, sexp]

  type id = string [@@deriving rpcty, sexp]

  type action = Coredump | Shutdown | Start | Pause | Softreboot
  [@@deriving rpcty, sexp]

  type scheduler_params = {
      priority: (int * int) option  (** weight, cap *)
    ; affinity: int list list  (** vcpu -> pcpu list *)
  }
  [@@deriving rpcty, sexp]

  type device_id = string * string [@@deriving rpcty, sexp]

  type nic_type = E1000 | RTL8139 [@@deriving rpcty, sexp]

  type pci_function =
    | EmulatedNIC of nic_type * device_id (* note: NOT SRIOV *)
    | EmulatedGPU of device_id (* note: NOT SRIOV *)
    | NVME
    | XenPV
    | XenPlatform
    | HostPCI of Xcp_pci.address
  [@@deriving rpcty, sexp]

  type t = {
      id: id
    ; name: string [@default "unnamed"]
    ; ssidref: int32
    ; xsdata: (string * string) list
    ; platformdata: (string * string) list
    ; bios_strings: (string * string) list
    ; ty: builder_info
    ; suppress_spurious_page_faults: bool
    ; machine_address_size: int option
    ; memory_static_max: int64
    ; memory_dynamic_max: int64
    ; memory_dynamic_min: int64
    ; vcpu_max: int  (** boot-time maximum *)
    ; vcpus: int  (** ideal number to use *)
    ; scheduler_params: scheduler_params
    ; on_crash: action list
    ; on_shutdown: action list
    ; on_reboot: action list
    ; on_softreboot: action list [@default [Softreboot]]
    ; pci_msitranslate: bool
    ; pci_power_mgmt: bool
    ; has_vendor_device: bool [@default false]
    ; generation_id: string option
    ; guest_pci_topology: PCITopology.t option [@default None]
  }
  [@@deriving rpcty, sexp]

  type console_protocol = Rfb | Vt100 [@@deriving rpcty, sexp]

  type console = {protocol: console_protocol; port: int; path: string}
  [@@deriving rpcty, sexp]

  type domain_type =
    | Domain_HVM
    | Domain_PV
    | Domain_PVinPVH
    | Domain_PVH
    | Domain_undefined
  [@@deriving rpcty, sexp]

  type state = {
      power_state: TopLevel.power_state
    ; domids: int list
    ; consoles: console list
    ; memory_target: int64
    ; memory_actual: int64
    ; memory_limit: int64
    ; vcpu_target: int  (** actual number of vcpus *)
    ; shadow_multiplier_target: float  (** actual setting *)
    ; rtc_timeoffset: string
    ; uncooperative_balloon_driver: bool
    ; guest_agent: (string * string) list
    ; xsdata_state: (string * string) list
    ; pv_drivers_detected: bool
    ; last_start_time: float
    ; hvm: bool
    ; nomigrate: bool  (** true means VM must not migrate *)
    ; nested_virt: bool  (** true means VM uses nested virtualisation *)
    ; domain_type: domain_type
    ; featureset: string
  }
  [@@deriving rpcty, sexp]
end
