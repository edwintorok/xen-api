+++
title = "Guest PCI topology"
+++

## Guest PCI topology

When a VM boots we may assign it various emulated or pass-through PCI devices.
Just like with real hardware these virtual PCI devices are on a (virtual) PCI bus, "plugged" into a specific slot and have a (virtual) PCI address in [Bus:Device.Function](https://wiki.xenproject.org/wiki/Bus:Device.Function_(BDF)_Notation) notation, and just like with real hardware the guest operating system may not behave well if their location suddenly changes across a reboot (e.g. Windows drivers may BSOD).

### Background (technical debt)

We currently support 3 `platform:device-model`:

* `qemu-none`: when no QEMU is needed. E.g. PV or PVH VMs without pass-through
* `qemu-upstream-compat`: virtual hardware backwards compatible with old BIOS VMs (supported on both BIOS and UEFI)
* `qemu-upstream-uefi`: virtual hardware supported only on UEFI VMs, attempts to address some of the technical debt from `qemu-upstream-compat`

The PCI devices on `qemu-upstream-compat` looked like this:

* 00:00.0: i440FX Host bridge
* 00:01.0: PIIX3 (ISA/IDE/USB)
* 00:02.0: VGA or Xen PV
* 00:03.0: Xen platform, or Xen PV
* 00:04.0 - 00:0a.0: (emulated) NIC, max 8 (devid+4)
* 00:0b.0+: NVidia vGPU (index+11), and other pass-through devices

This has several issues:
* although claims to support 8 NICs, we can only support 7 due to conflict on PCI address `00:0b.0` when NVidia vGPUs are used
* the "Xen PV" device doesn't have a fixed place, and moves around:
  * e.g. moving from emulated VGA to NVidia vGPU moves the address of the Xen PV device around which may cause a crash in the Windows PV drivers
  * setting `has-vendor-device` makes this device appear/disappear
* you cannot use other pass-through devices together with vGPUs: they'll attempt to claim the same virtual PCI addresses
* can only use at most 7 SRIOV NICs (TBC, do they share the address of the emulated ones?)
* there are only 32 devices in total (and Xen would only support 1 PCI bus), which limits the number of pass-through devices
* other pass-through devices are handled by qemu without explicit placement, their address is "wherever qemu happens to have placed them" (and hope that doesn't change across qemu versions)

To address the biggest issue (crashes caused by PCI devices moving around) the next device model attempted to use a static PCI address assignment model in `qemu-upstream-uefi`:

* 00:00.0: i440FX Host bridge
* 00:01.0: PIIX3 (ISA/IDE/USB)
* 00:02.0: VGA or empty
* 00:03.0: Xen platform or empty
* 00:04.0 - 00:05.0: (emulated) NIC, max 2 (devid+4)
* 00:06.0: Xen PV
* 00:07.0: NVME controller or empty (max 1)
* 00:08.0+: other pass-through devices
* 00:0b.0+: NVidia vGPU (index+11)

However there are still several issues, and some new ones:
* we now only support 2 emulated NICs, but continue to support 7 VIFs. This introduces a bug when upgrading Windows PV drivers, which copy the IP address from the virtual device to the emulated device on upgrade, and when there is no corresponding emulated device they lose the IP address setting completely.
* the 2 NIC limit also applies to SRIOV (TBC)
* it is now possible to mix other pass-through devices and vGPUs, but only 3, 4+ devices would conflict with the first NVidia vGPU
* the number 11 for the NVidia vGPU start slot is hardcoded in XAPI, but XAPI has no direct knowledge of where xenopsd will place devices
* we are still limited to 32 devices in total
* XAPI has no knowledge of where each (virtual) PCI device got placed, only xenopsd knows this and the information is only saved during migration and suspend/resume. The VM object in XAPI doesn't retain the assignment, and relies on the assignment algorithm staying exactly the same across releases to avoid breaking backwards compatibility

### Design constraints

In addition to fixing the above mentioned technical debt we want to improve the device model to also support these features:

* more pass-through PCI devices (at least 64), including arbitrary non-NIC devices, e.g. Intel QAT
  * this can be achieved by using multifunction devices, i.e. using all 8 functions on a PCI device instead of just function 0
  * could also try to use PCI-to-PCI bridges, but this would result in a non-flat PCI topology and complications with PCI addresses (which would be different on the bridge and on the host)
* same limits on SRIOV NICs as with arbitrary PCI pass-through devices
* Q35 device model, which would provide better PCIe support (we currently plug pass-through PCIe devices into a PCI slot, which limits their functionality)
* PVH with pass-through may benefit Tfrom a minimal device model with only enough virtual devices to make pass-through work (PVH without passthrough can use `qemu-none` as before)

To address the technical debt we need to follow these principles:

* (already ensured by xenopsd) virtual hardware must stay exactly the same when the VM is migrated
* virtual PCI devices that were seen by the guest do not change address when an unrelated virtual PCI device is added or removed
* we explicitly save in the XAPI database the location where each virtual PCI is in the guest, and persist this across reboots too
* we need to support the same number of (non-SRIOV) VIFs and emulated NICs
* SRIOV VIFs should not use the same addresses as emulated NICs
* XAPI should pick virtual PCI addresses such that they do not conflict, instead of using hardcoded numbers
* it should be easier to experiment with various PCI device layouts and topologies without changing the code

Using multi-function devices comes with additional constraints:

* we need to plug the functions in reverse order, i.e. function 0 must be plugged last
* if any functions are used in a PCI device then function 0 must always be present
* we can't support hotplug of functions on functions other than 0, but we don't support PCI hotplug on a running VM anyway (xenopsd does use QEMU's hotplug API to add devices, but the VM is not running at that point yet)

A new device-model should be introduced, `qemu-upstream-map` which explicitly stores the used PCI topology.
For backwards compatibility we could transparently upgrade `qemu-upstream-uefi` models (or `qemu-upstream-compat` without `other-config:pci` pass-through devices), but initially those device models will be retained exactly as is.

With the improved virtual PCI address assignment the following topology could be used:

* 00:00.0: i440FX Host bridge
* 00:01.0: PIIX3 (ISA/IDE/USB)
* 00:02.0: VGA or empty
* 00:03.0: Xen platform
*  00:03.1: Xen PV or empty
* 00:04.0: NVME controller
* 00:04.1-7: emulated NIC, max 7 (fn = devid+1)
* 00:05.0+: NVidia vGPU or other pass-through devices, assigned on a first-come first-served basis.

This assignment would support 216 pass-through devices, however if we try to do something similar for BIOS VM (where you'd assume some very old OS wouldn't have NVME drivers), then we wouldn't have a convenient fixed function to use for device 0 (Xen platform could be used, but then Xen PV would need to be separate, or when removing the platform device you'd also lose your emulated devices).

The following topology is more flexible and is the currently proposed template (it still supports 168 devices, which should be plenty):

* 00:00.0: i440FX Host bridge
* 00:01.0: PIIX3 (ISA/IDE/USB)
* 00:02.0: VGA or empty
* 00:03.0: Xen platform
*  00:03.1: Xen PV or empty
* 00:04.0: NVME controller or empty
* 00:05.0 - 00:0b.0: emulated NIC, max 7 (fn = devid+5)
* 00:0c.0+: NVidia vGPU, SRIOV NIC or other pass-through devices, assigned on a first-come first-served basis, using multifunction devices.

Unplugging a device won't renumber the rest of the devices, but instead leave a gap (where another function could be plugged later).
Except for function 0, which we cannot unplug if the rest of the functions are in use. We could either disallow unplugging it (awkard for the user), or move one of the existing devices there (may crash something unrelated in the guest), or use a "dummy" device (which again may be awkward).

Instead we try to avoid this limitation by by first assigning only function 0 of each PCI device, until we run out (reach 32 PCI devices), at which point we start filling function 1 of PCI devices 00:0c+
then function 2 of PCI devices 00:0c+ and so on.
When you unplug any function it leaves a gap that can be replaced by a newly added device, but won't cause existing devices to move, unless it is function 0.
In which case we renumber one of the existing function 1+ devices, and move it to function 0 to fill the gap.

However the virtual PCI address of pass-through devices <21 won't change (which is an improvement over the current situation), and as long as you only add new virtual PCI devices, then even  the address beyond pass-through device 21 will stay the same.
Only removing a device may lead to a renumbering of at most 1 other function.

Rationale:

* Don't touch device 0, 1 and 2 they are emulating real (legacy) hardware
* We cannot use 'Xen platform' or 'Xen PV' as function 0 in a multifunction device because both of these can be disabled (via `disable_pf` or `has-vendor-device`), and would cause unrelated PCI functions to change address. However when the 'Xen Platform' device is not present it doesn't make sense to have just the 'Xen PV' device, so they can share a PCI device.
* We can unplug emulated NIC 0, so that can only be used in a multifunction device if function 0 is something other than an emulated NIC and fixed, *or* all emulated NICs get their own PCI devices. The NVME controller could be used as fixed function 0, but what if we don't have one, e.g. for old BIOS VMs? Therefore we keep using separate PCI devices for emulated NICs
* Renumbering PCI devices should be avoided, especially that some OSes identify network devices by their PCI address



