# v64hdd

Modified BIOS for the **Bung Doctor V64**, a Nintendo 64 backup and development device. This BIOS replaces CD-based ROM loading with support for **Compact Flash cards** and **IDE hard drives**, using FAT16 or FAT32 file systems.

Created by **OzOnE**, based on the official v1.30 BIOS. This repository exists to preserve and document his work, which was originally shared on forums and at risk of being lost.

> For CD64 / CD64 Plus owners, the equivalent HDD BIOS is available at https://github.com/rogerhinders/hdd64

---

## What is the Doctor V64?

The Bung Doctor V64 is a backup and development device for the Nintendo 64. It connects to the N64 cartridge slot and was originally designed to load ROMs from CDs. This custom BIOS replaces CD loading with IDE storage support, allowing ROMs to be loaded from a Compact Flash card or hard drive instead.

---

## BIOS Details

| | |
|---|---|
| **Base** | Official v1.30 BIOS |
| **Author** | OzOnE |
| **Processor** | 6502 |
| **Compatibility** | V64 only, not V64 Jr |
| **File systems** | FAT16 and FAT32 |
| **Installation** | EPROM replacement or flash programmer |
| **Status** | Experimental, not officially supported |

### Timeline

- **May 1997**: Original v1.30 BIOS disassembled and annotated by OzOnE
- **January 2008**: HDD/CF support added, ROMs can be loaded from IDE storage
- **November 2009**: FAT32 support added with cluster chain following, eliminating the need to defragment

---

## Storage Setup

### Supported Hardware

- IDE to Compact Flash adapters (recommended)
- Standard IDE hard drives, up to 2 GB
- MBR partition table

### Formatting a CF Card on Linux

```bash
# Replace /dev/sdX with your CF card device
sudo fdisk /dev/sdX

# Inside fdisk:
# d    delete existing partitions
# n    new partition, primary, default start and end sectors
# t    set type to 06 (FAT16) or 0B (FAT32)
# w    write and exit

# Format the partition
sudo mkfs.vfat -F 16 /dev/sdX1
```

### Tips for Best Performance

- Keep ROMs in subfolders rather than the root directory
- FAT16 limits the root directory to 512 entries total
- Use 8.3 filenames, for example `ZELDAUSA.Z64`
- The BIOS slows down when listing many files in a single directory

---

## Installation

> **Proceed at your own risk.** This is experimental software.

1. **Identify your BIOS chip.** Most V64 units use an AM29F010 or compatible 128 KB flash chip. If the chip is soldered and covered in epoxy, do not attempt modification.

2. **Back up your original BIOS** using an EPROM programmer before making any changes.

3. **Flash the modified BIOS** to a spare chip.

4. **Install the chip** in the V64's BIOS socket.

Keep a chip with the original BIOS as a fallback. The original v1.30 or v2.03 BIOS can restore normal CD-based operation if needed.

---

## Building from Source

OzOnE's original build environment was DOS-based, using `dasm` for assembly and `MAKEBIOS.EXE` to produce the final BIOS image. The included `COMPILE.bat` shows the full original workflow.

### Original Build Process (DOS)

```batch
dasm OZ.asm -v3 -f3 -oOZ.obj -sOZ.sym -lOZ.lst
dos32a makebios OZ.obj V64_VER1.30I
```

This assembles the source into a raw binary, then `MAKEBIOS.EXE` pads and formats it into the final 256 KB BIOS image.

### Building on Linux

You can assemble the source on Linux using the modern [dasm](https://github.com/dasm-assembler/dasm) package. Note that `MAKEBIOS.EXE` is a DOS executable, so the final BIOS packaging step requires DOSBox or a similar compatibility layer.

```bash
git clone git@github.com:gufranco/v64hdd.git
cd v64hdd

# Assemble the source
dasm OZ.asm -f3 -oOZ.obj -sOZ.sym -lOZ.lst
```

### Source Files

| File | Description |
|------|-------------|
| `OZ.asm` | Main source code, ~10,000 lines of annotated 6502 assembly |
| `MAKEBIOS.EXE` | DOS tool that produces the final BIOS image from the assembled binary |
| `COMPILE.bat` | Original build script |
| `dasm.exe` | DOS version of the DASM assembler |
| `DASM.txt` | DASM v2.0 documentation |

---

## Repository Contents

### Preserved Releases

The `releases - hdd` directory contains pre-built HDD BIOS binaries by OzOnE:

| File | Date |
|------|------|
| `Doctor V64 BIOS V1.30_28-1-08_OzOnE.bin` | January 28, 2008 |
| `Doctor V64 BIOS V1.30_02-2-08_OzOnE.bin` | February 2, 2008 |

The `releases - cdrom` directory contains official CD-ROM BIOS versions for reference:

- V2.02, V2.02b
- V2.03 in multiple color variants: standard, Black, Blue, Green, Purple, Red

### Original Archives

The `zips` directory contains the original distribution archives as they were shared online:

- `V64_HDD_-_Working___28-1-08_OzOnE.zip`: first working HDD build
- `V64 BIOS Asm + DASM (OzOnE 20-11-12).zip`: source code and tools bundle

---

## Known Issues

- Listing performance degrades with many files in a single directory
- Some ROMs may not load correctly
- Certain CF cards or IDE adapters may be incompatible
- The BIOS update via CD-R or parallel port was not fully tested by OzOnE. Using an EPROM programmer is the safest approach.

---

## Credits

- **OzOnE**: BIOS disassembly, annotation, and all HDD/CF modifications
- **Lac, Gaston, and others**: code, tools, and suggestions that inspired the project
- **Matthew Dillon**: DASM assembler

This repository only preserves OzOnE's work and is not affiliated with the original author.

---

## Disclaimer

This software is provided as-is with no warranty. Use it at your own risk. The repository maintainer accepts no responsibility for damage to hardware, data loss, or failed installations.
