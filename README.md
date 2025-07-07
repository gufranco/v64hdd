# v64hdd

This repository preserves a modified BIOS for the **Bung V64** — a Nintendo 64 backup device. The BIOS adds experimental support for loading ROMs from **Compact Flash (CF)** cards or **IDE HDDs**, using the FAT16 file system. It was created by **OzOnE** and is based on the official **v1.30 BIOS**.

This repository exists to **preserve and document OzOnE’s work**, which was previously scattered across forums and risked being lost.

> ℹ️ For CD64 / CD64 Plus owners, the equivalent HDD BIOS is hosted here:  
> 👉 https://github.com/rogerhinders/hdd64

---

## 🎮 What is the V64?

The **Bung V64** is a backup and development device for the Nintendo 64. It connects to the N64’s expansion port and originally loaded games from CDs. This custom BIOS enables the use of a CF or HDD via an IDE interface — eliminating CD use entirely.

---

## 🔧 BIOS Version and Scope

- ✅ Based on: Official **v1.30 BIOS**
- ❌ Not based on: v2.03 / v2.03b (the last official versions)
- 🧠 Author: **OzOnE**
- 🖥️ Compatible with: **V64 only** (not V64 Jr)
- ⚠️ Not officially supported, experimental and may be unstable
- 🪛 Installation: via EPROM replacement or flash programmer

---

## 💾 Compact Flash Setup

This BIOS supports:

- **IDE to Compact Flash** adapters (preferred)
- **Standard IDE HDDs** (max 2GB)
- **FAT16 file system**
- **MBR partitioning**

### ✅ Recommended Setup (Linux)

You can format your CF card using the following steps:

```bash
# Wipe partition table and create MBR
sudo fdisk /dev/sdX

# Inside fdisk:
# d (delete existing partition)
# n (new partition, primary, default sectors)
# t (change type, set to 06 for FAT16)
# w (write and exit)

# Format as FAT16
sudo mkfs.vfat -F 16 /dev/sdX1
```

Replace `/dev/sdX1` with your CF partition.

---

## ⚠️ FAT16 Limitations

FAT16 imposes several important limits:

- **Max root entries**: 512 total (files + folders)
- **Max file size**: 2GB (not a concern for N64 ROMs)
- **No support for long filenames**: use 8.3 format (e.g. `ZELDAUSA.Z64`)
- **Performance issues**: the BIOS slows down when listing many files, especially in the root folder

> 📌 For best results:
> - Keep ROMs in subfolders
> - Avoid clutter in the root directory
> - Stay well below the 512 root entries limit

---

## 🛠️ Installation Instructions

> ⚠️ Proceed at your own risk. This software is experimental.

1. **Identify your V64 BIOS chip**:
   - Most units use an **AM29F010** or compatible 128KB Flash chip
   - If the chip is soldered and covered in epoxy, **do not attempt modification**

2. **Backup your original BIOS** using a programmer.

3. **Flash the modified BIOS** (e.g. `OZ.bin`) to a spare chip.

4. **Install the new chip** in a socket or as a replacement.

> 🔁 It is highly recommended to keep a **chip with the original BIOS** (v1.30 or 2.03) as a backup.

---

## 🧱 Building from Source (Linux)

OzOnE originally used a DOS environment and the `DASM` assembler to build the BIOS. You can build it on Linux using the modern `dasm` package.

### 🔧 Requirements

- [dasm](https://github.com/dasm-assembler/dasm) (Install via package manager or compile from source)

### 📂 Files

- `OZ.asm` — main source code
- `MAKEBIOS.EXE` — tool originally used to finalize the BIOS (not used on Linux)

### ✅ Build Instructions

```bash
# Clone and enter the repo
git clone https://github.com/youruser/v64hdd
cd v64hdd

# Build using dasm
dasm OZ.asm -f3 -oOZ.BIN
```

> This will generate `OZ.BIN` — ready to be flashed to your BIOS chip.

---

## 🚫 Known Issues

- Slow performance when listing many files
- Some ROMs may not load correctly
- FAT32 is not supported
- Unstable with certain CF cards or adapters
- BIOS is not optimized for speed or efficiency

---

## 📜 Credits

- Original BIOS mod created by **OzOnE**
- This repository only **preserves** the work and is **not affiliated** with the original author
- Thanks to **Matthew Dillon** for the original **DASM assembler**

---

## 🧷 Disclaimer

> This software is provided **as-is** with no warranty.  
> Use it at your own risk.  
> The author(s) and repository maintainer **accept no responsibility** for damage to your hardware, data loss, or failed installations.
