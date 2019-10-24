all: boot kernel
.PHONY: boot kernel img qemu
boot: target/typeboot.efi

target/typeboot.efi: target/bootloader.c
	gcc target/bootloader.c              \
      -c                                 \
      -O2 \
      -fno-stack-protector               \
      -fpic                              \
      -fshort-wchar                      \
      -mno-red-zone                      \
      -I /usr/include/efi        \
      -I /usr/include/efi/x86_64 \
      -DEFI_FUNCTION_WRAPPER             \
      -o target/typeboot.o
	ld target/typeboot.o                         \
     /usr/lib/crt0-efi-x86_64.o     \
     -nostdlib                      \
     -znocombreloc                  \
     -T /usr/lib/elf_x86_64_efi.lds \
     -shared                        \
     -Bsymbolic                     \
     -L /usr/lib                    \
     -l:libgnuefi.a                 \
     -l:libefi.a                    \
     -o target/typeboot.so
	objcopy -j .text                \
          -j .sdata               \
          -j .data                \
          -j .dynamic             \
          -j .dynsym              \
          -j .rel                 \
          -j .rela                \
          -j .reloc               \
          --target=efi-app-x86_64 \
          target/typeboot.so                 \
          target/typeboot.efi
target/bootloader.c:
	stack run bootloader target/bootloader.c
kernel:
	echo "Kernel not implemented yet."
img: target/typekernel.img
target/typekernel.img: boot
	dd if=/dev/zero of=target/typekernel.img bs=512 count=93750
	parted target/typekernel.img -s -a minimal mklabel gpt
	parted target/typekernel.img -s -a minimal mkpart EFI FAT16 2048s 93716s
	parted target/typekernel.img -s -a minimal toggle 1 boot
	dd if=/dev/zero of=target/part.img bs=512 count=91669
	mformat -i target/part.img -h 32 -t 32 -n 64 -c 1
	mcopy -i target/part.img target/typeboot.efi ::
	dd if=target/part.img of=target/typekernel.img bs=512 count=91669 seek=2048 conv=notrunc
OVMF := runtime/x86_64/OVMF.fd
qemu:
	qemu-system-x86_64 \
		-drive if=pflash,format=raw,file=${OVMF},readonly=on \
		-drive format=raw,file=target/typekernel.img,if=ide \
		-net none
