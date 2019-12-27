all: boot kernel
.PHONY: boot kernel img qemu tirr
boot: target/typeboot.efi

TIRR = tirr/tirr/bin/Release/netcoreapp3.0/tirr

tirr: $(TIRR)

$(TIRR):
	cd tirr; dotnet build -p:Configuration=Release

target/trap.o: arch/x86_64/trap.S
	gcc arch/x86_64/trap.S -c -O2 -fno-stack-protector -fpic -fshort-wchar -mno-red-zone -o target/trap.o

target/typeboot.efi: target/bootloader.c
	gcc target/bootloader.c             \
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
	ld target/typeboot.o    \
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
target/bootloader.c: target/bootloader.tirr
	$(TIRR) <target/bootloader.tirr > target/bootloader.c
target/bootloader.tirr:
	stack run bootloader target/bootloader.tirr
target/kernel.c: target/kernel.tirr
	$(TIRR) <target/kernel.tirr > target/kernel.c
target/kernel.tirr:
	stack run kernel target/kernel.tirr

target/kernel.elf: target/kernel.c arch/x86_64/trap.S
	gcc -T arch/x86_64/linker.ld -o target/kernel.elf -ffreestanding -O3 -nostdlib target/kernel.c arch/x86_64/trap.S -lgcc
kernel: target/kernel.elf
img: target/typekernel.img
target/typekernel.img: kernel #boot
	dd if=/dev/zero of=target/typekernel.img bs=512 count=93750
	parted target/typekernel.img -s -a minimal mklabel gpt
	parted target/typekernel.img -s -a minimal mkpart EFI FAT16 2048s 93716s
	parted target/typekernel.img -s -a minimal toggle 1 boot
	dd if=/dev/zero of=target/part.img bs=512 count=91669
	mformat -i target/part.img -h 32 -t 32 -n 64 -c 1
	#mcopy -i target/part.img target/kernel.efi ::
	#mcopy -i target/part.img target/typeboot.efi ::
	mkdir -p target/efi/boot/
	cp rboot.efi target/efi/boot/bootx64.efi
	cp rboot.conf target/efi/boot/
	mcopy -i target/part.img -s target/efi ::
	mcopy -i target/part.img -s target/kernel.elf ::/efi/boot/
	dd if=target/part.img of=target/typekernel.img bs=512 count=91669 seek=2048 conv=notrunc
OVMF := runtime/x86_64/OVMF.fd
qemu: img
	qemu-system-x86_64 \
		-drive if=pflash,format=raw,file=${OVMF},readonly=on \
		-drive format=raw,file=target/typekernel.img,if=ide \
		-net none -m 1G \
            -serial mon:stdio 
debug:
	qemu-system-x86_64 \
		-drive if=pflash,format=raw,file=${OVMF},readonly=on \
		-drive format=raw,file=target/typekernel.img,if=ide \
		-net none -m 1G \
            -serial mon:stdio -S -gdb tcp::11234

