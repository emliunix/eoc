AS = riscv64-unknown-elf-as
LD = riscv64-unknown-elf-ld

ASFLAGS = -march=rv64gc
LDFLAGS = -T kernel.ld

OBJ_FILES = entry.o main.o

%.o: %.S
	@echo "AS $@"
	$(AS) $(ASFLAGS) -o $@ $<

run: kernel.elf
	qemu-system-riscv64 -nographic -machine virt -cpu rv64,v=on -kernel kernel.elf

kernel.elf: $(OBJ_FILES)
	@echo "LD $@"
	$(LD) $(LDFLAGS) -o $@ $(OBJ_FILES)

