ENTRY(_start)

MEMORY {
  kernel_space (rx) : ORIGIN = 0x80200000, LENGTH = 0x8000000 # 128MB
  stack (rw) : ORIGIN = 0x801FF000, LENGTH = 0x1000 # 4KB for stack
}

SECTIONS {
  .text : {
    entry.o(.text)
    *(.text .text.*)
  } > kernel_space

  .data : {
    *(.data .data.*)
  } > kernel_space

  .stack : {
    *(.stack .stack.*)
  } > stack

  __stack_top = ((ORIGIN(stack) + LENGTH(stack)) & ~15);
}
