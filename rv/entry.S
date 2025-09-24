.global _start
.section .text
_start:
	# Setup stack using label from kernel.ld
	la sp, __stack_top

	# Call the main function
	c.li a0, 12
	lla a1, hello_string
	call main

exit:
	li a7, 0x53525354
	c.li a6, 0x00
	c.li a0, 0
	c.li a1, 0
	ecall
.section .data
hello_string:
	.string "Hello world\n"
