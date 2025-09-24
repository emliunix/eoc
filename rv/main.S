# Assembly file containing the main function

.global main
.section .text
main:
    # args: s.len s.data

    # prologue
    addi sp,sp,-16
    sd ra,8(sp)
    sd fp,0(sp)

    li a7, 0x4442434E
    c.li a6, 0x00
    c.li a2, 0
    ecall

    # epilogue
    ld fp,0(sp)
    ld ra,8(sp)
    addi sp,sp,16
    ret


