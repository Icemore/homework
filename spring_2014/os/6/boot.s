.code16

.text
.globl _start

_start:
    cli
    movw %cs, %ax
    movw %ax, %ds
    addw $0x0220, %ax
    movw %ax, %ss
    movw $0x0100, %sp
    sti
   
    lea string, %si
printString:
    lodsb
    test %al, %al
    jz exit
    movb $0x0e, %ah
    int $0x10
    jmp printString

exit:

    hlt

    string: .asciz "Hello world!"

    . = _start + 0x0200 - 2
    .short 0xAA55
