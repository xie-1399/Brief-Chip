# here is some memory operation to test the lsu unit

.text			# Define beginning of text section
.global	_start	# Define entry _start

_start:
    li x6, 0		# x6 = 0
    li x5, 0        # x5 = 0
    
    # write sw to the dbus
    lui x1, %hi(0x80001000)
    li  x2, %lo(0x80001000)
    add x1,x1,x2
    lui x2,%hi(0x10101010)
    addi x2,x2,16
    sw x2,0(x1)   # write 0x80001000 with 0x10101010

    lui x1, %hi(0x80001004)
    li  x2, %lo(0x80001004)
    add x1,x1,x2
    lui x2,%hi(0x20202020)
    addi x2,x2,32
    sw x2,0(x1)   # write 0x80001004 with 0x20202020

    # write sh to the dbus
    lui x1, %hi(0x80001008)
    li  x2, %lo(0x80001008)
    add x1,x1,x2
    lui x2,%hi(0x20202020)
    addi x2,x2,32
    sh x2,0(x1)   # write 0x80001008 with 0x2020

    lui x1, %hi(0x8000100C)
    li  x2, %lo(0x8000100C)
    add x1,x1,x2
    lui x2,%hi(0x30303030)
    addi x2,x2,48
    sh x2,0(x1)   # write 0x8000100C with 0x3030

    # write sb to the dbus
    lui x1, %hi(0x80001010)
    li  x2, %lo(0x80001010)
    add x1,x1,x2
    lui x2,%hi(0x40404040)
    addi x2,x2,4
    sb x2,0(x1)   # write 0x80001010 with 0x04

    lui x1, %hi(0x80001020)
    li  x2, %lo(0x80001020)
    add x1,x1,x2
    lui x2,%hi(0x50505050)
    addi x2,x2,5
    sb x2,0(x1)   # write 0x80001020 with 0x05

    # lw/lh/lb It to check
    li x4,0       # x4 == 0
    lui x1, %hi(0x80001000)
    li  x2, %lo(0x80001000)
    add x1,x1,x2
    lw x4,0(x1)   # x4 == 0x10101010

    lui x1, %hi(0x80001004)
    li  x2, %lo(0x80001004)
    add x1,x1,x2
    lw x4,0(x1)   # x4 == 0x20202020

    lui x1, %hi(0x80001008)
    li  x2, %lo(0x80001008)
    add x1,x1,x2
    lhu x4,0(x1)   # x4 == 0x2020

    lui x1, %hi(0x8000100C)
    li  x2, %lo(0x8000100C)
    add x1,x1,x2
    lhu x4,0(x1)   # x4 == 0x3030

    lui x1, %hi(0x80001010)
    li  x2, %lo(0x80001010)
    add x1,x1,x2
    lbu x4,0(x1)   # x4 == 0x04

    lui x1, %hi(0x80001020)
    li  x2, %lo(0x80001020)
    add x1,x1,x2
    lui x2,%hi(0x50505050)
    lbu x4,0(x1)   # x2 == 0x05

    nop
    nop
    nop
    nop
    nop

    lui x1, %hi(0x10000020)
    li  x2, %lo(0x10000020)
    add x1,x1,x2
    li x5,0x10
    sw x5,0(x1)   # write the 0x10000020 with 0x10

    lui x1, %hi(0x10000020)
    li  x2, %lo(0x10000020)
    add x1,x1,x2
    li x5,0x20
    sw x5,0(x1)   # write the 0x10000020 with 0x10