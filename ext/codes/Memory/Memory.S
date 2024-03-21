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
    sw x2,0(x1)   # write 0x80001000 with 0x10101010

    lui x1, %hi(0x80001004)
    li  x2, %lo(0x80001004)
    add x1,x1,x2
    lui x2,%hi(0x20202020)
    sw x2,0(x1)   # write 0x80001004 with 0x20202020

    # write sh to the dbus
    lui x1, %hi(0x80001008)
    li  x2, %lo(0x80001008)
    add x1,x1,x2
    lui x2,%hi(0x20202020)
    sh x2,0(x1)   # write 0x80001008 with 0x2020

    lui x1, %hi(0x8000100C)
    li  x2, %lo(0x8000100C)
    add x1,x1,x2
    lui x2,%hi(0x30303030)
    sh x2,0(x1)   # write 0x8000100C with 0x3030

    # write sb to the dbus
    lui x1, %hi(0x8000100F)
    li  x2, %lo(0x8000100F)
    add x1,x1,x2
    lui x2,%hi(0x40404040)
    sb x2,0(x1)   # write 0x8000100F with 0x40

    lui x1, %hi(0x8000101F)
    li  x2, %lo(0x8000101F)
    add x1,x1,x2
    lui x2,%hi(0x50505050)
    sb x2,0(x1)   # write 0x8000101F with 0x50

    # lw/lh/lb It to check
    lui x1, %hi(0x80001000)
    li  x2, %lo(0x80001000)
    add x1,x1,x2
    lw x2,0(x1)   # x2 == 0x10101010

    lui x1, %hi(0x80001004)
    li  x2, %lo(0x80001004)
    add x1,x1,x2
    lw x2,0(x1)   # x2 == 0x20202020

    lui x1, %hi(0x80001008)
    li  x2, %lo(0x80001008)
    add x1,x1,x2
    lh x2,0(x1)   # x2 == 0x2020

    lui x1, %hi(0x8000100C)
    li  x2, %lo(0x8000100C)
    add x1,x1,x2
    lh x2,0(x1)   # x2 == 0x3030

    lui x1, %hi(0x8000100F)
    li  x2, %lo(0x8000100F)
    add x1,x1,x2
    lb x2,0(x1)   # x2 == 0x40

    lui x1, %hi(0x8000101F)
    li  x2, %lo(0x8000101F)
    add x1,x1,x2
    lui x2,%hi(0x50505050)
    lb x2,0(x1)   # x2 == 0x50