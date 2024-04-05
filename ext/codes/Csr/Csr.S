# here is some csr write and read values

.text			# Define beginning of text section
.global	_start	# Define entry _start

_start:
    li x6, 2		# x6 = 2 010
    li x5, 6        # x5 = 6 110

    # csr types
    csrrw x4,mstatus,x5    # x4 == 0
    csrrw x4,mstatus,x6    # x4 == 6
    csrrwi x4,mstatus,8    # x4 == 2

    li x1,2
    csrrs x4,mstatus,x1    # x4 == 8
    csrrsi x4,mstatus,4    # x4 == 10

    li x1,4
    csrrc x4,mstatus,x1    # x4 == 14
    csrrci x4,mstatus,3    # x4 == 10
    nop
    nop
    nop
    li x5,1000
    li x6,0
    lui x1, %hi(0x10020000)
    li  x2, %lo(0x10020000)
    add x1,x1,x2
    j _ram

_ram:
    /* write into the 0x10020000 ram */
    add x1,x1,x6
    sw x6,0(x1)
    lw x5,0(x1)
    bne x5,x6,_fail
    addi x6,x6,4
    beq x6,x5,_pass
    bne x6,x5,_ram

_fail:
    j _fail

_pass:
    j _pass