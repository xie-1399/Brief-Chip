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
