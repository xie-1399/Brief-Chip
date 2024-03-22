# here is some arith operation to test the mul and alu unit

.text			# Define beginning of text section
.global	_start	# Define entry _start

_start:
    li x6, 2		# x6 = 2 010
    li x5, 6        # x5 = 6 110

    # mul and div
    li x1, 0
    mul x1,x5,x6   # x1 = 12
    mul x1,x1,x5   # x1 = 72

    li x1,0
    div x1,x5,x6   # x1 = 3
    rem x1,x1,x6   # x1 = 1

    # add more arithmetic operation here
    li x1,0
    add x1,x5,x6   # x1 = 8
    addi x1,x5,10  # x1 = 16

    li x1,0
    sub x1,x5,x6   # x1 = 4
    addi x1,x5,-1  # x1 = 5

    li x1,0
    or x1,x5,x6    # x1 = 6
    and x1,x5,x6   # x1 = 2
    sll x1,x5,x6   # x1 = 24
    slli x1,x5,3   # x1 = 48
    sra x1,x5,x6   # x1 = 1
    srai x1,x5,1   # x1 = 3
    slt x1,x5,x6   # x1 = 0
    slti x1,x5,10  # x1 = 1

    # U type also work
    lui x1,0x1     # x1 = 4096
    lui x1,0x2     # x1 = 8192
    auipc x1,0x1   # x1 = pc + 4096
    auipc x1,0x2   # x1 = pc + 8192