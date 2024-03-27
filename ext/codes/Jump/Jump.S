# here is some jump and branch test all

.text			# Define beginning of text section
.global	_start	# Define entry _start

_start:
    li x6, 2
    li x5, 6
    beq x5,x6,_fail
    bne x5,x6,_main
    j _fail
    nop
    nop
    nop

_main:
    li x1,3
    li x2,4
    j _blt
    j _fail

_blt:
    blt x1,x2,_bge
    li x1,5
    li x2,6
    li x4,10    # not excute
    j _fail

_bge:
    li x1,5
    li x2,4
    bge x1,x2,_bltu
    li x1,5
    li x2,6
    li x4,10    # not excute
    j _fail

_bltu:
    bltu x1,x2,_bgeu
    li x1,5
    li x2,6
    li x4,10    # x4 = 10
    j _bgeu
    j _fail

_bgeu:
    bgeu x1,x2,_bgeu
    li x1,5
    li x2,6
    li x4,20    # x4 = 20
    j _pass
    j _fail

_pass:
    li x4 , 1   # x4 = 1
    j _pass

_fail:
    li x4, 0    # x4 = 0
    j _fail

