      PROGRAM DAY17P1
C
      INTEGER A,B,C,PROG(0:15)
      INTEGER I,IPTR,EPTR,NEXTPTR,OUTPTR
      INTEGER OPCODE,OPERAND,RES
      CHARACTER*1000 OUTSTR
C
   10 FORMAT(A)
   20 FORMAT(A,I16)
   30 FORMAT(A,I4,I4)
   40 FORMAT(A,16I2)
   50 FORMAT(A,I6)
C
      WRITE(*,10)"Advent of Code 2024 day 17, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day17in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      CALL READREGISTERS(10,A,B,C)
C
      CALL READPROGRAM(10,PROG)
      CLOSE(10)
C
      WRITE(*,40)"Program is ",(PROG(I),I=0,15)
C     Set instruction pointer to first opcode
      IPTR=0
C     Set end of program marker to last operand
      EPTR=15
C     Clear OUTSTR string and set it's pointer to 1
      OUTSTR(1:)=" "
      OUTPTR=1
C
  100 CONTINUE
      OPCODE=PROG(IPTR)
C     Handle combo operands 1-3
      IF (PROG(IPTR+1).LE.3) OPERAND=PROG(IPTR+1)
C     Handle combo operands 4-7
      IF (PROG(IPTR+1).EQ.4) OPERAND=A
      IF (PROG(IPTR+1).EQ.5) OPERAND=B
      IF (PROG(IPTR+1).EQ.6) OPERAND=C
      IF ((PROG(IPTR+1).GE.7).OR.(PROG(IPTR+1).LT.0)) THEN
        WRITE(*,50)"Illegal OPERAND ",PROG(IPTR+1)
        STOP 7
      ENDIF
C     Next opcode is 2 instructions away, unless changed by
C     the current opcode
      NEXTPTR=IPTR+2
C     Process the current OPCODE
      IF (OPCODE.EQ.0) THEN
C-------ADV (Works on combo OPERAND)
        A=A/2**OPERAND
      ELSE IF (OPCODE.EQ.1) THEN
C-------BXL (Works on literal operand)
C       Use f2008 function IEOR
        B=IEOR(B,PROG(IPTR+1)) 
      ELSE IF (OPCODE.EQ.2) THEN
C-----BST (Works on combo OPERAND)
        B=MOD(OPERAND,8)
      ELSE IF (OPCODE.EQ.3) THEN
C-----JNZ (Jumps to literal operand if A not zero)
        IF (A.NE.0) THEN
          NEXTPTR=(PROG(IPTR+1))
        ENDIF
      ELSE IF (OPCODE.EQ.4) THEN
C-----BXC (Operand not used)
        B=IEOR(B,C)
      ELSE IF (OPCODE.EQ.5) THEN
C-----OUT
        RES=MOD(OPERAND,8)
        OUTSTR(OUTPTR:OUTPTR)=CHAR(RES+48)
        OUTSTR(OUTPTR+1:OUTPTR+1)=','
        WRITE(*,*)OUTSTR(1:OUTPTR)
        OUTPTR=OUTPTR+2
      ELSE IF (OPCODE.EQ.6) THEN
C-----BDV
        B=A/2**OPERAND
      ELSE IF (OPCODE.EQ.7) THEN
C-----CDV
        C=A/2**OPERAND
      ELSE
        WRITE(*,50)"Error - impossible opcode",OPCODE
        STOP 8
      ENDIF
C
C     Get pointer to next opcode or halt
C
      IF (NEXTPTR.LT.EPTR) THEN
        IPTR=NEXTPTR
        GOTO 100
      ELSE
        WRITE(*,10)"Program halt"
      ENDIF
      END
C
      SUBROUTINE READREGISTERS(FILE,REGA,REGB,REGC)
      INTEGER FILE,REGA,REGB,REGC
      CHARACTER*12 SUGAR
   10 FORMAT(A12,I8)
   20 FORMAT(A)
   30 FORMAT(A,I8)
C
C     Read registers A,B,C from file and discard next row
C
      READ(FILE,FMT=10)SUGAR,REGA
      READ(FILE,FMT=10)SUGAR,REGB
      READ(FILE,FMT=10)SUGAR,REGC
      READ(FILE,FMT=20)SUGAR
C
      WRITE(*,30)"Register A is ",REGA
      WRITE(*,30)"Register B is ",REGB
      WRITE(*,30)"Register C is ",REGC
C
      RETURN
      END
C
      SUBROUTINE READPROGRAM(FILE,P)
      INTEGER FILE,P(*)
      CHARACTER*8 SUGAR
      CHARACTER*50 PROG
      INTEGER I
   10 FORMAT(A8,A)
   20 FORMAT(A)
   30 FORMAT(BZ,16I2)
      READ(FILE,FMT=10)SUGAR,PROG
C     Strip commas from PROG
      I=INDEX(PROG,',')
      DO WHILE (I.NE.0)
        PROG(I:I)=" "
        I=INDEX(PROG,',')
      ENDDO
C     Convert string to array of integers
      READ(PROG,30)(P(I),I=1,16)
C
      RETURN
      END
