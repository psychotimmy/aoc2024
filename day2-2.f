      PROGRAM DAY2P2
C
      CHARACTER*30 STR
      INTEGER LIST(8),IDX,TOTAL,NUMVALS,ISSAFE,ISDAMPED
C
   10 FORMAT(A)
C
C     Input file not well formatted for f77 fixed width integers
C     So read in as a character string and separate it first
C
   30 FORMAT(A,I8)
      WRITE(*,10)"Advent of Code 2024 day 2, part 2"
      OPEN(10,FILE="day2in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
   50 CONTINUE
C     Read each input line, convert to an array of integers
      READ(10,10,ERR=100,END=100)STR
      CALL STR2INTARRAY(STR,LIST,8,NUMVALS)
C     Work out if the values are safe
      IF (ISSAFE(LIST,NUMVALS).EQ.1) THEN
C     Will also be safe if dampened, so don't bother
        TOTAL=TOTAL+1
      ELSE
C     Only safe if ISDAMPED returns 1
        TOTAL=TOTAL+ISDAMPED(LIST,NUMVALS)
      ENDIF
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      WRITE(*,30)"Result is ",TOTAL
      END
C
      SUBROUTINE STR2INTARRAY(STR,LIST,MAXL,NONZERO)
      CHARACTER*(*) STR
      INTEGER LIST(*),MAXL,NONZERO
C
C     Separates a list of MAXL integers stored in STR into the
C     array LIST. The number of consecutive non-zero elements at 
C     the start of the list is returned in NONZERO
C
      INTEGER IDX,SDX
      DO 10 IDX=1,MAXL
        LIST(IDX)=0
   10 CONTINUE
      NONZERO=0
      IDX=1
      SDX=1
   20 CONTINUE
      IF ((IDX.GT.MAXL).OR.(SDX.GT.LEN(STR))) GOTO 999
      IF (STR(SDX:SDX).EQ.' ') THEN 
        IF (LIST(IDX).NE.0) THEN 
          NONZERO=NONZERO+1
        ELSE
          GOTO 999
        ENDIF
        IDX=IDX+1
      ELSE
        LIST(IDX)=LIST(IDX)*10+ICHAR(STR(SDX:SDX))-48
      ENDIF
      SDX=SDX+1
      GOTO 20
  999 CONTINUE
      RETURN
      END
C
      INTEGER FUNCTION ISSAFE(LIST,NUM)
      INTEGER LIST(*),NUM
C
C     List is safe if all numbers are ascending or descending
C     and that the absolute difference between is number is between
C     1 and 3.
C
      INTEGER IDX
      LOGICAL ASCEND,DESCEND,VALIDGAP
C
      ISSAFE=1
      ASCEND=.TRUE.
      DESCEND=.TRUE.
      VALIDGAP=.TRUE.
C
      DO 100 IDX=2,NUM
        IF (LIST(IDX).GT.LIST(IDX-1)) DESCEND=.FALSE.
        IF (LIST(IDX).LT.LIST(IDX-1)) ASCEND=.FALSE.
        IF ((IABS(LIST(IDX)-LIST(IDX-1)).LT.1).OR.
     +      (IABS(LIST(IDX)-LIST(IDX-1)).GT.3)) VALIDGAP=.FALSE.
  100 CONTINUE
      IF (.NOT. VALIDGAP) ISSAFE=0
C     Complier flags .XOR. as a warning - but it is legal f77!
      IF (.NOT. (ASCEND.XOR.DESCEND)) ISSAFE=0
      RETURN
      END
C
      INTEGER FUNCTION ISDAMPED(LIST,NUM)
      INTEGER LIST(*),NUM
C
C     Go through the list and remove each number until we find we have
C     a safe report, or until all are checked and all are marked unsafe
C
      INTEGER IDX,JDX,DAMPLIST(NUM-1)
      ISDAMPED=0
C
C     Split the original list into two halves and ignore one value
C
      DO 200 IDX=1,NUM
        DO 100 JDX=1,IDX-1
          DAMPLIST(JDX)=LIST(JDX)
  100   CONTINUE
        DO 150 JDX=IDX+1,NUM
          DAMPLIST(JDX-1)=LIST(JDX)
  150   CONTINUE
C       Is the damped list safe?
        ISDAMPED=ISSAFE(DAMPLIST,NUM-1)
C       If so, exit
        IF (ISDAMPED.EQ.1) GOTO 999
  200 CONTINUE 
  999 CONTINUE
      RETURN
      END
        
        

