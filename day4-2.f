      PROGRAM DAY4P2
C
      INTEGER XMAS(146,146)
      COMMON /A/ XMAS
C
      CHARACTER*140 STR
      INTEGER L1,L2,TOTAL,PARSE
C
   10 FORMAT(A)
   20 FORMAT(A,I3,A,I4)
   30 FORMAT(A,I4)
      WRITE(*,10)"Advent of Code 2024 day 4, part 2"
      WRITE(*,10)" "
C     Zero integer 2d array
      DO 45 L1=1,146
        DO 40 L2=1,146
          XMAS(L1,L2)=0
   40   CONTINUE
   45   CONTINUE
C
      OPEN(10,FILE="day4in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
      L1=0
   50 CONTINUE
      L1=L1+1
C     Read each input line
      READ(10,FMT=10,ERR=100,END=100)STR
C     Convert to ASCII codes (A=65 etc), store in XMAS 2d array
      DO 55 L2=1,140
        XMAS(L1+3,L2+3)=ICHAR(STR(L2:L2))
   55 CONTINUE
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C
C     Parse each cell from 4,4 to 143,143 for MAS as a cross
C     Only bother to check cells that are an A
C
      DO 110 L1=4,143
        DO 105 L2=4,143
          IF (XMAS(L1,L2).EQ.65) TOTAL=TOTAL+PARSE(L1,L2)
  105   CONTINUE
        WRITE(*,20)"Running total after line ",L1-3," is ",TOTAL
  110 CONTINUE
      WRITE(*,10)" "
      WRITE(*,30)"Result is ",TOTAL
      END
C
      INTEGER FUNCTION PARSE(L1,L2)
      INTEGER L1,L2
C
      COMMON /A/ XMAS
      INTEGER XMAS(146,146)
C
      PARSE=0
C
C     Check the 4 directions for the sum M+A+S
C     = 77+65+83 = 225
C 
      IF (((XMAS(L1,L2)+XMAS(L1-1,L2-1)+XMAS(L1+1,L2+1)).EQ.225) .AND.
     +    ((XMAS(L1,L2)+XMAS(L1+1,L2-1)+XMAS(L1-1,L2+1)).EQ.225)) THEN
        PARSE=PARSE+1
      ENDIF
C
      RETURN
      END
