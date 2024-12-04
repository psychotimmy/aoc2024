      PROGRAM DAY4P1
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
      WRITE(*,10)"Advent of Code 2024 day 4, part 1"
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
C     Parse each cell from 4,4 to 143,143 for XMAS in 8 directions
C
      DO 110 L1=4,143
        DO 105 L2=4,143
          TOTAL=TOTAL+PARSE(L1,L2)
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
      INTEGER X,Y
      PARSE=0
C     Check the 8 directions
      DO 200 X=-1,1
        DO 100 Y=-1,1
          IF ((XMAS(L1,L2).EQ.88).AND.
     +        (XMAS(L1+(X*1),L2+(Y*1)).EQ.77) .AND.
     +        (XMAS(L1+(X*2),L2+(Y*2)).EQ.65) .AND.
     +        (XMAS(L1+(X*3),L2+(Y*3)).EQ.83)) THEN
            PARSE=PARSE+1
          ENDIF
  100   CONTINUE
  200   CONTINUE
C
      RETURN
      END
