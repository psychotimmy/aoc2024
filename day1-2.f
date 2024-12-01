      PROGRAM DAY1P2
C
C     Read the input file (2 integers, blank separated)
C     For each number in list 1 (not unique), work out how often it
C     appears in list2. Multiply the number in list 1 by times in list 2
C     Calculate the sum.
C
      INTEGER LIST1(1000),LIST2(1000)
      INTEGER IDX,TOTAL,SIMSCORE
   10 FORMAT(A)
   20 FORMAT(I8,BN,I8)
   30 FORMAT(A,I8)
      WRITE(*,10)"Advent of Code 2024 day 1, part 2"
      OPEN(10,FILE="day1in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      IDX=1
   50 CONTINUE
      READ(10,20,ERR=100,END=100)LIST1(IDX),LIST2(IDX)
      IDX=IDX+1
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      TOTAL=SIMSCORE(LIST1,LIST2,1000)
      WRITE(*,30)"Result is ",TOTAL
      END
C
      INTEGER FUNCTION SIMSCORE(LIST1,LIST2,NUM)
      INTEGER LIST1(*),LIST2(*),NUM
C     Calculate similarity score for each number in list 1 vs list 2
      INTEGER IDX,JDX,TEST,TIMES
      SIMSCORE=0
      DO 20 IDX=1,NUM
        TEST=LIST1(IDX) 
        TIMES=0
        DO 10 JDX=1,NUM
          IF (LIST2(JDX).EQ.TEST) TIMES=TIMES+1
   10   CONTINUE
        SIMSCORE=SIMSCORE+TEST*TIMES
   20 CONTINUE
      RETURN
      END
