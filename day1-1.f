      PROGRAM DAY1P1
C
C     Read the input file (2 integers, blank separated)
C     Sort each array in ascending order
C     Calculate the absolute differences between each ordered element
C
      INTEGER LIST1(1000),LIST2(1000)
      INTEGER IDX,TOTAL
   10 FORMAT(A)
   20 FORMAT(I8,BN,I8)
   30 FORMAT(A,I8)
      WRITE(*,10)"Advent of Code 2024 day 1, part 1"
      OPEN(10,FILE="day1in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      IDX=1
   50 CONTINUE
      READ(10,20,ERR=100,END=100)LIST1(IDX),LIST2(IDX)
      IDX=IDX+1
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      CALL INSORT(LIST1,1000)
      CALL INSORT(LIST2,1000)
      DO 200 IDX=1,1000
        TOTAL=TOTAL+IABS(LIST1(IDX)-LIST2(IDX))
  200 CONTINUE
      WRITE(*,30)"Result is ",TOTAL
      END
C
      SUBROUTINE INSORT(LIST,NUM)
C     Slow but useful insertion sort
C     LIST is returned with the first NUM elements sorted
C     in ascending order
      INTEGER LIST(*),NUM
      INTEGER J,K,TEMP
      DO 20 J=2,NUM
        TEMP=LIST(J)
        K=J-1
        DO 10 WHILE ((LIST(K).GT.TEMP).AND.(K.GE.1))
          LIST(K+1)=LIST(K)
          K=K-1
   10   CONTINUE
        LIST(K+1)=TEMP
   20 CONTINUE
      RETURN
      END
