      COMMON /ADVENT/ MAP,DISTINCT
      INTEGER MAP(60,60),DISTINCT
C
      INTEGER X,Y,D
      INTEGER TOTAL,DISTINCTT,FINDTRAILS
C
   10 FORMAT(A)
   20 FORMAT(58I1)
   30 FORMAT(A,I5)
   40 FORMAT(A,I2,A,I2,A)
C
      WRITE(*,10)"Advent of Code 2024 day 10, both parts"
      WRITE(*,10)" "
      OPEN(10,FILE="day10in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
      DISTINCTT=0
C     Set all map sqaures to 99 so we have a boundary that can't
C     be stepped on - means we don't have to think about edge cases
      DO X=1,60
        DO Y=1,60
          MAP(X,Y)=99
        ENDDO
      ENDDO
C
      Y=2
   50 CONTINUE
C     Read the input lines into a 2D integer array
      READ(10,FMT=20,ERR=100,END=100) (MAP(X,Y),X=2,59)
      Y=Y+1
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C     Check each 0 point for good trails
      DO X=2,59
        DO Y=2,59
          IF(MAP(X,Y).EQ.0) THEN 
            WRITE(*,40)"MAP ",X,",",Y," is 0"
            D=MAP(X,Y)
            DISTINCT=0
            TOTAL=FINDTRAILS(X,Y,D)
            DISTINCTT=DISTINCTT+DISTINCT
            WRITE(*,10)" "
            WRITE(*,30)"Running total of trailhead scores is ",DISTINCTT
            WRITE(*,30)"Running total of trailhead ratings is",TOTAL
            WRITE(*,10)"------------------------------------"
            WRITE(*,10)" "
          ENDIF
        ENDDO
      ENDDO
      WRITE(*,30)"Sum of trailhead scores  (part 1) is",DISTINCTT
      WRITE(*,30)"Sum of trailhead ratings (part 2) is",TOTAL
      END
C
C     Fortran 90 extension - recursive function
C
      RECURSIVE INTEGER FUNCTION FINDTRAILS(X,Y,D) RESULT(RES)
      INTEGER X,Y,D
C
      COMMON /ADVENT/ MAP,DISTINCT
      INTEGER MAP(60,60),DISTINCT
C
      COMPLEX DHEAD(100)
      INTEGER L1
C
   10 FORMAT(A,I2,A,I2)
   20 FORMAT(A)
C
      IF (D.EQ.9) THEN
        WRITE(*,10)"Trail end found at ",X,",",Y
        DO L1=1,DISTINCT
          IF (DHEAD(L1).EQ.COMPLEX(X,Y)) GOTO 100
        ENDDO
        WRITE(*,20)"This is a distinct trail end"
        DISTINCT=DISTINCT+1
        DHEAD(DISTINCT)=COMPLEX(X,Y)
100     CONTINUE
        RES=RES+1
        GOTO 999
      ENDIF
C
      IF (MAP(X+1,Y).EQ.(D+1)) THEN
        RES=FINDTRAILS(X+1,Y,D+1)
      ENDIF
      IF (MAP(X,Y+1).EQ.(D+1)) THEN
        RES=FINDTRAILS(X,Y+1,D+1)
      ENDIF
      IF (MAP(X-1,Y).EQ.(D+1)) THEN
        RES=FINDTRAILS(X-1,Y,D+1)
      ENDIF
      IF (MAP(X,Y-1).EQ.(D+1)) THEN
        RES=FINDTRAILS(X,Y-1,D+1)
      ENDIF
C             
  999 CONTINUE
      RETURN
      END
