      PROGRAM DAY14P1
C
      CHARACTER*50 STR
      COMPLEX ROBOTS(500,2),POS,VEL
      INTEGER NUMBOT,L1,X,Y,MOVES
      INTEGER Q1,Q2,Q3,Q4,TOTAL
      DATA MOVES,Q1,Q2,Q3,Q4,TOTAL /100,0,0,0,0,0/
C
   10 FORMAT(A)
   20 FORMAT(A,I16)
   30 FORMAT(A,4I4,A)
C
      WRITE(*,10)"Advent of Code 2024 day 14, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day14in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C
      NUMBOT=1
   50 CONTINUE
C     Read the input into a temporary string
      READ(10,FMT=10,ERR=100,END=100)STR
C     Convert input to complex numbers (X=real, Y=imaginary)
      CALL STR2COMPLEX(STR,POS,VEL)
      ROBOTS(NUMBOT,1)=POS
      ROBOTS(NUMBOT,2)=VEL
      NUMBOT=NUMBOT+1
      GOTO 50
C
  100 CONTINUE
      CLOSE(10)
C
      NUMBOT=NUMBOT-1
C     Make MOVES of each robot
      DO L1=1,NUMBOT
        ROBOTS(L1,1)=ROBOTS(L1,1)+MOVES*ROBOTS(L1,2)
        X=INT(REAL(ROBOTS(L1,1)))
        Y=INT(AIMAG(ROBOTS(L1,1)))
        IF (X.GE.0) THEN 
          X=MOD(X,101)
        ELSE
          X=101-MOD(-X,101)
          IF (X.EQ.101) X=0
        ENDIF
        IF (Y.GE.0) THEN 
          Y=MOD(Y,103)
        ELSE
          Y=103-MOD(-Y,103)
          IF (Y.EQ.103) Y=0
        ENDIF
        ROBOTS(L1,1)=COMPLEX(X,Y)
      ENDDO
C     Assign robots to quadrants
      DO L1=1,NUMBOT
        X=INT(REAL(ROBOTS(L1,1)))
        Y=INT(AIMAG(ROBOTS(L1,1)))
        IF ((X.LT.50).AND.(Y.LT.51)) Q1=Q1+1
        IF ((X.GT.50).AND.(Y.LT.51)) Q2=Q2+1
        IF ((X.LT.50).AND.(Y.GT.51)) Q3=Q3+1
        IF ((X.GT.50).AND.(Y.GT.51)) Q4=Q4+1
      ENDDO
C
      WRITE(*,30)"Quadrants have",Q1,Q2,Q3,Q4," robots"
      WRITE(*,10)" "
      TOTAL=Q1*Q2*Q3*Q4
      WRITE(*,20)"Result is ",TOTAL
      END
C
      SUBROUTINE STR2COMPLEX(STR,CNUMP,CNUMV)
C     Note: Subroutine destroys STR, returns CNUM
      CHARACTER*(*) STR
      COMPLEX CNUMP,CNUMV
C
      CHARACTER*4 STRPX,STRPY,STRVX,STRVY
      INTEGER X,Y
C     Deal with part of string up to value of p=X
      STR=STR(3:)
      STRPX=STR(:INDEX(STR,",")-1)
C     Deal with paty of string up to value of p=Y
      STR=STR(INDEX(STR,",")+1:)
      STRPY=STR(1:INDEX(STR," ")-1)
C     Deal with part of string up to value of v=X
      STR=STR(INDEX(STR,"=")+1:)
      STRVX=STR(:INDEX(STR,",")-1)
C     Deal with part of string up to value of v=Y
      STR=STR(INDEX(STR,",")+1:)
      STRVY=STR
C
   10 FORMAT (I4)
      READ(STRPX,10)X
      READ(STRPY,10)Y
      CNUMP=COMPLEX(X,Y)
      READ(STRVX,10)X
      READ(STRVY,10)Y
      CNUMV=COMPLEX(X,Y)
C
      RETURN
      END
