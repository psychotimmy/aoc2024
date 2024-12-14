      PROGRAM DAY14P2
C
      CHARACTER*50 STR
      CHARACTER*101 ROW
      COMPLEX ROBOTS(500,2),POS,VEL
      INTEGER GRID(0:100,0:102)
      INTEGER NUMBOT,L1,L2,X,Y,MOVES
      INTEGER TOTAL
      DATA MOVES,TOTAL /1,0/
C
   10 FORMAT(A)
   30 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2024 day 14, part 2"
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
C     We want to save the grids so we can inspect them
C     visually afterwards
      OPEN(10,FILE="day14out.txt",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="WRITE")
C
      NUMBOT=NUMBOT-1
C
C     Top of grid generation loop
  200 CONTINUE
      DO L2=0,102
        DO L1=0,100
          GRID(L1,L2)=0
        ENDDO
      ENDDO
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
        GRID(X,Y)=GRID(X,Y)+1
      ENDDO
C
      TOTAL=TOTAL+MOVES
C
C     These bounds are from 2 earlier guesses that were wrong
C     when I'd assumed the tree would be in the middle and so
C     symmetrical around Y=51! Simplest way to solve is to 
C     write all of the grids out to a file and then look for
C     the tree using vi. Happy Christmas!!!
C
      IF ((TOTAL.GT.4800).AND.(TOTAL.LT.10000)) THEN
C       Write current number of moves to file
        WRITE(10,30)"After move",TOTAL
        DO L2=0,102
          DO L1=0,100
            ROW(L1+1:L1+1)="."
            IF (GRID(L1,L2).GT.0) THEN
              ROW(L1+1:L1+1)="X"
            ENDIF
          ENDDO
C         Write current row to file
          WRITE(10,10)ROW
        ENDDO
C       Write a blank line to file
        WRITE(10,10)" "
        GOTO 200
      ENDIF
      IF (TOTAL.LE.4800) GOTO 200
C    
      CLOSE(10)
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
