      PROGRAM DAY8P1
      COMMON /ADVENT/ ANTIGRID
      INTEGER ANTIGRID(50,50)
      CHARACTER*50 STR
      INTEGER GRIDKEY(75),X,Y,L1,ANT,COUNTANTIGRID
      COMPLEX GRID(75,4)
C
   10 FORMAT(A)
   20 FORMAT(A,I4)
C
      WRITE(*,10)"Advent of Code 2024 day 8, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day8in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Zero GRIDKEY array
      DO 30 X=1,75
        GRIDKEY(X)=0
   30 CONTINUE
C     Zero ANTIGRID 2D array
      DO 45,X=1,50
        DO 40 Y=1,50
          ANTIGRID(X,Y)=0
   40   CONTINUE
   45 CONTINUE
C
      Y=0
   50 CONTINUE
C     Read each input line
      READ(10,FMT=10,ERR=100,END=100) STR
      Y=Y+1
C     Store each antenna as a complex value in GRID, with each
C     distinct type of antenna indexed by GRIDKEY
      DO 60 X=1,50
        IF (STR(X:X).NE.'.') THEN
          ANT=ICHAR(STR(X:X))-47
          GRIDKEY(ANT)=GRIDKEY(ANT)+1
          GRID(ANT,GRIDKEY(ANT))=COMPLEX(X,Y)
        ENDIF
   60 CONTINUE
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C     Work through each of the distinct antenna types
      DO 400 X=1,75
        DO 300 Y=1,GRIDKEY(X)-1
          DO 200 L1=Y+1,GRIDKEY(X)
            CALL LINEUP(GRID(X,Y),GRID(X,L1))
  200     CONTINUE
  300   CONTINUE
  400 CONTINUE
      WRITE(*,20)"Result is ",COUNTANTIGRID(50,50)
      END
C
      SUBROUTINE LINEUP(P1,P2)
      COMPLEX P1,P2
      COMMON /ADVENT/ ANTIGRID
      INTEGER ANTIGRID(50,50)
      COMPLEX PDIFF
      INTEGER X1,X2,Y1,Y2
C
C     Need the difference between the points
      PDIFF=P1-P2
C     Work out where the antinode will be
C     Convert complex values to integers
      X1=INT(REAL(P1+PDIFF))
      Y1=INT(AIMAG(P1+PDIFF))
      X2=INT(REAL(P2-PDIFF))
      Y2=INT(AIMAG(P2-PDIFF))
C     Place antinodes on antigrid if we can
      IF ((X1.GE.1).AND.(X1.LE.50).AND.
     +    (Y1.GE.1).AND.(Y1.LE.50)) THEN
        ANTIGRID(X1,Y1)=ANTIGRID(X1,Y1)+1
      ENDIF
      IF ((X2.GE.1).AND.(X2.LE.50).AND.
     +    (Y2.GE.1).AND.(Y2.LE.50)) THEN
        ANTIGRID(X2,Y2)=ANTIGRID(X2,Y2)+1
      ENDIF
      RETURN
      END
C
      INTEGER FUNCTION COUNTANTIGRID(X,Y)
      INTEGER X,Y
      COMMON /ADVENT/ ANTIGRID
      INTEGER ANTIGRID(50,50)
      INTEGER I,J
C
      COUNTANTIGRID=0
      DO 200 I=1,X
        DO 100 J=1,Y
          IF (ANTIGRID(I,J).GT.0) COUNTANTIGRID=COUNTANTIGRID+1
  100   CONTINUE
  200 CONTINUE
      RETURN
      END
