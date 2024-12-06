      PROGRAM DAY6P1
      COMMON /ADVENT/ GRID, GX, GY, MOVEX, MOVEY
      INTEGER GRID(132,132), GX, GY, MOVEX(4), MOVEY(4)
C
C     Initialise moves - Move<dir>(1) is up, 2 is 90deg etc.
      DATA MOVEX /0,1,0,-1/
      DATA MOVEY /-1,0,1,0/
C
      CHARACTER*130 STR
      INTEGER X,Y,MOVET,TOTAL
C
   10 FORMAT(A)
   30 FORMAT(A,I4)
C
      WRITE(*,10)"Advent of Code 2024 day 6, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day6in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Pad the top row of the grid with 5's to mark out of bounds
      DO 70 X=1,132
        GRID(X,1)=5
   70 CONTINUE
C     Pad the bottom row of the grid with 5's
      DO 80 X=1,132
        GRID(X,132)=5
   80 CONTINUE
      TOTAL=0
      Y=1
   90 CONTINUE
C     Read each input line
      READ(10,FMT=10,ERR=100,END=100)STR
      Y=Y+1
C     Place into the grid
      CALL GRIDINIT(STR,Y)
      GOTO 90
  100 CONTINUE
      CLOSE(10)
C
C     Move the guard until the edge of the grid is reached, counting
C     distinct grid positions visited by the guard
C
C     Mark guard starting position as first grid square visited
      GRID(GX,GY)=0
      TOTAL=1
      WRITE(*,30)"Running total of unique squares is ",TOTAL
C     Move the guard up the grid to start with
      MOVET=1
C     While we're still on the 130x130 grid
      DO 200 WHILE (GRID(GX+MOVEX(MOVET),GY+MOVEY(MOVET)).NE.5)
C       Do we need to turn?
        IF (GRID(GX+MOVEX(MOVET),GY+MOVEY(MOVET)).EQ.2) THEN
          MOVET=MOVET+1
          IF (MOVET.EQ.5) MOVET=1
          WRITE(*,10)"Turning ..."
        ELSE
C         Move to next grid square
          GX=GX+MOVEX(MOVET)
          GY=GY+MOVEY(MOVET)
C         Add 1 to TOTAL if we've not visited it before
          TOTAL=TOTAL+GRID(GX,GY)
C         Set this square to zero so we don't add again if revisited
          GRID(GX,GY)=0
          WRITE(*,30)"Running total of unique squares is ",TOTAL
        ENDIF
  200 CONTINUE
      WRITE(*,10)" "
      WRITE(*,30)"Result is ",TOTAL
      END
C
      SUBROUTINE GRIDINIT(STR,Y)
      CHARACTER*(*)STR
      INTEGER Y
C
      COMMON /ADVENT/ GRID, GX, GY, MOVEX, MOVEY
      INTEGER GRID(132,132), GX, GY, MOVEX(4), MOVEY(4)
C
      INTEGER L1
C
C     Pad the edge of the grid with 5 (arbitrary value)
C
      GRID(1,Y)=5
      GRID(132,Y)=5
      DO 100 L1=1,130
C       Mark unvisited spaces with 1
        IF (STR(L1:L1).EQ.'.') GRID(L1+1,Y)=1
C       Mark obstacles with 2
        IF (STR(L1:L1).EQ.'#') GRID(L1+1,Y)=2
C       Mark guard starting point with 3
        IF (STR(L1:L1).EQ.'^') THEN 
          GRID(L1+1,Y)=3
          GX=L1+1
          GY=Y
        ENDIF
100   CONTINUE
      RETURN
      END
