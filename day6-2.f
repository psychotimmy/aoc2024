      PROGRAM DAY6P2
      COMMON /ADVENT/ GRID, GX, GY, GXS, GYS, MOVEX, MOVEY
      INTEGER GRID(132,132), GX, GY, GXS, GYS,MOVEX(4), MOVEY(4)
C
C     Initialise moves - Move<dir>(1) is up, 2 is 90deg etc.
      DATA MOVEX /0,1,0,-1/
      DATA MOVEY /-1,0,1,0/
C
      CHARACTER*130 STR
      INTEGER X,Y,MOVET,TOTAL,TRYOBSTACLE
C
   10 FORMAT(A)
   20 FORMAT(A,I4,I4)
   30 FORMAT(A,I4)
   40 FORMAT(A,I4,A)
C
      WRITE(*,10)"Advent of Code 2024 day 6, part 2"
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
C     Move the guard up the grid to start with
      MOVET=1
C     While we're still on the 130x130 grid
      DO 200 WHILE (GRID(GX+MOVEX(MOVET),GY+MOVEY(MOVET)).NE.5)
C       Do we need to turn?
        IF (GRID(GX+MOVEX(MOVET),GY+MOVEY(MOVET)).EQ.2) THEN
          MOVET=MOVET+1
          IF (MOVET.EQ.5) MOVET=1
        ELSE
C         Move to next grid square
          GX=GX+MOVEX(MOVET)
          GY=GY+MOVEY(MOVET)
C         Add 1 to TOTAL if we've not visited it before
          TOTAL=TOTAL+GRID(GX,GY)
C         Set this square to zero so we don't add again if revisited
          GRID(GX,GY)=0
        ENDIF
  200 CONTINUE
      WRITE(*,10)" "
      WRITE(*,40)"Need to test ",TOTAL-1," grid squares"
      WRITE(*,10)" "
C
C     We now have a grid with the only squares on it marked as 0
C     that it makes sense to try an obstacle in. But we need to
C     disallow the guards starting point first ...
C
C
      GRID(GXS,GYS)=3
      TOTAL=0
C
      DO 500 Y=2,131
        DO 400 X=2,131
          IF (GRID(X,Y).EQ.0) THEN
            WRITE(*,20)"Testing grid square ",X,Y
            TOTAL=TOTAL+TRYOBSTACLE(X,Y)
            WRITE(*,30)"Running total of possible loops is ",TOTAL
            WRITE(*,10)" "
          ENDIF
  400   CONTINUE
  500 CONTINUE
      WRITE(*,30)"Result is ",TOTAL
      END
C
      SUBROUTINE GRIDINIT(STR,Y)
      CHARACTER*(*)STR
      INTEGER Y
C
      COMMON /ADVENT/ GRID, GX, GY, GXS, GYS, MOVEX, MOVEY
      INTEGER GRID(132,132), GX, GY, GXS, GYS,MOVEX(4), MOVEY(4)
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
C       Save the starting point permanently
          GXS=GX
          GYS=GY
        ENDIF
100   CONTINUE
      RETURN
      END
C
      INTEGER FUNCTION TRYOBSTACLE(X,Y)
      INTEGER X,Y
C
      COMMON /ADVENT/ GRID, GX, GY, GXS, GYS, MOVEX, MOVEY
      INTEGER GRID(132,132), GX, GY, GXS, GYS,MOVEX(4), MOVEY(4)
C
      INTEGER GRIDT(132,132), X1, Y1, MOVET
   10 FORMAT(A)
C
C     Make a copy of the master grid
C
      DO 30 Y1=1,132
        DO 20 X1=1,132
          GRIDT(X1,Y1)=GRID(X1,Y1)
   20   CONTINUE 
   30 CONTINUE
C
C     Place the new obstacle at X,Y
C
      GRIDT(X,Y)=2
C
C     Move the guard. Assume if we've visited the same square
C     10 times we have a loop (BIG ASSUMPTION - my input works
C     if the test is 5 times though ...)
C
C     Mark guard starting position as first grid square visited
      GRIDT(GXS,GYS)=0
C     Reset GX and GY as they will have changed (common vars)
      GX=GXS
      GY=GYS
C     Move the guard up the grid to start with
      MOVET=1
C     While we're still on the 130x130 grid
      DO 200 WHILE (GRIDT(GX+MOVEX(MOVET),GY+MOVEY(MOVET)).NE.5)
C       Do we need to turn?
        IF (GRIDT(GX+MOVEX(MOVET),GY+MOVEY(MOVET)).EQ.2) THEN
          MOVET=MOVET+1
          IF (MOVET.EQ.5) MOVET=1
        ELSE
C         Move to next grid square
          GX=GX+MOVEX(MOVET)
          GY=GY+MOVEY(MOVET)
C         Subtract 1 from this square
          GRIDT(GX,GY)=GRIDT(GX,GY)-1
          IF (GRIDT(GX,GY).LE.-10) THEN
C           We have a loop!
            WRITE(*,10)"Loop found!"
            TRYOBSTACLE=1
            GOTO 999
          ENDIF
        ENDIF
  200 CONTINUE
      TRYOBSTACLE=0
  999 CONTINUE
      RETURN
      END
