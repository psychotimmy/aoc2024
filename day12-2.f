      PROGRAM DAY12P2
      COMMON /ADVENT/ GRID
      INTEGER GRID(0:141,0:141)
C
      CHARACTER*140 STR
      INTEGER X,Y,AREA,CORNERS,VAL,TOTAL
C
   10 FORMAT(A)
   20 FORMAT(I3)
   30 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2024 day 12, part 2"
      WRITE(*,10)" "
      OPEN(10,FILE="day12in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C
C     Zero a 2d grid for use with a border of 0 to avoid edge cases
C
      DO Y=0,141
        DO X=0,141
          GRID(X,Y)=0
        ENDDO
      ENDDO
C
      Y=1
   50 CONTINUE
C     Read the input into a temporary string
      READ(10,FMT=10,ERR=100,END=100)STR
C     Convert to an integer 2d grid - chars are A-Z, so store as 1-26
      DO X=1,140
        GRID(X,Y)=ICHAR(STR(X:X))-64
      ENDDO
      Y=Y+1
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C
C     Walk the grid from (1,1) - work out how large each connected area
C     is - number of squares plus the perimeter. Negate visited squares
C     in the grid as they are counted to make sure we don't see them twice
C
      DO Y=1,140
        DO X=1,140
          IF (GRID(X,Y).GT.0) THEN
            AREA=0
            CORNERS=0
            VAL=GRID(X,Y)
            CALL WALK(X,Y,VAL,AREA,CORNERS)
            TOTAL=TOTAL+AREA*CORNERS
            WRITE(*,30)"Running price of fencing is ",TOTAL
          ENDIF
        ENDDO
      ENDDO
C
      WRITE(*,10)" "
      WRITE(*,30)"Fencing for all regions will cost ",TOTAL
      END
C
      RECURSIVE SUBROUTINE WALK(X,Y,VAL,AREA,CORNERS)
      INTEGER X,Y,VAL,AREA,CORNERS
C
      COMMON /ADVENT/ GRID
      INTEGER GRID(0:141,0:141)
C
      INTEGER X1,Y1,A,B,C,D,F,G,H,I
C
C     Mark grid square as visited, area of one square is 
C     always 1, fence change of direction = number of corners
C     Count interior corners and exterior corners to ensure uniqueness 
C
      GRID(X,Y)=-GRID(X,Y)
      AREA=AREA+1
C     Corners - need the 8 values around VAL    A   B   C
C                                               D       F
C                                               G   H   I
      A=IABS(GRID(X-1,Y-1))
      B=IABS(GRID(X,Y-1))
      C=IABS(GRID(X+1,Y-1))
      D=IABS(GRID(X-1,Y))
      F=IABS(GRID(X+1,Y))
      G=IABS(GRID(X-1,Y+1))
      H=IABS(GRID(X,Y+1))
      I=IABS(GRID(X+1,Y+1))
C     Exterior corners
      IF ((VAL.NE.D).AND.(VAL.NE.B)) CORNERS=CORNERS+1
      IF ((VAL.NE.B).AND.(VAL.NE.F)) CORNERS=CORNERS+1
      IF ((VAL.NE.F).AND.(VAL.NE.H)) CORNERS=CORNERS+1
      IF ((VAL.NE.H).AND.(VAL.NE.D)) CORNERS=CORNERS+1
C     Interior corners
      IF ((VAL.EQ.D).AND.(VAL.EQ.B)
     +              .AND.(VAL.NE.A)) CORNERS=CORNERS+1
      IF ((VAL.EQ.B).AND.(VAL.EQ.F)
     +              .AND.(VAL.NE.C)) CORNERS=CORNERS+1
      IF ((VAL.EQ.F).AND.(VAL.EQ.H) 
     +              .AND.(VAL.NE.I)) CORNERS=CORNERS+1
      IF ((VAL.EQ.H).AND.(VAL.EQ.D) 
     +              .AND.(VAL.NE.G)) CORNERS=CORNERS+1
C
      IF (GRID(X+1,Y).EQ.VAL) THEN
         X1=X+1
         Y1=Y
         CALL WALK(X1,Y1,VAL,AREA,CORNERS)
      ENDIF
      IF (GRID(X,Y-1).EQ.VAL) THEN
         X1=X
         Y1=Y-1
         CALL WALK(X1,Y1,VAL,AREA,CORNERS)
      ENDIF
      IF (GRID(X-1,Y).EQ.VAL) THEN
         X1=X-1
         Y1=Y
         CALL WALK(X1,Y1,VAL,AREA,CORNERS)
      ENDIF
      IF (GRID(X,Y+1).EQ.VAL) THEN
         X1=X
         Y1=Y+1
         CALL WALK(X1,Y1,VAL,AREA,CORNERS)
      ENDIF
C
      RETURN
      END
