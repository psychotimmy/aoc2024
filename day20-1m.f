      PROGRAM DAY20P1
C
      INTEGER XSIZE,YSIZE,BSIZE
      PARAMETER (XSIZE=143,YSIZE=143)
C
      INTEGER TARGET,GRID(0:XSIZE-1,0:YSIZE-1),X,Y,L1,L2,N,TOTAL
      INTEGER ASTAR
      COMPLEX START,FINISH,ROUTE(20000),NEWROUTE(20000)
      COMPLEX CHEAT(12),CHECK
C
   10 FORMAT(A)
   20 FORMAT(A,I10,A)
   30 FORMAT(A,I6)
C
      WRITE(*,10)"Advent of Code 2024 day 20, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day20in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C
C     Initialise grid from file - add a border so we don't have to
C     bother with edge cases when we remove obstacles for 2 steps.
C
      CALL READGRID(10,GRID,XSIZE,YSIZE,START,FINISH)
      CLOSE(10)
C     Walk grid from START to FINISH, return route
      TARGET=ASTAR(GRID,XSIZE,YSIZE,START,FINISH,ROUTE)
      WRITE(*,20)"Original racetrack distance is",TARGET," steps"
      WRITE(*,10)" "
C
C     Cheat by removing all obstacles for two steps for each step
C     taken on the original route. If not in a wall at the end of
C     these steps, recalculate the minimum path to the end. There
C     are a maximum of 12 cheats to try if walls are removed for
C     2 picoseconds. Note that if TARGET is N, there are N+1 steps
C     recorded in the route as ROUTE(1) is START and ROUTE(N+1) is
C     finish. We don't need to cheat if we're at the goal, so number
C     of positions to cheat at is TARGET (same number as steps).
C
      TOTAL=0
C
      DO L1=1,TARGET
        WRITE(*,30) "Evaluating cheats for step ",L1
        CHEAT(1)=ROUTE(L1)+COMPLEX(2,0)
        CHEAT(2)=ROUTE(L1)+COMPLEX(1,0)
        CHEAT(3)=ROUTE(L1)+COMPLEX(-2,0)
        CHEAT(4)=ROUTE(L1)+COMPLEX(-1,0)
        CHEAT(5)=ROUTE(L1)+COMPLEX(0,2)
        CHEAT(6)=ROUTE(L1)+COMPLEX(0,1)
        CHEAT(7)=ROUTE(L1)+COMPLEX(0,-2)
        CHEAT(8)=ROUTE(L1)+COMPLEX(0,-1)
C
        DO L2=1,8,2
          START=CHEAT(L2)
          CHECK=CHEAT(L2+1)
C         Check that we're not starting from a wall and that the cheat
C         isn't simply following the path
          IF (GRID(INT(REAL(START)),INT(AIMAG(START))).EQ.99999) THEN
C           WRITE(*,*)"Segmentation fault at ",START
          ELSE
            IF (GRID(INT(REAL(CHECK)),INT(AIMAG(CHECK))).EQ.99999) THEN
              N=ASTAR(GRID,XSIZE,YSIZE,START,FINISH,NEWROUTE)+2+(L1-1)
C             Value of route is 2 more than calculated + number of steps
C             taken BEFORE the cheat happened
              WRITE(*,20)"New racetrack distance is",N," steps"
              IF ((TARGET-N).GE.100) THEN
                WRITE(*,20)"Saving",TARGET-N," picoseconds!"
                TOTAL=TOTAL+1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      WRITE(*,10)" "
      WRITE(*,20)"Cheats saving at least 100 picoseconds is",TOTAL
      END
C
      INTEGER FUNCTION MANHATTAN(P1,P2)
      COMPLEX P1,P2
C
      INTEGER I,J
C
      I=IABS(INT(REAL(P1))-INT(REAL(P2)))
      J=IABS(INT(AIMAG(P1))-INT(AIMAG(P2)))
      MANHATTAN=I+J
      RETURN
      END
C
      INTEGER FUNCTION ASTAR(G,XSIZE,YSIZE,START,GOAL,ROUTE)
      INTEGER G(0:XSIZE-1,0:YSIZE-1),XSIZE,YSIZE
      COMPLEX START,GOAL,ROUTE(*)
C
      COMPLEX OPENL(XSIZE*YSIZE,5),CLOSEDL(XSIZE*YSIZE,5)
      INTEGER OPENPTR,CLOSEDPTR,LOWFVAL,PTR,NUM,L4,L3,L1,L2,R,I
      INTEGER MANHATTAN
      COMPLEX MOVE(4),CHILD(4,5),CURRENT(1,5),NEXTNODE
C
   10 FORMAT(A)
C     Order of move array is up,right,down,left
      MOVE(1)=COMPLEX(0,-1)
      MOVE(2)=COMPLEX(1,0)
      MOVE(3)=COMPLEX(0,1)
      MOVE(4)=COMPLEX(-1,0)
C
      CLOSEDPTR=0
      OPENPTR=1
      OPENL(OPENPTR,1)=START
C-----The real parts of indices 2,3 & 4 represent node's f,g & h values
      OPENL(OPENPTR,3)=COMPLEX(0,0)
      OPENL(OPENPTR,4)=MANHATTAN(START,GOAL)
      OPENL(OPENPTR,2)=OPENL(OPENPTR,3)+OPENL(OPENPTR,4)
C-----index 5 = parent node (-1,-1) for start node
      OPENL(OPENPTR,5)=COMPLEX(-1,-1)
C-----Get node with lowest f value from the open list
      DO WHILE (OPENPTR.GT.0)
        LOWFVAL=999999
        DO L1=1,OPENPTR
          IF (INT(REAL(OPENL(L1,2))).LT.LOWFVAL) THEN
            PTR=L1
            CURRENT(1,1)=OPENL(PTR,1)
            CURRENT(1,2)=OPENL(PTR,2)
            CURRENT(1,3)=OPENL(PTR,3)
            CURRENT(1,4)=OPENL(PTR,4)
            CURRENT(1,5)=OPENL(PTR,5)
            LOWFVAL=OPENL(PTR,2)
          ENDIF
        ENDDO
C-------Shuffle the open list up one from PTR to OPENPTR-1
        DO L1=PTR,OPENPTR
          OPENL(L1,1)=OPENL(L1+1,1)
          OPENL(L1,2)=OPENL(L1+1,2)
          OPENL(L1,3)=OPENL(L1+1,3)
          OPENL(L1,4)=OPENL(L1+1,4)
          OPENL(L1,5)=OPENL(L1+1,5)
        ENDDO
C-------Decrement OPENPTR 
        OPENPTR=OPENPTR-1
C-------Move the current node to the closed list
        CLOSEDPTR=CLOSEDPTR+1
        CLOSEDL(CLOSEDPTR,1)=CURRENT(1,1)
        CLOSEDL(CLOSEDPTR,2)=CURRENT(1,2)
        CLOSEDL(CLOSEDPTR,3)=CURRENT(1,3)
        CLOSEDL(CLOSEDPTR,4)=CURRENT(1,4)
        CLOSEDL(CLOSEDPTR,5)=CURRENT(1,5)
C-------Is the current node the goal?
        IF (CURRENT(1,1).EQ.GOAL) THEN
C         WRITE(*,10)"Goal reached!"
          GOTO 300
        ENDIF
C-------Find all the legal child node of the current node (up to 4)
        NUM=0
        DO L1=1,4
          NEXTNODE=CURRENT(1,1)+MOVE(L1)
          R=INT(REAL(NEXTNODE))
          I=INT(AIMAG(NEXTNODE))
          IF (G(R,I).NE.99999) THEN
            NUM=NUM+1
            CHILD(NUM,1)=NEXTNODE
          ENDIF
        ENDDO
C-------Are the children of the current node on the closed list? 
        DO L3=1,NUM
          DO L4=1,CLOSEDPTR
            IF (CLOSEDL(L4,1).EQ.CHILD(L3,1)) THEN 
              GOTO 200
            ENDIF
          ENDDO
C---------Child is NOT on the closed list
C---------Child g is CURRENT's g + distance from child to current (1)
          CHILD(L3,3)=CURRENT(1,3)+COMPLEX(1,0) 
C---------Child h is heuristic distance to goal (0 = Dijkstra)
          CHILD(L3,4)=MANHATTAN(CHILD(L3,1),GOAL)
C---------Child f value is the sum of g and h
          CHILD(L3,2)=CHILD(L1,3)+CHILD(L3,4)
C---------Save the parent node for recreating path
          CHILD(L3,5)=CURRENT(1,1)
C---------Is the child already in the open list?
          DO L2=1,OPENPTR
            IF (OPENL(L2,1).EQ.CHILD(L3,1)) THEN
C-------------Update child's f,g,h if lower than current g
C-------------and also the parent node
              IF (INT(REAL(OPENL(L2,3))).GT.
     +            INT(REAL(CHILD(L3,3)))) THEN
                OPENL(L2,2)=CHILD(L3,2)
                OPENL(L2,3)=CHILD(L3,3)
                OPENL(L2,4)=CHILD(L3,4)
                OPENL(L2,5)=CHILD(L3,5)
              ENDIF
            GOTO 200
            ENDIF
          ENDDO
C---------Child not on open list if we get here, update list
          OPENPTR=OPENPTR+1 
          OPENL(OPENPTR,1)=CHILD(L3,1)
          OPENL(OPENPTR,2)=CHILD(L3,2)
          OPENL(OPENPTR,3)=CHILD(L3,3)
          OPENL(OPENPTR,4)=CHILD(L3,4)
          OPENL(OPENPTR,5)=CHILD(L3,5)
  200   CONTINUE
        ENDDO
      ENDDO
C
  300 CONTINUE
C
C     Return value is length of ROUTE
      ASTAR=INT(REAL(CURRENT(1,3)))
C
C     Save path, return in ROUTE array.
C     START is at index 1, GOAL at index ASTAR+1
C
      DO L1=ASTAR+1,1,-1
        ROUTE(L1)=CURRENT(1,1)
        DO L2=CLOSEDPTR,1,-1
C         Should only match one entry on the closed list
C         Escape L2 loop when we do
          IF (CLOSEDL(L2,1).EQ.CURRENT(1,5)) THEN
            CURRENT(1,1)=CLOSEDL(L2,1)
            CURRENT(1,5)=CLOSEDL(L2,5)
            GOTO 400
          ENDIF
        ENDDO
  400   CONTINUE
      ENDDO
C       
      RETURN
      END
C
      SUBROUTINE READGRID(FILE,GRID,XSIZE,YSIZE,S,E)
      INTEGER FILE,GRID(0:XSIZE-1,0:YSIZE-1),XSIZE,YSIZE
      COMPLEX S,E
C
      INTEGER X,Y
      CHARACTER*150 ROW
   10 FORMAT(A)
   20 FORMAT(17I1)
C
C     Read the grid. Convert '#' to obstacles (99999) and add
C     a border of '#' all the way around
C
      DO X=0,XSIZE-1
        GRID(X,0)=99999
        GRID(X,YSIZE-1)=99999
      ENDDO
      DO Y=0,YSIZE-1
        GRID(0,Y)=99999
        GRID(XSIZE-1,Y)=99999
      ENDDO
C
      DO Y=1,YSIZE-2
        READ(FILE,FMT=10,ERR=999,END=999) ROW
        DO X=1,XSIZE-2
          IF (ROW(X:X).EQ.'#') THEN
            GRID(X,Y)=99999
          ELSE IF (ROW(X:X).EQ.'.') THEN
            GRID(X,Y)=0
          ELSE IF (ROW(X:X).EQ.'S') THEN
            GRID(X,Y)=0
            S=COMPLEX(X,Y)
          ELSE IF (ROW(X:X).EQ.'E') THEN
            GRID(X,Y)=0
            E=COMPLEX(X,Y)
          ENDIF
        ENDDO
      ENDDO
C     DO Y=0,YSIZE-1
C       WRITE(*,20)(GRID(X,Y),X=0,XSIZE-1)
C     ENDDO
C
  999 CONTINUE
      RETURN
      END
