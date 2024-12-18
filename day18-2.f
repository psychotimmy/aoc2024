      PROGRAM DAY18P2
C
      INTEGER XSIZE,YSIZE,BSIZE
      PARAMETER (XSIZE=73,YSIZE=73,BSIZE=3450)
C
      INTEGER TOTAL,GRID(0:XSIZE-1,0:YSIZE-1),X,Y,N,TRY
      INTEGER ASTAR,XS,YS,XE,YE
      COMPLEX BLOCKS(BSIZE),B
C
   10 FORMAT(A)
   20 FORMAT(A,I10,A)
   30 FORMAT(A,I2,A,I2)
C
      WRITE(*,10)"Advent of Code 2024 day 18, part 2"
      WRITE(*,10)" "
      OPEN(10,FILE="day18in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C
      CALL READBLOCKS(10,BLOCKS,BSIZE)
      CLOSE(10)
C
      TRY=1023
   50 CONTINUE
C
C     Initialise grid with zeros - will become min cost of reaching each
C     square from 1,1
C
      DO Y=1,YSIZE-2
        DO X=1,XSIZE-2
          GRID(X,Y)=0
        ENDDO
      ENDDO
C
C     Border for grid
      DO N=0,XSIZE-1
        GRID(0,N)=99999
        GRID(N,0)=99999
        GRID(XSIZE-1,N)=99999
        GRID(N,YSIZE-1)=99999
      ENDDO
C
C     Populate grid with N blocks (1024 for part 1)
      TRY=TRY+1
      DO N=1,TRY
        B=BLOCKS(N)
        GRID((INT(REAL(B))),(INT(AIMAG(B))))=99999
      ENDDO
C
C     Walk grid from start (1,1) to finish (71,71)
      XS=1
      YS=1
      XE=71
      YE=71
      TOTAL=ASTAR(GRID,COMPLEX(XS,YS),COMPLEX(XE,YE))
      IF (TOTAL.GT.0) THEN
        WRITE(*,20)"Minimum distance to escape is",TOTAL," steps"
        WRITE(*,20)"after",TRY," blocks fell."
        GOTO 50
      ELSE
        WRITE(*,20)"Path to exit blocked after",IABS(TOTAL)," steps"
        WRITE(*,20)"A total of",TRY," blocks fell"
        WRITE(*,*)"Last block fell at ",BLOCKS(TRY)
        WRITE(*,30)"Which without grid padding is ",
     +             INT(REAL(BLOCKS(TRY)))-1,",",
     +             INT(AIMAG(BLOCKS(TRY)))-1
      ENDIF
      END
C
      INTEGER FUNCTION ASTAR(G,START,GOAL)
      INTEGER G(0:72,0:72)
      COMPLEX START,GOAL
C
      COMPLEX OPENL(5000,4),CLOSEDL(5000)
      INTEGER OPENPTR,CLOSEDPTR,LOWFVAL,PTR,NUM,L4,L3,L1,L2,R,I
      COMPLEX MOVE(4),CHILD(4,4),CURRENT(1,4),NEXTNODE
      LOGICAL GOALREACHED
C
   10 FORMAT(A)
      GOALREACHED=.FALSE.
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
      OPENL(OPENPTR,2)=COMPLEX(0,0)
      OPENL(OPENPTR,3)=COMPLEX(0,0)
      OPENL(OPENPTR,4)=COMPLEX(0,0)
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
            LOWFVAL=OPENL(PTR,2)
          ENDIF
        ENDDO
C-------Shuffle the open list up one from PTR to OPENPTR-1
        DO L1=PTR,OPENPTR
          OPENL(L1,1)=OPENL(L1+1,1)
          OPENL(L1,2)=OPENL(L1+1,2)
          OPENL(L1,3)=OPENL(L1+1,3)
          OPENL(L1,4)=OPENL(L1+1,4)
        ENDDO
C-------Decrement OPENPTR 
        OPENPTR=OPENPTR-1
C-------Move the current node to the closed list
        CLOSEDPTR=CLOSEDPTR+1
        CLOSEDL(CLOSEDPTR)=CURRENT(1,1)
C-------Is the current node the goal?
        IF (CURRENT(1,1).EQ.GOAL) THEN
          WRITE(*,10)"Goal reached!"
          GOALREACHED=.TRUE.
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
            IF (CLOSEDL(L4).EQ.CHILD(L3,1)) THEN 
              GOTO 200
            ENDIF
          ENDDO
C---------Child is NOT on the closed list
C---------Child g is CURRENT's g + distance from child to current (1)
          CHILD(L3,3)=CURRENT(1,3)+COMPLEX(1,0) 
C---------Child h is heuristic distance to goal (0 = Dijkstra)
          CHILD(L3,4)=COMPLEX(0,0)
C---------Child f value is the sum of g and h
          CHILD(L3,2)=CHILD(L1,3)+CHILD(L3,4)
C---------Is the child already in the open list?
          DO L2=1,OPENPTR
            IF (OPENL(L2,1).EQ.CHILD(L3,1)) THEN
C-------------Update child's g if lower than current g
              IF (INT(REAL(OPENL(L2,3))).GT.
     +            INT(REAL(CHILD(L3,3)))) THEN
                OPENL(L2,3)=CHILD(L3,3)
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
  200   CONTINUE
        ENDDO
      ENDDO
C
  300 CONTINUE
      IF (GOALREACHED) THEN
        ASTAR=INT(REAL(CURRENT(1,3)))
      ELSE
        ASTAR=0-INT(REAL(CURRENT(1,3)))
      ENDIF
      RETURN
      END
C
      SUBROUTINE READBLOCKS(FILE,BLOCKS,N)
      INTEGER FILE,N
      COMPLEX BLOCKS(*)
C
      CHARACTER*50 COORDS
      INTEGER L1,R,I
C
   10 FORMAT(A)
   20 FORMAT(I3)
   30 FORMAT(2I3)
C
C     Read the co-ordinates and convert to complex numbers
      DO L1=1,N
        READ(FILE,FMT=10)COORDS
        READ(COORDS(1:INDEX(COORDS,',')-1),20) R
        READ(COORDS(INDEX(COORDS,',')+1:),20) I
        R=R+1
        I=I+1
        BLOCKS(L1)=COMPLEX(R,I)
      ENDDO
C
      RETURN
      END
