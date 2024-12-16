      PROGRAM DAY15P2
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:19,0:9),INST(700)
      COMMON /TEMP/ TGRID,MN
      INTEGER TGRID(0:19,0:9),MN
C
      INTEGER I,N,X,Y
      INTEGER*8 TOTAL,TEMP
C
   10 FORMAT(A)
   20 FORMAT(A,I16)
   30 FORMAT(A,I2,A,I6,A,I8)
C
      WRITE(*,10)"Advent of Code 2024 day 15, part 2"
      WRITE(*,10)" "
      OPEN(10,FILE="day15in.sample",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      CALL READGRID(10,X,Y)
      CALL READINST(10,N)
      CLOSE(10)
C
      CALL SUMGPS(TOTAL)
      WRITE(*,20)"Starting GPS sum is",TOTAL
      CALL PRINTGRID
C
      DO I=1,N
        MN=I
        CALL MOVEBOT(X,Y,INST(I))
        CALL SUMGPS(TOTAL)
        WRITE(*,30)"Last ",INST(I)," sum after",I," moves is",TOTAL
        CALL PRINTGRID
        TEMP=TOTAL
      ENDDO
C   
      END
C
      SUBROUTINE READGRID(FILE,X0,Y0)
      INTEGER FILE,X0,Y0
C
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:19,0:9),INST(700)
C
      CHARACTER*50 ROW 
      INTEGER X,Y
C
   10 FORMAT(A)
   20 FORMAT(I5,A)
C
      Y=0
   50 CONTINUE
      READ(FILE,FMT=10,END=999,ERR=999)ROW
C     Exit fast at end of grid
      IF (ROW.EQ." ") GOTO 999
      DO X=1,20,2
        IF (ROW(X/2+1:X/2+1).EQ.'#') THEN
          GRID(X-1,Y)=7
          GRID(X,Y)=7
        ELSE IF (ROW(X/2+1:X/2+1).EQ.'.') THEN
          GRID(X-1,Y)=0
          GRID(X,Y)=0
        ELSE IF (ROW(X/2+1:X/2+1).EQ.'O') THEN
          GRID(X-1,Y)=1
          GRID(X,Y)=-1
        ELSE IF (ROW(X/2+1:X/2+1).EQ.'@') THEN
          X0=X-1
          Y0=Y
          GRID(X-1,Y)=2
          GRID(X,Y)=0
        ENDIF
      ENDDO
      Y=Y+1
      GOTO 50
  999 CONTINUE
      WRITE(*,20)Y," grid rows read from file"
      RETURN
      END
C
      SUBROUTINE READINST(FILE,NUM)
      INTEGER FILE,NUM
C
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:19,0:9),INST(700)
C
      CHARACTER*70 ROW
      INTEGER X,Y
C
   10 FORMAT(A)
   20 FORMAT(I5,A)
C
      Y=0
   50 CONTINUE
      READ(FILE,FMT=10,END=999,ERR=999)ROW
      DO X=1,70
        IF (ROW(X:X).EQ.'^') THEN
          INST(Y*70+X)=1
        ELSE IF (ROW(X:X).EQ.'>') THEN
          INST(Y*70+X)=2
        ELSE IF (ROW(X:X).EQ.'v') THEN
          INST(Y*70+X)=3
        ELSE IF (ROW(X:X).EQ.'<') THEN
          INST(Y*70+X)=4
        ENDIF
      ENDDO
      Y=Y+1
      GOTO 50
  999 CONTINUE
      NUM=Y*70
      WRITE(*,20)Y," instruction rows read from file"
      WRITE(*,20)NUM," instructions read from file"
      RETURN
      END
C
      SUBROUTINE SUMGPS(GPS)
      INTEGER*8 GPS
C
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:19,0:9),INST(700)
C
      INTEGER X,Y
C
      GPS=0
      DO Y=0,9
        DO X=0,19
          IF (GRID(X,Y).EQ.1) GPS=GPS+100*Y+X
        ENDDO
      ENDDO
      RETURN
      END
C
      RECURSIVE INTEGER FUNCTION 
     +          MOVEVERT(OFFSET,XLEFT,XRIGHT,Y) RESULT(RES)
      INTEGER OFFSET,XLEFT,XRIGHT,Y
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:19,0:9),INST(700)
C
      COMMON /TEMP/ TGRID,MN
      INTEGER TGRID(0:19,0:9),MN
C
      INTEGER ROW,XX
   10 FORMAT (A,I4,I4,A,I4)
C
C     On entry we have one box to move vertically by OFFSET row. Also,
C     fix grid in case two boxes pushed one box in the previous move.
C
      DO XX=2,17
        IF (TGRID(XX,Y).EQ.1) THEN
          IF (TGRID(XX+1,Y).NE.-1) THEN
            TGRID(XX+1,Y)=-1
            WRITE(*,10)"Fixed rhs of box at",XX,XX+1," row",Y
            STOP 8
          ENDIF
        ENDIF
      ENDDO
      DO XX=17,2,-1
        IF (TGRID(XX,Y).EQ.-1) THEN
          IF (TGRID(XX-1,Y).NE.1) THEN
            TGRID(XX-1,Y)=1
            WRITE(*,10)"Fixed lhs of box at",XX-1,XX," row",Y
            STOP 8
          ENDIF
        ENDIF
      ENDDO
      ROW=Y+OFFSET
      RES=1
C
C     If either side of this box hits a wall, game over
C
      IF ((GRID(XLEFT,ROW).EQ.7).OR.(GRID(XRIGHT,ROW).EQ.7)) THEN
        RES=-999
        GOTO 999
      ENDIF
C
C     If we have spaces 'above' both sides of the box, change TGRID
      IF ((GRID(XLEFT,ROW).EQ.0).AND.(GRID(XRIGHT,ROW).EQ.0)) THEN
        TGRID(XLEFT,ROW)=1
        TGRID(XRIGHT,ROW)=-1
C     4 more possibilities - box 'above', or 1 or 2 overlapping
      ELSE IF (GRID(XLEFT,ROW).EQ.GRID(XLEFT,Y)) THEN
C       Box directly above
        RES=MOVEVERT(OFFSET,XLEFT,XRIGHT,ROW)
      ELSE IF ((GRID(XLEFT,ROW).EQ.-1).AND.
     +         (GRID(XRIGHT,ROW).EQ.1)) THEN
C       Boxes 'above' to the left and to the right
        TGRID(XLEFT,ROW)=1
        TGRID(XRIGHT,ROW)=-1
        TGRID(XLEFT-1,ROW)=0
C
        TGRID(XLEFT,ROW)=1
        TGRID(XRIGHT,ROW)=-1
        TGRID(XRIGHT+1,ROW)=0
        RES=MOVEVERT(OFFSET,XLEFT-1,XLEFT,ROW)
        RES=MOVEVERT(OFFSET,XRIGHT,XRIGHT+1,ROW)
      ELSE IF (GRID(XLEFT,ROW).EQ.-1) THEN
C       Box 'above' to the left
        TGRID(XLEFT,ROW)=1
        TGRID(XRIGHT,ROW)=-1
        TGRID(XLEFT-1,ROW)=0
        RES=MOVEVERT(OFFSET,XLEFT-1,XLEFT,ROW)
      ELSE IF (GRID(XRIGHT,ROW).EQ.1) THEN
C       Box 'above' to the right
        TGRID(XLEFT,ROW)=1
        TGRID(XRIGHT,ROW)=-1
        TGRID(XRIGHT+1,ROW)=0
        RES=MOVEVERT(OFFSET,XRIGHT,XRIGHT+1,ROW)
      ENDIF
  999 CONTINUE
      RETURN
      END
C
      SUBROUTINE MOVEBOT(X,Y,DIRECTION)
      INTEGER X,Y,DIRECTION
C
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:19,0:9),INST(700)
      COMMON /TEMP/ TGRID,MN
      INTEGER TGRID(0:19,0:9),MN
C
      INTEGER MOVEX(4),MOVEY(4),XNEW,YNEW,YM,XM,XX,YY,XL,XR
      INTEGER L1,L2,RES
      DATA MOVEX /0,1,0,-1/
      DATA MOVEY /-1,0,1,0/
C
      XNEW=X+MOVEX(DIRECTION)
      YNEW=Y+MOVEY(DIRECTION)
C
C     Note: do nothing if GRID(XNEW,YNEW).EQ.7 (wall)
C
      IF (GRID(XNEW,YNEW).EQ.0) THEN
        GRID(X,Y)=0
        GRID(XNEW,YNEW)=2
        X=XNEW
        Y=YNEW
      ELSE IF (IABS(GRID(XNEW,YNEW)).EQ.1) THEN
        IF (DIRECTION.EQ.1) THEN
          IF (GRID(XNEW,YNEW).EQ.1) THEN
            XL=XNEW
            XR=XNEW+1
          ELSE
            XL=XNEW-1
            XR=XNEW
          ENDIF
          DO L2=0,9
            DO L1=0,19
              TGRID(L1,L2)=GRID(L1,L2)
            ENDDO
          ENDDO
          RES=MOVEVERT(MOVEY(1),XL,XR,YNEW)
          IF (RES.NE.-999) THEN
            IF (XL.EQ.XNEW) THEN
              TGRID(XL,YNEW)=2
              TGRID(XL,Y)=0
              TGRID(XR,YNEW)=0
            ELSE
              TGRID(XL,YNEW)=0
              TGRID(XR,YNEW)=2
              TGRID(XR,Y)=0
            ENDIF
            DO L2=0,9
              DO L1=0,19
                GRID(L1,L2)=TGRID(L1,L2)
              ENDDO
            ENDDO
            X=XNEW
            Y=YNEW
          ENDIF
        ELSE IF (DIRECTION.EQ.2) THEN
          DO XM=XNEW+MOVEX(2)*2,19,MOVEX(2)
            IF (GRID(XM,YNEW).EQ.7) GOTO 999
            IF (GRID(XM,YNEW).EQ.0) THEN
              GRID(XM,YNEW)=GRID(XNEW,YNEW)
              DO XX=XNEW+MOVEX(2),XM,MOVEX(2)
                GRID(XX,YNEW)=GRID(XX,YNEW)*(-1)
              ENDDO
              GRID(XNEW,YNEW)=2
              GRID(X,Y)=0
              X=XNEW
              Y=YNEW
              GOTO 999
            ENDIF
          ENDDO
        ELSE IF (DIRECTION.EQ.3) THEN
          IF (GRID(XNEW,YNEW).EQ.1) THEN
            XL=XNEW
            XR=XNEW+1
          ELSE
            XL=XNEW-1
            XR=XNEW
          ENDIF
          DO L2=0,9
            DO L1=0,19
              TGRID(L1,L2)=GRID(L1,L2)
            ENDDO
          ENDDO
          RES=MOVEVERT(MOVEY(3),XL,XR,YNEW)
          IF (MN.EQ.310) WRITE(*,*)"+++",RES
          IF (RES.NE.-999) THEN
            IF (XL.EQ.XNEW) THEN
              TGRID(XL,YNEW)=2
              TGRID(XL,Y)=0
              TGRID(XR,YNEW)=0
            ELSE
              TGRID(XL,YNEW)=0
              TGRID(XR,YNEW)=2
              TGRID(XR,Y)=0
            ENDIF
            DO L2=0,9
              DO L1=0,19
                GRID(L1,L2)=TGRID(L1,L2)
              ENDDO
            ENDDO
            X=XNEW
            Y=YNEW
          ENDIF
        ELSE IF (DIRECTION.EQ.4) THEN
          DO XM=XNEW+MOVEX(4)*2,0,MOVEX(4)
            IF (GRID(XM,YNEW).EQ.7) GOTO 999
            IF (GRID(XM,YNEW).EQ.0) THEN
              GRID(XM,YNEW)=GRID(XNEW,YNEW)
              DO XX=XNEW+MOVEX(4),XM,MOVEX(4)
                GRID(XX,YNEW)=GRID(XX,YNEW)*(-1)
              ENDDO
              GRID(XNEW,YNEW)=2
              GRID(X,Y)=0
              X=XNEW
              Y=YNEW
              GOTO 999
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
  999 CONTINUE
      RETURN
      END
C
      SUBROUTINE PRINTGRID
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:19,0:9),INST(700)
C
      INTEGER X,Y
   10 FORMAT(20I2)
      DO Y=0,9
        WRITE(*,10)(GRID(X,Y),X=0,19)
      ENDDO
      RETURN
      END
