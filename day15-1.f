      PROGRAM DAY15P1
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:49,0:49),INST(20000)
C
      INTEGER I,N,X,Y
      INTEGER*8 TOTAL
C
   10 FORMAT(A)
   20 FORMAT(A,I16)
   30 FORMAT(A,I6,A,I16)
C
      WRITE(*,10)"Advent of Code 2024 day 15, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day15in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      CALL READGRID(10,X,Y)
      CALL READINST(10,N)
      CLOSE(10)
C
      CALL SUMGPS(TOTAL)
      WRITE(*,20)"Starting GPS sum is",TOTAL
C
      DO I=1,N
        CALL MOVEBOT(X,Y,INST(I))
        CALL SUMGPS(TOTAL)
        WRITE(*,30)"GPS sum after",I," moves is",TOTAL
      ENDDO
C   
      END
C
      SUBROUTINE READGRID(FILE,X0,Y0)
      INTEGER FILE,X0,Y0
C
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:49,0:49),INST(20000)
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
      DO X=1,50
        IF (ROW(X:X).EQ.'#') THEN
          GRID(X-1,Y)=7
        ELSE IF (ROW(X:X).EQ.'.') THEN
          GRID(X-1,Y)=0
        ELSE IF (ROW(X:X).EQ.'O') THEN
          GRID(X-1,Y)=1
        ELSE IF (ROW(X:X).EQ.'@') THEN
          X0=X-1
          Y0=Y
          GRID(X-1,Y)=2
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
      INTEGER GRID(0:49,0:49),INST(20000)
C
      CHARACTER*1000 ROW
      INTEGER X,Y
C
   10 FORMAT(A)
   20 FORMAT(I5,A)
C
      Y=0
   50 CONTINUE
      READ(FILE,FMT=10,END=999,ERR=999)ROW
      DO X=1,1000
        IF (ROW(X:X).EQ.'^') THEN
          INST(Y*1000+X)=1
        ELSE IF (ROW(X:X).EQ.'>') THEN
          INST(Y*1000+X)=2
        ELSE IF (ROW(X:X).EQ.'v') THEN
          INST(Y*1000+X)=3
        ELSE IF (ROW(X:X).EQ.'<') THEN
          INST(Y*1000+X)=4
        ENDIF
      ENDDO
      Y=Y+1
      GOTO 50
  999 CONTINUE
      NUM=Y*1000
      WRITE(*,20)Y," instruction rows read from file"
      WRITE(*,20)NUM," instructions read from file"
      RETURN
      END
C
      SUBROUTINE SUMGPS(GPS)
      INTEGER*8 GPS
C
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:49,0:49),INST(20000)
C
      INTEGER X,Y
C
      GPS=0
      DO Y=0,49
        DO X=0,49
          IF (GRID(X,Y).EQ.1) GPS=GPS+100*Y+X
        ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE MOVEBOT(X,Y,DIRECTION)
      INTEGER X,Y,DIRECTION
C
      COMMON /ADVENT/ GRID,INST
      INTEGER GRID(0:49,0:49),INST(20000)
C
      INTEGER MOVEX(4),MOVEY(4),XNEW,YNEW,XSPC,YSPC,YM,XM
      DATA MOVEX /0,1,0,-1/
      DATA MOVEY /-1,0,1,0/
C
      XNEW=X+MOVEX(DIRECTION)
      YNEW=Y+MOVEY(DIRECTION)
C
      IF (GRID(XNEW,YNEW).EQ.0) THEN
        GRID(X,Y)=0
        GRID(XNEW,YNEW)=2
        X=XNEW
        Y=YNEW
      ELSE IF (GRID(XNEW,YNEW).EQ.1) THEN
        IF (DIRECTION.EQ.1) THEN
          DO YM=YNEW+MOVEY(1),1,MOVEY(1)
            IF (GRID(XNEW,YM).EQ.7) GOTO 999
            IF (GRID(XNEW,YM).EQ.0) THEN
              GRID(X,Y)=0
              GRID(XNEW,YNEW)=2
              GRID(XNEW,YM)=1
              X=XNEW
              Y=YNEW
              GOTO 999
            ENDIF
          ENDDO
        ELSE IF (DIRECTION.EQ.2) THEN
          DO XM=XNEW+MOVEX(2),48,MOVEX(2)
            IF (GRID(XM,YNEW).EQ.7) GOTO 999
            IF (GRID(XM,YNEW).EQ.0) THEN
              GRID(X,Y)=0
              GRID(XNEW,YNEW)=2
              GRID(XM,YNEW)=1
              X=XNEW
              Y=YNEW
              GOTO 999
            ENDIF
          ENDDO
        ELSE IF (DIRECTION.EQ.3) THEN
          DO YM=YNEW+MOVEY(3),48,MOVEY(3)
            IF (GRID(XNEW,YM).EQ.7) GOTO 999
            IF (GRID(XNEW,YM).EQ.0) THEN
              GRID(X,Y)=0
              GRID(XNEW,YNEW)=2
              GRID(XNEW,YM)=1
              X=XNEW
              Y=YNEW
              GOTO 999
            ENDIF
          ENDDO
        ELSE IF (DIRECTION.EQ.4) THEN
          DO XM=XNEW+MOVEX(4),1,MOVEX(4)
            IF (GRID(XM,YNEW).EQ.7) GOTO 999
            IF (GRID(XM,YNEW).EQ.0) THEN
              GRID(X,Y)=0
              GRID(XNEW,YNEW)=2
              GRID(XM,YNEW)=1
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
