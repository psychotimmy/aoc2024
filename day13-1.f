      PROGRAM DAY13P1
C
      CHARACTER*50 STR(4)
      COMPLEX POS(3)
      INTEGER TOTAL,L1,GETMINCOST
C
   10 FORMAT(A)
   20 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2024 day 13, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day13in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
C
   50 CONTINUE
C     Read the input into temporary strings
      DO L1=1,4
        READ(10,FMT=10,ERR=100,END=100)STR(L1)
      ENDDO
C     Convert input to complex numbers (X=real, Y=imaginary)
      DO L1=1,3
        CALL STR2COMPLEX(STR(L1),POS(L1))
      ENDDO
      TOTAL=TOTAL+GETMINCOST(POS)
      GOTO 50
C
  100 CONTINUE
      CLOSE(10)
C     Deal with last 3 lines of input as no blank line follows
      DO L1=1,3
        CALL STR2COMPLEX(STR(L1),POS(L1))
      ENDDO
      TOTAL=TOTAL+GETMINCOST(POS)
C
      WRITE(*,20)"Result is ",TOTAL
      END
C
      SUBROUTINE STR2COMPLEX(STR,CNUM)
C     Note: Subroutine destroys STR, returns CNUM
      CHARACTER*(*) STR
      COMPLEX CNUM
C
      CHARACTER*10 STRX
      INTEGER X,Y
C     Remove part of string up to value of X
C     Deal with both '=' and '+' as marker for start (!)
      IF (INDEX(STR,'=').EQ.0) THEN
        STRX=STR((INDEX(STR,'+')+1):(INDEX(STR,',')-1))
        STR=STR(INDEX(STR,',')+1:)
        STR=STR(INDEX(STR,'+')+1:)
      ELSE
        STRX=STR((INDEX(STR,'=')+1):(INDEX(STR,',')-1))
        STR=STR(INDEX(STR,',')+1:)
        STR=STR(INDEX(STR,'=')+1:)
      ENDIF  
   10 FORMAT (I8)
      READ(STRX,10)X
      READ(STR,10)Y
      CNUM=COMPLEX(X,Y)
C
      RETURN
      END
C
      INTEGER FUNCTION GETMINCOST(POS)
      COMPLEX POS(*)
C
      INTEGER L1,L2,TCOST
      TCOST=0
      DO L1=0,100
        DO L2=0,100
          IF ((POS(1)*L1+POS(2)*L2).EQ.POS(3)) THEN
            IF (TCOST.NE.0) THEN
              IF ((3*L1+L2).LT.TCOST) TCOST=3*L1+L2
            ELSE
              TCOST=3*L1+L2
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
      GETMINCOST=TCOST
      RETURN
      END
