      PROGRAM DAY23P1
      INTEGER SIZE,MAXC
      PARAMETER (SIZE=3400,MAXC=16)
C
      INTEGER TOTAL,LL,L1,L2,COMPUTERS(SIZE,MAXC),NC,CC
      INTEGER NLEFT,NRIGHT
      COMPLEX CONPAIRS(SIZE)
C
   10 FORMAT(A)
   30 FORMAT(A,I6)
C
      WRITE(*,10)"Advent of Code 2024 day 23, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day23in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C 
C     Read connected computer pairs from file as a list of complex
C     numbers. One computer stored as real, the other imaginary.
C
      CALL READPAIRS(10,CONPAIRS,LL)
      CLOSE(10)
C
      TOTAL=0
C
C     Store unique computers in COMPUTERS array - all are defined on
C     lhs for sample and puzzle data, so only need to scan the real part
C     of the complex numbers stored in CONPAIRS.
C
      NC=0
      DO L1=1,LL
        DO L2=1,NC
          IF (INT(REAL(CONPAIRS(L1))).EQ.COMPUTERS(L2,1)) GOTO 100
        ENDDO
        NC=NC+1
C       Index 1 stores a computer name
        COMPUTERS(NC,1)=INT(REAL(CONPAIRS(L1)))
C       Index 2 stores the number of others it is connected to
        COMPUTERS(NC,2)=0
C       Index 3 - N is the connected computer numbers
  100   CONTINUE
      ENDDO
C
C     Work out which computers are connected to each other
C
      DO L1=1,LL
        NLEFT=INT(REAL(CONPAIRS(L1)))
        NRIGHT=INT(AIMAG(CONPAIRS(L1)))
        DO L2=1,NC
          IF (COMPUTERS(L2,1).EQ.NLEFT) THEN
            COMPUTERS(L2,2)=COMPUTERS(L2,2)+1
            COMPUTERS(L2,2+COMPUTERS(L2,2))=NRIGHT
          ENDIF
          IF (COMPUTERS(L2,1).EQ.NRIGHT) THEN
            COMPUTERS(L2,2)=COMPUTERS(L2,2)+1
            COMPUTERS(L2,2+COMPUTERS(L2,2))=NLEFT
          ENDIF
        ENDDO
      ENDDO
C
C     DO L1=1,NC
C       WRITE(*,*)"Computer ",COMPUTERS(L1,1)
C       WRITE(*,*)"Has ",COMPUTERS(L1,2)," connections"
C       WRITE(*,*)(COMPUTERS(L1,L2),L2=3,2+COMPUTERS(L1,2))
C     ENDDO
C
      CALL FINDSETCOUNT(COMPUTERS,NC,3,TOTAL)
C
      WRITE(*,30)"Total is ",TOTAL
      END
C
      SUBROUTINE FINDSETCOUNT(COMP,NC,TARGET,TOTAL)
      INTEGER COMP(3400,16),NC,TARGET,TOTAL
      INTEGER L1,L2,L3,L4,T1,T2,T3,COUNT3
      INTEGER*8 RESULTS(2500),TEMP
      LOGICAL STARTST
C
      TOTAL=0
      DO L1=1,NC-2
        T1=COMP(L1,1)
        DO L2=L1+1,NC-1
          T2=COMP(L2,1)
          DO L3=L2+1,NC
            T3=COMP(L3,1)
            COUNT3=0
            DO L4=1,COMP(L1,2)
              IF (COMP(L1,2+L4).EQ.T2) THEN
                COUNT3=COUNT3+1
              ENDIF
            ENDDO
            DO L4=1,COMP(L1,2)
              IF (COMP(L1,2+L4).EQ.T3) THEN
                COUNT3=COUNT3+1
              ENDIF
            ENDDO
            DO L4=1,COMP(L2,2)
              IF (COMP(L2,2+L4).EQ.T3) THEN
                COUNT3=COUNT3+1
              ENDIF
            ENDDO
            IF (COUNT3.EQ.TARGET) THEN
              IF (STARTST(T1).OR.STARTST(T2).OR.STARTST(T3)) THEN
c               IF ((L1.LT.L2).AND.(L1.LT.L3)) THEN
c                 TEMP=L1
c                 IF (L2.LT.L3) THEN
c                   TEMP=TEMP+(L2*1000)+(L3*1000000)
c                 ELSE
c                   TEMP=TEMP+(L3*1000)+(L2*1000000)
c                 ENDIF
c               ELSE IF ((L2.LT.L1).AND.(L2.LT.L3)) THEN
c                 TEMP=L2
c                 IF (L1.LT.L3) THEN
c                   TEMP=TEMP+(L1*1000)+(L3*1000000)
c                 ELSE
c                   TEMP=TEMP+(L3*1000)+(L1*1000000)
c                 ENDIF
c               ELSE
c                 TEMP=L3
c                 IF (L1.LT.L2) THEN
c                   TEMP=TEMP+(L1*1000)+(L2*1000000)
c                 ELSE
c                   TEMP=TEMP+(L2*1000)+(L1*1000000)
c                 ENDIF
c               ENDIF
c               WRITE(*,*)TEMP
c               DO L4=1,TOTAL
C                 Already seen this triplet, don't count it again
c                 IF (RESULTS(L4).EQ.TEMP) GOTO 100
c               ENDDO
                TOTAL=TOTAL+1
c               RESULTS(TOTAL)=TEMP
c 100           CONTINUE
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE READPAIRS(FILE,LIST,LL)
      INTEGER FILE,LL,X,Y
      COMPLEX LIST(*)
C
      CHARACTER*5 PAIR
   10 FORMAT(A)
C
      LL=0
  100 CONTINUE
      READ(FILE,FMT=10,END=999,ERR=999)PAIR
      LL=LL+1
      X=(ICHAR(PAIR(1:1))-96)*100+ICHAR(PAIR(2:2))-96
      Y=(ICHAR(PAIR(4:4))-96)*100+ICHAR(PAIR(5:5))-96
      LIST(LL)=COMPLEX(X,Y)

      GOTO 100
C
  999 CONTINUE
      RETURN
      END
C
      LOGICAL FUNCTION STARTST(NUM)
      INTEGER NUM
      IF ((NUM/100).EQ.20) THEN
        STARTST=.TRUE.
      ELSE
        STARTST=.FALSE.
      ENDIF
      RETURN
      END
