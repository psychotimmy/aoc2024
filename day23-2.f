      PROGRAM DAY23P2
      INTEGER SIZE,MAXC
      PARAMETER (SIZE=3400,MAXC=16)
C
      INTEGER TOTAL,LL,L1,L2,COMPUTERS(SIZE,MAXC),COMPCOUNT(520),NC
      INTEGER NLEFT,NRIGHT,LARGEST,LARGESTIDX,LANPARTY(14)
      INTEGER SHORTLIST(50,2),SLC,L3
      CHARACTER*80 LANPARTYPWD
      COMPLEX CONPAIRS(SIZE)
C
   10 FORMAT(A)
   20 FORMAT(2A)
   30 FORMAT(A,I5)
C
      WRITE(*,10)"Advent of Code 2024 day 23, part 2"
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
      WRITE(*,30)"Number of computers is: ",NC
      WRITE(*,10)" "
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
C     Code commented out below shows that every computer (520 off) has
C     13 connections to other computers. So therefore just need to work
C     through each computer and see which has the greatest number of
C     appearances in the triplets. Those computers (plus connections)
C     can be shortlisted for the password. The 13 computers that are
C     common across all sets of 14 computers found is the password!
C     Note that this is not a general algorithm for finding the answer
C     in an arbitrary network, but utilises the shape of the input data
C     to find the answer for an otherwise potentially NP complete problem.
C
C     DO L1=1,NC
C       WRITE(*,30)"Computer ",COMPUTERS(L1,1)
C       WRITE(*,40)"Has ",COMPUTERS(L1,2)," connections"
C       WRITE(*,20)(COMPUTERS(L1,L2),L2=3,2+COMPUTERS(L1,2))
C     ENDDO
C
      CALL FINDCAPPCOUNT(COMPUTERS,NC,3,COMPCOUNT)
C
      LARGEST=0
      DO L1=1,NC
        IF (COMPCOUNT(L1).GT.LARGEST) THEN
          LARGEST=COMPCOUNT(L1)
        ENDIF
      ENDDO
C     At least LARGEST computers to examine further
      WRITE(*,10)"LAN party password is the 13 computers that are the "
      WRITE(*,10)"intersection between the sets of 14 computers below."
      WRITE(*,10)" "
C     Set shorlist count to 0
      SLC=0
      DO L1=1,NC
        IF (COMPCOUNT(L1).EQ.LARGEST) THEN
          LARGESTIDX=L1
          LANPARTY(1)=COMPUTERS(LARGESTIDX,1)
          DO L2=2,14
            LANPARTY(L2)=COMPUTERS(LARGESTIDX,1+L2)
          ENDDO
          DO L2=1,14
            DO L3=1,SLC
              IF (SHORTLIST(L3,1).EQ.LANPARTY(L2)) THEN
                SHORTLIST(L3,2)=SHORTLIST(L3,2)+1
                GOTO 200
              ENDIF
            ENDDO
            SLC=SLC+1
            SHORTLIST(SLC,1)=LANPARTY(L2)
            SHORTLIST(SLC,2)=1
  200       CONTINUE
          ENDDO
          CALL INSORT(LANPARTY,14)
          DO L2=0,13
            LANPARTYPWD((L2*3)+1:(L2*3)+1)=
     +        ACHAR((LANPARTY(L2+1)/100)+96)
            LANPARTYPWD((L2*3)+2:(L2*3)+2)=
     +        ACHAR(MOD(LANPARTY(L2+1),100)+96)
            LANPARTYPWD((L2*3)+3:(L2*3)+3)=','
          ENDDO
          WRITE(*,10)LANPARTYPWD(1:(14*3)-1)
        ENDIF
      ENDDO
      WRITE(*,10)" "
C     Work out the intersection
      L2=0
      DO L1=1,SLC
        IF (SHORTLIST(L1,2).EQ.13) THEN
          L2=L2+1 
          LANPARTY(L2)=SHORTLIST(L1,1)
        ENDIF
      ENDDO
C     Sort it into alphabetical order
      CALL INSORT(LANPARTY,13)
      DO L2=0,12
        LANPARTYPWD((L2*3)+1:(L2*3)+1)=
     +    ACHAR((LANPARTY(L2+1)/100)+96)
        LANPARTYPWD((L2*3)+2:(L2*3)+2)=
     +    ACHAR(MOD(LANPARTY(L2+1),100)+96)
        LANPARTYPWD((L2*3)+3:(L2*3)+3)=','
      ENDDO
C     Password cracked!!
      WRITE(*,20)"Password is : ",LANPARTYPWD(1:(13*3)-1)
      END
C
      SUBROUTINE FINDCAPPCOUNT(COMP,NC,TARGET,COMPCOUNT)
      INTEGER COMP(3400,16),NC,TARGET,COMPCOUNT(*)
      INTEGER L1,L2,L3,L4,T1,T2,T3,COUNT3
C
C     Initialise COMPCOUNT array
      DO L1=1,520
        COMPCOUNT(L1)=0
      ENDDO
C
C     Triplet evaluator
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
C             Add one to each computer's appearance count
              COMPCOUNT(L1)=COMPCOUNT(L1)+1
              COMPCOUNT(L2)=COMPCOUNT(L2)+1
              COMPCOUNT(L3)=COMPCOUNT(L3)+1
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
      SUBROUTINE INSORT(LIST,NUM)
C     Slow but useful insertion sort
C     LIST is returned with the first NUM elements sorted
C     in ascending order
      INTEGER LIST(*),NUM
      INTEGER J,K,TEMP
      DO 20 J=2,NUM
        TEMP=LIST(J)
        K=J-1
        DO 10 WHILE ((LIST(K).GT.TEMP).AND.(K.GE.1))
          LIST(K+1)=LIST(K)
          K=K-1
   10   CONTINUE
        LIST(K+1)=TEMP
   20 CONTINUE
      RETURN
      END
