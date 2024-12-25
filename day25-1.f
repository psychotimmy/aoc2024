      PROGRAM DAY25P1
      INTEGER TOTAL,L1,L2,L3,NL,NK,L,K,VALUE,VL,VK
      PARAMETER (L=250,K=250)
      INTEGER LOCKS(L),KEYS(K)
      CHARACTER PATTERN*10,SUGAR*10
C
   10 FORMAT(A)
   30 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2024 day 25, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day25in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C
      TOTAL=0
      NL=0
      NK=0
  100 CONTINUE
      READ(10,FMT=10,END=999,ERR=999)PATTERN
      IF (PATTERN(1:5).EQ.'.....') THEN
C       Key
        NK=NK+1
        VALUE=0
        DO L1=1,5
          READ(10,FMT=10,END=999,ERR=999)PATTERN
          DO L2=1,5
            IF (PATTERN(L2:L2).EQ.'#') THEN
              VALUE=VALUE+(10**(5-L2))
            ENDIF
          ENDDO
        ENDDO
        KEYS(NK)=VALUE
C       Skip last two lines - '#####', followed by blank
        READ(10,FMT=10,END=999,ERR=999)SUGAR
        READ(10,FMT=10,END=999,ERR=999)SUGAR
      ELSE
C       Lock
        NL=NL+1
        VALUE=0
        DO L1=1,5
          READ(10,FMT=10,END=999,ERR=999)PATTERN
          DO L2=1,5
            IF (PATTERN(L2:L2).EQ.'#') THEN
              VALUE=VALUE+(10**(5-L2))
            ENDIF
          ENDDO
        ENDDO
        LOCKS(NL)=VALUE
C       Skip last two lines - '.....', followed by blank
        READ(10,FMT=10,END=999,ERR=999)SUGAR
        READ(10,FMT=10,END=999,ERR=999)SUGAR
      ENDIF
      GOTO 100
  999 CONTINUE
      DO L1=1,NK
        DO L2=1,NL
          VK=KEYS(L1)
          VL=LOCKS(L2)
          DO L3=1,5
            IF ((MOD(VL,10)+MOD(VK,10)).GT.5) GOTO 1000
            VK=VK/10
            VL=VL/10
          ENDDO
          TOTAL=TOTAL+1
 1000     CONTINUE
        ENDDO
      ENDDO
      WRITE(*,30)"Number of possible lock/key combinations is ",TOTAL
      END
