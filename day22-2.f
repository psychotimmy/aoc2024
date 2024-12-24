      PROGRAM DAY22P2
      INTEGER L1,L2,L3,L4,MONKEYNUM,BANANAS,S1,S2,S3,S4
      INTEGER*8 SECRET,TOTAL,CALCSECRET
      INTEGER SEQ(0:2000,3270), MONKEYSUM(1635)
C
   10 FORMAT(A)
   20 FORMAT(I10)
   30 FORMAT(A,I20)
C
      WRITE(*,10)"Advent of Code 2024 day 22, part 2"
      WRITE(*,10)" "
      OPEN(10,FILE="day22in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C
      TOTAL=0
C     MONKEYNUM counts 1,3,5 for the 2000 secret numbers generated from
C     the 1635 input numbers. 2,4,6 etc. store the differences for that
C     MONKEYNUM.
      MONKEYNUM=1
  100 CONTINUE
      READ(10,FMT=20,END=999,ERR=999)SECRET
C     Store last digit of first secret number starter
      SEQ(0,MONKEYNUM)=INT(MOD(SECRET,10))
      SEQ(0,MONKEYNUM+1)=99
      DO L1=1,2000
        SECRET=CALCSECRET(SECRET)
        SEQ(L1,MONKEYNUM)=INT(MOD(SECRET,10))
        SEQ(L1,MONKEYNUM+1)=SEQ(L1,MONKEYNUM)-SEQ(L1-1,MONKEYNUM)
      ENDDO
      MONKEYNUM=MONKEYNUM+2
      GOTO 100
  999 CONTINUE
C     We now have the SEQ 2D array fully populated. How many bananas
C     can we buy?
      DO L1=1,3270,2
        WRITE(*,30)"Looking at sequences for Monkey ",(L1/2)+1
        DO L2=1,1997
          S1=SEQ(L2,L1+1)
          S2=SEQ(L2+1,L1+1)
          S3=SEQ(L2+2,L1+1)
          S4=SEQ(L2+3,L1+1)
          DO L3=1,1635
            MONKEYSUM(L3)=0
          ENDDO
          DO L3=1,3270,2
            DO L4=1,1997
              IF ((S1.EQ.SEQ(L4,L3+1)).AND.
     +            (S2.EQ.SEQ(L4+1,L3+1)).AND.
     +            (S3.EQ.SEQ(L4+2,L3+1)).AND.
     +            (S4.EQ.SEQ(L4+3,L3+1))) THEN
                MONKEYSUM((L3/2)+1)=SEQ(L4+3,L3)
                GOTO 200
              ENDIF
            ENDDO
  200       CONTINUE
          ENDDO
          BANANAS=0
          DO L3=1,1635
            BANANAS=BANANAS+MONKEYSUM(L3)
          ENDDO
          IF (BANANAS.GT.TOTAL) THEN
            TOTAL=BANANAS
            WRITE(*,30)"New banana maximum!",TOTAL
          ENDIF
        ENDDO
      ENDDO
      END
c
      INTEGER*8 FUNCTION CALCSECRET(NUM)
      INTEGER*8 NUM,N,MIX,PRUNE
C
C     Multiply NUM8 by 64
      N=NUM*64
C     Bitwise XOR of N and NUM
      MIX=XOR(N,NUM)
C     Prune MIX
      PRUNE=MOD(MIX,16777216)
      NUM=PRUNE
C     Divide NUM 8 by 32
      N=NUM/32
C     Bitwise XOR of N and NUM
      MIX=XOR(N,NUM)
C     Prune MIX
      PRUNE=MOD(MIX,16777216)
      NUM=PRUNE
C     Multiply NUM by 2048
      N=NUM*2048
C     Bitwise XOR of N and NUM
      MIX=XOR(N,NUM)
C     Prune MIX
      PRUNE=MOD(MIX,16777216)
      NUM=PRUNE
C
      CALCSECRET=NUM
      RETURN
      END
