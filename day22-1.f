      PROGRAM DAY22P1
      INTEGER L1
      INTEGER*8 SECRET,TOTAL,CALCSECRET
C
   10 FORMAT(A)
   20 FORMAT(I10)
   30 FORMAT(A,I20)
C
      WRITE(*,10)"Advent of Code 2024 day 22, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day22in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C
      TOTAL=0
  100 CONTINUE
      READ(10,FMT=20,END=999,ERR=999)SECRET
      WRITE(*,30)"1st secret number is                ",SECRET
      DO L1=1,2000
        SECRET=CALCSECRET(SECRET)
      ENDDO
      WRITE(*,30)"2000th secret number is             ",SECRET
      TOTAL=TOTAL+SECRET
      GOTO 100
  999 CONTINUE
      WRITE(*,10)" "
      WRITE(*,30)"Sum of all 2000th secret numbers is ",TOTAL
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
