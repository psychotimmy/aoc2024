      PROGRAM DAY7P1
C
      CHARACTER*50 STR
      INTEGER*8 TARGET,TOTAL
      INTEGER VALUES(16),NUMVALUES
C
   10 FORMAT(A)
   15 FORMAT(I16)
   20 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2024 day 7, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day7in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
   50 CONTINUE
C     Read each input line
      READ(10,FMT=10,ERR=100,END=100)STR
C     Extract target value from string start into an INTEGER*8
      READ(STR(1:INDEX(STR,':')-1),15) TARGET
C     Truncate the string, parse remainder into an INTEGER array
      STR=STR(INDEX(STR,':')+2:)
      CALL STR2INTARRAY(STR,VALUES,16,NUMVALUES)
C     Can we reach the target - if not, target gets set to 0
      CALL EVALTARGET(TARGET,VALUES,NUMVALUES)
C     Increment the total 
      TOTAL=TOTAL+TARGET
      WRITE(*,20)"Running total is ",TOTAL
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      WRITE(*,10)" "
      WRITE(*,20)"Result is ",TOTAL
      END
C
      SUBROUTINE STR2INTARRAY(STR,LIST,MAXL,NONZERO)
      CHARACTER*(*) STR
      INTEGER LIST(*),MAXL,NONZERO
C
C     Separates a list of MAXL integers stored in STR into the
C     array LIST. The number of consecutive non-zero elements at
C     the start of the list is returned in NONZERO
C
C     This method assumes STR does NOT start with a blank when 
C     passed to this subroutine, as blank in position 1 = end of 
C     string of values to parse OR if we're going to overflow
C     the integer array (NONZERO values found = MAXL)
C
C     Note that STR is destroyed by this subroutine!
C
      INTEGER IDX
C     Format statement assumes values in range -99,999 .. 999,999
    5 FORMAT(I6)
C     Zero the return array
      DO 10 IDX=1,MAXL
        LIST(IDX)=0
   10 CONTINUE
C
      NONZERO=0
  100 CONTINUE
      IDX=INDEX(STR,' ')
      IF ((IDX.EQ.1).OR.(NONZERO.EQ.MAXL)) GOTO 999
C     We have a non-zero value, put it in the next array element
      NONZERO=NONZERO+1
      READ(STR(1:IDX-1),5) LIST(NONZERO)
C     And truncate the front of the string for the next value
      STR(1:)=STR(IDX+1:)
      GOTO 100
  999 CONTINUE
      RETURN
      END
C
      SUBROUTINE EVALTARGET(TARGET,VALUES,NUMVALUES)
      INTEGER*8 TARGET,SUM,CONFIGS
      INTEGER VALUES(*),NUMVALUES
C
      INTEGER OPS,IDX,JDX
      LOGICAL HIT
C     Target not yet found
      HIT=.FALSE.
C     The number of possible operator configurations
      CONFIGS=2**(NUMVALUES-1)
C     Work through each configuration until we find one that
C     works, or we run out of configurations to test. Treat OPS
C     as binary, 0 = add, 1 = mul
      DO 200 IDX=1,CONFIGS
        SUM=VALUES(1)
        OPS=IDX
        DO 100 JDX=1,NUMVALUES-1
          IF (MOD(OPS,2).EQ.0) THEN 
            SUM=SUM+VALUES(JDX+1)
          ELSE
            SUM=SUM*VALUES(JDX+1)
          ENDIF
          OPS=OPS/2
C         Can abandon the search if we exceed target
          IF (SUM.GT.TARGET) GOTO 200
  100   CONTINUE
C       Test this config
        IF (SUM.EQ.TARGET) THEN
C         Time to make a quick exit!
          HIT=.TRUE.
          GOTO 999
        ENDIF
  200 CONTINUE
C
  999 CONTINUE
C     Set TARGET to 0 if we couldn't use * and + to find it
      IF (.NOT.HIT) TARGET=0
      RETURN
      END
