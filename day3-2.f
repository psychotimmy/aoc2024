      PROGRAM DAY3P2
      COMMON /A/ DOPROCESS
      LOGICAL DOPROCESS
C
      CHARACTER*4096 STR
      INTEGER LIST(8),IDX,TOTAL,NUMVALS,PROCESS
C
   10 FORMAT(A)
   30 FORMAT(A,I10)
      WRITE(*,10)"Advent of Code 2024 day 3, part 2"
      WRITE(*,10)" "
      OPEN(10,FILE="day3in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
C     DOPROCESS in common as value preserved over calls to PROCESS()
      DOPROCESS=.TRUE.
   50 CONTINUE
C     Read each input line, convert to an array of integers
      READ(10,FMT=10,ERR=100,END=100)STR
C     Process the line, add its value to the total
      TOTAL=TOTAL+PROCESS(STR)
      WRITE(*,30)"Running total is ",TOTAL
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      WRITE(*,10)" "
      WRITE(*,30)"Result is ",TOTAL
      END
C
      INTEGER FUNCTION PROCESS(STR)
      CHARACTER*(*) STR
C
      COMMON /A/ DOPROCESS
      LOGICAL DOPROCESS
C
      CHARACTER*7 TSTR,LSTR,RSTR
      INTEGER STARTPOS, ENDPOS, COMMAPOS, LVAL, RVAL
      INTEGER DOPOS,DONTPOS
C
      PROCESS=0
C     Find the first 'mul' followed by ')' in STR
      STARTPOS=INDEX(STR,'mul(')
C     ... and now find the do() and don't() positions
      DOPOS=INDEX(STR,"do()")
      DONTPOS=INDEX(STR,"don't()")
C     Set the values high if 0 to make logic easier
      IF (DOPOS.EQ.0) DOPOS=9999
      IF (DONTPOS.EQ.0) DONTPOS=9999
      IF (STARTPOS.EQ.0) STARTPOS=9999
C
      DO 200 WHILE ((DONTPOS.LT.STARTPOS).OR.(DOPOS.LT.STARTPOS)) 
        IF (DOPOS.NE.9999) THEN
          IF ((DOPOS.LT.DONTPOS).OR.(DONTPOS.EQ.9999)) THEN
            DOPROCESS=.TRUE.
            STR=STR(DOPOS+4:LEN(STR))
          ENDIF
        ENDIF
        IF (DONTPOS.NE.9999) THEN 
          IF ((DONTPOS.LT.DOPOS).OR.(DOPOS.EQ.9999)) THEN
            DOPROCESS=.FALSE.
            STR=STR(DONTPOS+7:LEN(STR))
          ENDIF
        ENDIF
        STARTPOS=INDEX(STR,'mul(')
        DOPOS=INDEX(STR,"do()")
        DONTPOS=INDEX(STR,"don't()")
        IF (DOPOS.EQ.0) DOPOS=9999
        IF (DONTPOS.EQ.0) DONTPOS=9999
        IF (STARTPOS.EQ.0) STARTPOS=9999
  200 CONTINUE
      IF (STARTPOS.NE.9999) THEN
        STR=STR(STARTPOS+4:LEN(STR))
        ENDPOS=INDEX(STR,')')
      ELSE
        STARTPOS=0
        ENDPOS=0
      ENDIF
C
C     If neither is zero we've not completely processed the string
C
      DO 500 WHILE ((STARTPOS.NE.0).AND.(ENDPOS.NE.0))
C       STARTPOS is always 1 as we've always trimmed the string
C       before we get to this point in the loop
        STARTPOS=1
C       Don't care about the ')', so ignore it
        ENDPOS=ENDPOS-1
C       Total length between 'mul(' and ')' not greater than 7
        IF (ENDPOS.LE.7) THEN
          TSTR(1:)=STR(1:ENDPOS)
C         Check we have a comma
          COMMAPOS=INDEX(STR,',')
          IF (COMMAPOS.NE.0) THEN
C           Split the string into two sections
            LSTR(1:)=TSTR(1:COMMAPOS-1)
            RSTR(1:)=TSTR(COMMAPOS+1:ENDPOS)
C           Convert both to numbers - 0 is returned if not legal
C           numbers between 1 and 999
            LVAL=STR2INT(LSTR,COMMAPOS-1)
            RVAL=STR2INT(RSTR,ENDPOS-COMMAPOS)
C           Add the multipled values to the return value
C           ... but only if DOPROCESS is TRUE
            IF (DOPROCESS) PROCESS=PROCESS+LVAL*RVAL
          ENDIF
        ENDIF
C       ... and get the next 'mul(' and ')' to process, flipping 
C       the DOPROCESS flag as necessary
        STARTPOS=INDEX(STR,'mul(')
C       ... and now find the do() and don't() positions
        DOPOS=INDEX(STR,"do()")
        DONTPOS=INDEX(STR,"don't()")
C       Set the values high if 0 to make logic easier
        IF (DOPOS.EQ.0) DOPOS=9999
        IF (DONTPOS.EQ.0) DONTPOS=9999
        IF (STARTPOS.EQ.2) STARTPOS=9999
C
        DO 700 WHILE ((DONTPOS.LT.STARTPOS).OR.(DOPOS.LT.STARTPOS)) 
          IF (DOPOS.NE.9999) THEN
            IF ((DOPOS.LT.DONTPOS).OR.(DONTPOS.EQ.9999)) THEN
              DOPROCESS=.TRUE.
              STR=STR(DOPOS+4:LEN(STR))
            ENDIF
          ENDIF
          IF (DONTPOS.NE.9999) THEN 
            IF ((DONTPOS.LT.DOPOS).OR.(DOPOS.EQ.9999)) THEN
              DOPROCESS=.FALSE.
              STR=STR(DONTPOS+7:LEN(STR))
            ENDIF
          ENDIF
          STARTPOS=INDEX(STR,'mul(')
          DOPOS=INDEX(STR,"do()")
          DONTPOS=INDEX(STR,"don't()")
          IF (DOPOS.EQ.0) DOPOS=9999
          IF (DONTPOS.EQ.0) DONTPOS=9999
          IF (STARTPOS.EQ.0) STARTPOS=9999
  700   CONTINUE
        IF (STARTPOS.NE.9999) THEN
          STR=STR(STARTPOS+4:LEN(STR))
          ENDPOS=INDEX(STR,')')
        ELSE
          STARTPOS=0
          ENDPOS=0
        ENDIF
  500 CONTINUE
      RETURN
      END
C    
      FUNCTION STR2INT(STR,STRLEN)
      CHARACTER*(*) STR
      INTEGER STRLEN
C
C     Return an integer value between 1 and 999 if the string passed
C     is a character representation between '1' and '999', otherwise
C     return 0
C
      INTEGER IDX,JVAL
C
      STR2INT=0
C
C     Numbers > 999 are not legal, so only process strings with
C     3 chars or fewer
C
      IF (STRLEN.LE.3) THEN
        DO 100 IDX=1,STRLEN  
          JVAL=ICHAR(STR(IDX:IDX))
          IF ((JVAL.GE.48).AND.(JVAL.LE.57)) THEN
            STR2INT=STR2INT*10+JVAL-48
          ELSE
C           Not a legal digit - exit quickly with 0
            STR2INT=0
            GOTO 999
          ENDIF
  100   CONTINUE
      ENDIF
  999 CONTINUE
      RETURN
      END
