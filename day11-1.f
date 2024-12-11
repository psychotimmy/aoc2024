      PROGRAM DAY11P1
C
      COMMON /ADVENT/ NOW,FUTURE
      INTEGER*8 NOW(0:9999,0:1),FUTURE(0:9999,0:1)
C
      INTEGER*8 LIST(8),LEFT,RIGHT,TOTAL
      INTEGER IDX,ITER,NOWPTR,FUTPTR
      CHARACTER*40 STR
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I4,A,I17)
   40 FORMAT(5I12)
   50 FORMAT(A,I17)
C
      WRITE(*,10)"Advent of Code 2024 day 11, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day11in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Read the input line into a temporary string
      READ(10,FMT=20,ERR=100,END=100) STR
  100 CONTINUE
      CLOSE(10)
C     Convert string to integers
      CALL STR2INTARRAY(STR,8,LIST,TOTAL)
C     Copy the list into the NOW 2D array
      DO IDX=0,TOTAL-1
        NOW(IDX,0)=LIST(IDX+1)
        NOW(IDX,1)=1
      ENDDO
C     Update the pointer to the last filled space
      NOWPTR=TOTAL-1
C     Set iteration count to 1
      ITER=1
      DO WHILE (ITER.LE.25)
        FUTPTR=-1
        DO IDX=0,NOWPTR
          CALL RULES(NOW(IDX,0),LEFT,RIGHT)
C>>       WRITE(*,40)IDX,NOW(IDX,0),LEFT,RIGHT
          CALL INSERT(LEFT,NOW(IDX,1),FUTPTR)
          IF (RIGHT.NE.-1) CALL INSERT(RIGHT,NOW(IDX,1),FUTPTR)
        ENDDO
C>>     WRITE(*,*)"======================================"
C       Copy future list to now list for next iteration
        NOW=FUTURE
        NOWPTR=FUTPTR
C       Calculate the total number of stones after iteration
        TOTAL=0
        DO IDX=0,NOWPTR
C>>       WRITE(*,40)IDX,FUTPTR,NOW(IDX,0),NOW(IDX,1)
          TOTAL=TOTAL+NOW(IDX,1)
        ENDDO
        WRITE(*,30)"Stones after",ITER," iterations is",TOTAL
C       We've finished this iteration
        ITER=ITER+1
      ENDDO
      WRITE(*,10)" "
      WRITE(*,50)"Result is",TOTAL
      END
C
      SUBROUTINE INSERT(VALUE,COUNT,ENDPTR)
      INTEGER*8 VALUE,COUNT
      INTEGER ENDPTR
C
      COMMON /ADVENT/ NOW,FUTURE
      INTEGER*8 NOW(0:9999,0:1),FUTURE(0:9999,0:1)
C
      INTEGER IDX
C
      DO IDX=0,ENDPTR
        IF (FUTURE(IDX,0).EQ.VALUE) THEN
C       We've already seen this value, increment count and exit
          FUTURE(IDX,1)=FUTURE(IDX,1)+COUNT
          GOTO 999
        ENDIF
      ENDDO
C     New value, add 1 to the pointer to the end of the list
      ENDPTR=ENDPTR+1
C     and add the value to the end of the list, set count
      FUTURE(ENDPTR,0)=VALUE
      FUTURE(ENDPTR,1)=COUNT
C
  999 CONTINUE
      RETURN
      END
C
      SUBROUTINE RULES(VALUE,LEFT,RIGHT)
      INTEGER*8 VALUE,LEFT,RIGHT
      INTEGER DIGITS,NUMDIGITS
C
      IF (VALUE.EQ.0) THEN
        LEFT=1
        RIGHT=-1
        GOTO 999
      ENDIF
C
      DIGITS=NUMDIGITS(VALUE)
      IF (MOD(DIGITS,2).EQ.0) THEN
        LEFT=VALUE/(10**(DIGITS/2))
        RIGHT=MOD(VALUE,(10**(DIGITS/2)))
      ELSE
        LEFT=VALUE*2024
        RIGHT=-1
      ENDIF
C
  999 CONTINUE
      RETURN
      END
C
      INTEGER FUNCTION NUMDIGITS(N)
      INTEGER*8 N
      INTEGER*8 TEST
C
      TEST=N
      NUMDIGITS=0
      DO WHILE (TEST.GT.0)
        TEST=TEST/10 
        NUMDIGITS=NUMDIGITS+1
      ENDDO
C
      RETURN
      END
C
      SUBROUTINE STR2INTARRAY(STR,MAXL,LIST,ELEMENTS)
      CHARACTER*(*) STR
      INTEGER MAXL
      INTEGER*8 LIST(*), ELEMENTS
C
C     Separates a list of MAXL integers stored in STR into the
C     array LIST. The number of consecutive elements read from
C     the start of the list is returned in ELEMENTS
C
C     This method assumes STR does NOT start with a blank when 
C     passed to this subroutine, as blank in position 1 = end of 
C     string of values to parse OR if we're going to overflow
C     the integer array (ELEMENTS values found = MAXL)
C
C     Note that STR is destroyed by this subroutine!
C
      INTEGER IDX
C     Format assumes values in range -9,999,999 .. 99,999,999
    5 FORMAT(I8)
C     Zero the return array
      DO 10 IDX=1,MAXL
        LIST(IDX)=0
   10 CONTINUE
C
      ELEMENTS=0
  100 CONTINUE
      IDX=INDEX(STR,' ')
      IF ((IDX.EQ.1).OR.(ELEMENTS.EQ.MAXL)) GOTO 999
C     We have an integer value, put it in the next array element
      ELEMENTS=ELEMENTS+1
      READ(STR(1:IDX-1),5) LIST(ELEMENTS)
C     And truncate the front of the string for the next value
      STR(1:)=STR(IDX+1:)
      GOTO 100
  999 CONTINUE
      RETURN
      END
