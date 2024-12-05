      PROGRAM DAY5P1
      COMMON /ADVENT/ PAGE,PAGEORDER
      INTEGER PAGE(99),PAGEORDER(99,24)
C     Largest page order instruction is 24 integers long
      INTEGER MANUAL(24)
C     Largest manual has 23 pages
      CHARACTER*140 STR
      INTEGER MIDVAL,L1,L2,TOTAL
C
   10 FORMAT(A)
   20 FORMAT(I2)
   25 FORMAT(24I3)
   30 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2024 day 5, part 1"
      WRITE(*,10)" "
C
C     Zero the page array - stores number of ordering rules per page
C
      DO 40 L1=1,99
        PAGE(L1)=0
   40 CONTINUE
C
      OPEN(10,FILE="day5in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
   50 CONTINUE
C     Read each input line contating a page ordering rule
      READ(10,FMT=10,ERR=100,END=100)STR
C     Are we at the end of the page ordering rules?
      IF (STR(1:1).EQ.' ')GOTO 100
C     Store it into the correct place in the PAGEORDER array
C     and increment the page order count for this page
      READ(STR(1:2),20) L1
      PAGE(L1)=PAGE(L1)+1
C     Can do this as all input is of format xx|yy
      READ(STR(4:5),20) L2
      PAGEORDER(L1,PAGE(L1))=L2
      GOTO 50
  100 CONTINUE
C     Read in each manual's contents
      READ(10,FMT=25,ERR=200,END=200) MANUAL
C     Check it obeys page ordering rules, return the middle
C     value if it does, zero otherwise
      TOTAL=TOTAL+MIDVAL(MANUAL)
      WRITE(*,30)"Running total is ",TOTAL
      GOTO 100
  200 CONTINUE
      CLOSE(10)
      WRITE(*,10)" "
      WRITE(*,30)"Result is ",TOTAL
      END
C
      INTEGER FUNCTION MIDVAL(MANUAL)
      INTEGER MANUAL(*)
      INTEGER NUMPAGES
      LOGICAL CHECK
C
C     Work out how many pages are in the manual and what the middle
C     page number is
      NUMPAGES=0
      DO 100 WHILE (MANUAL(NUMPAGES).NE.0)
        NUMPAGES=NUMPAGES+1
  100 CONTINUE
      MIDVAL=MANUAL((NUMPAGES+1)/2)
C
C     For each page, check it obeys the ordering rules, if not
C     we return 0 rather than the calulated middle page number
      IF (.NOT. CHECK(MANUAL,NUMPAGES)) MIDVAL=0
      RETURN
      END
C
      LOGICAL FUNCTION CHECK(MANUAL,NUMPAGES)
      INTEGER MANUAL(*),NUMPAGES
C
      COMMON /ADVENT/ PAGE,PAGEORDER
      INTEGER PAGE(99),PAGEORDER(99,24)
C
      INTEGER MANPAGE,PREVPAGE,X,Y,L1
C
      CHECK=.TRUE.
      DO 500 MANPAGE=2,NUMPAGES
        Y=MANUAL(MANPAGE)
        DO 400 PREVPAGE=1,MANPAGE-1
          X=MANUAL(PREVPAGE)
C         The next loop only executes if there are pages to check
          DO 300 L1=1,PAGE(Y)
            IF (PAGEORDER(Y,L1).EQ.X) THEN 
C             Found a match - stop checking as a page order rule
C             has failed
              CHECK=.FALSE.
              GOTO 999
            ENDIF
  300     CONTINUE
  400   CONTINUE
  500 CONTINUE
  999 CONTINUE
      RETURN
      END
