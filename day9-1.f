      PROGRAM DAY9P1
      INTEGER DISK(0:19998)
      INTEGER IDX,FILEID
      INTEGER USED(0:9999),FREE(0:9999)
      INTEGER FRONTID,BACKID,COUNTBL
      INTEGER*8 TOTAL
C
   10 FORMAT(A)
   20 FORMAT(19999I1)
   30 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2024 day 9, part 1"
      WRITE(*,10)" "
      OPEN(10,FILE="day9in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Read the input line into an integer array
      READ(10,FMT=20,ERR=100,END=100) (DISK(IDX),IDX=0,19998)
  100 CONTINUE
      CLOSE(10)
C
C     Set up array indexed by fileid storing used blocks for each id
C     and an array with free blocks BEFORE each file
C
      DO 200 FILEID=0,9999
        USED(FILEID)=DISK(FILEID*2)
C       File id 0 has no free blocks before it
        IF (FILEID.NE.0) THEN
          FREE(FILEID)=DISK(FILEID*2-1)
        ELSE
          FREE(FILEID)=0
        ENDIF
  200 CONTINUE
C
C     Shuffle file id from the end to the beginning one block at
C     a time. Start block count at wherever file id 0 ends
C
      TOTAL=0
      COUNTBL=USED(0)-1
      FRONTID=1
      BACKID=9999
C
C     We stop when BACKID.GT.FRONTID
C
      DO 500 WHILE (FRONTID.LE.BACKID)
C       Work out how many file blocks we can move from the back
C       to the free space before current FRONTID.
C
        DO 300 WHILE (FREE(FRONTID).GT.0) 
          IF (USED(BACKID).GT.0) THEN
            USED(BACKID)=USED(BACKID)-1
            COUNTBL=COUNTBL+1
            TOTAL=TOTAL+COUNTBL*BACKID
C>>         WRITE(*,*)"FREE",FRONTID,BACKID,COUNTBL,TOTAL
            FREE(FRONTID)=FREE(FRONTID)-1
          ELSE
            BACKID=BACKID-1
          ENDIF
  300   CONTINUE
C
C       Free space is now full.
C       Add each block used by the front file id to COUNTBL,
C       multiply COUNTBL*FRONTID and add to TOTAL
C
        DO 400 IDX=1,USED(FRONTID)
          COUNTBL=COUNTBL+1
          TOTAL=TOTAL+COUNTBL*FRONTID
C>>       WRITE(*,*)"FULL",FRONTID,BACKID,COUNTBL,TOTAL
  400   CONTINUE
C
C       Increment FRONTID and start filling free space again
C
        FRONTID=FRONTID+1
  500 CONTINUE
C 
      WRITE(*,10)" "
      WRITE(*,30)"Checksum is ",TOTAL
      END
