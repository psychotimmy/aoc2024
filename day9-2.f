      PROGRAM DAY9P2
      INTEGER DISK(0:19998)
      INTEGER IDX,JDX,KDX,FILEID
      INTEGER USED(0:9999,0:1),FREE(0:9999,0:1)
      INTEGER FRONTID,BACKID,BLKNO
      INTEGER*8 VALFILE,TOTAL
C
   10 FORMAT(A)
   20 FORMAT(19999I1)
   30 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2024 day 9, part 2"
      WRITE(*,10)" "
      OPEN(10,FILE="day9in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Read the input line into an integer array
      READ(10,FMT=20,ERR=100,END=100) (DISK(IDX),IDX=0,19998)
  100 CONTINUE
      CLOSE(10)
C
      BLKNO=0
      DO 200 FILEID=0,9999
        IF (FILEID.EQ.0) THEN
C         File id 0 has no free blocks before it
          FREE(FILEID,0)=0
        ELSE
          FREE(FILEID,0)=DISK(FILEID*2-1)
        ENDIF
        FREE(FILEID,1)=BLKNO
        BLKNO=BLKNO+FREE(FILEID,0)
C
        USED(FILEID,0)=DISK(FILEID*2)
        USED(FILEID,1)=BLKNO
        BLKNO=BLKNO+USED(FILEID,0)
  200 CONTINUE
C
      TOTAL=0
      FRONTID=1
      BACKID=9999
C
C     We stop when we've got to FILEID 1 (can't move 0) 
C
      DO 900 IDX=BACKID,FRONTID,-1
C>>     WRITE(*,*)"++++++++++++++++++++++++++++++++++++++++++++++++++"
C>>     WRITE(*,*)"Attempting to fit file ",IDX
        DO 400 JDX=1,IDX
          IF (FREE(JDX,0).GE.USED(IDX,0)) THEN
C>>         WRITE(*,*)"File",IDX,"fits free space",JDX
            VALFILE=0
            DO 300 KDX=1,USED(IDX,0)
              VALFILE=VALFILE+IDX*FREE(JDX,1)
              FREE(JDX,0)=FREE(JDX,0)-1
              FREE(JDX,1)=FREE(JDX,1)+1
  300       CONTINUE
C>>         WRITE(*,*)"value of file",IDX,"is",VALFILE
            TOTAL=TOTAL+VALFILE
C>>         WRITE(*,*)"running checksum is",TOTAL
            GOTO 900
          ENDIF
  400   CONTINUE
C>>     WRITE(*,*)"File",IDX,"cannot move"
        VALFILE=0
        DO 500 KDX=1,USED(IDX,0)
          VALFILE=VALFILE+IDX*USED(IDX,1)
          USED(IDX,1)=USED(IDX,1)+1
  500   CONTINUE      
C>>     WRITE(*,*)"value of file",IDX,"is",VALFILE
        TOTAL=TOTAL+VALFILE
C>>     WRITE(*,*)"running checksum is",TOTAL
  900 CONTINUE
      WRITE(*,10)" "
      WRITE(*,30)"Checksum is ",TOTAL
      END
