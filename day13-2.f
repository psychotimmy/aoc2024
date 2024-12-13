      PROGRAM DAY13P2
C
      CHARACTER*50 STR(4)
      COMPLEX POS(3)
      INTEGER*8 TOTAL,GETMINCOST
      INTEGER L1
C
   10 FORMAT(A)
   20 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2024 day 13, part 2"
      WRITE(*,10)" "
      OPEN(10,FILE="day13in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
C
   50 CONTINUE
C     Read the input into temporary strings
      DO L1=1,4
        READ(10,FMT=10,ERR=100,END=100)STR(L1)
      ENDDO
C     Convert input to complex numbers (X=real, Y=imaginary)
      DO L1=1,3
        CALL STR2COMPLEX(STR(L1),POS(L1))
      ENDDO
      TOTAL=TOTAL+GETMINCOST(POS)
      GOTO 50
C
  100 CONTINUE
      CLOSE(10)
C     Deal with last 3 lines of input as no blank line follows
      DO L1=1,3
        CALL STR2COMPLEX(STR(L1),POS(L1))
      ENDDO
      TOTAL=TOTAL+GETMINCOST(POS)
C
      WRITE(*,20)"Result is ",TOTAL
      END
C
      SUBROUTINE STR2COMPLEX(STR,CNUM)
C     Note: Subroutine destroys STR, returns CNUM
      CHARACTER*(*) STR
      COMPLEX CNUM
C
      CHARACTER*10 STRX
      INTEGER X,Y
C     Remove part of string up to value of X
C     Deal with both '=' and '+' as marker for start (!)
      IF (INDEX(STR,'=').EQ.0) THEN
        STRX=STR((INDEX(STR,'+')+1):(INDEX(STR,',')-1))
        STR=STR(INDEX(STR,',')+1:)
        STR=STR(INDEX(STR,'+')+1:)
      ELSE
        STRX=STR((INDEX(STR,'=')+1):(INDEX(STR,',')-1))
        STR=STR(INDEX(STR,',')+1:)
        STR=STR(INDEX(STR,'=')+1:)
      ENDIF  
   10 FORMAT (I8)
      READ(STRX,10)X
      READ(STR,10)Y
      CNUM=COMPLEX(X,Y)
C
      RETURN
      END
C
      INTEGER*8 FUNCTION GETMINCOST(POS)
      COMPLEX POS(*)
C
      INTEGER*8 OFFSET,X,Y
      INTEGER*8 X1,Y1,X2,Y2,XR,YR
      DOUBLE PRECISION N,M
     
C
C     f77 hates constants that aren't integer*4!
C     do this to avoid using -fno-range-check option
C
      X=10
      Y=13
      OFFSET=X**Y
C
C     Get the x and y co-ordinate from the complex numbers and
C     add OFFSET to the target co-ordinates XR,YR
C
      X1=INT(REAL(POS(1)))
      Y1=INT(AIMAG(POS(1)))
      X2=INT(REAL(POS(2)))
      Y2=INT(AIMAG(POS(2)))
      
      XR=INT(REAL(POS(3)))
      YR=INT(AIMAG(POS(3)))
      XR=XR+OFFSET
      YR=YR+OFFSET
C 
C     Need to solve simultaneous equations as there's only 1 solution
C     per combination of buttons A and B. Part 1 looping method in 
C     day13-1.f demonstrates this is true. Equations are:
C     1. N*X1+M*X2=XR 
C     2. N*Y1+M*Y2=XY
C     ... so:
C
      N=(DFLOAT(Y2*XR-X2*YR)/DFLOAT(X1*Y2-X2*Y1))
      M=(DFLOAT(X1*YR-XR*Y1)/DFLOAT(X1*Y2-X2*Y1))
C
C     Only want exact integers for N and M - check this
C
      IF ((INT8(N).EQ.N).AND.(INT8(M).EQ.M)) THEN
        GETMINCOST=3*INT8(N)+INT8(M)
      ELSE
        GETMINCOST=0
      ENDIF
C
      RETURN
      END
