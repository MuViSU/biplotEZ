      SUBROUTINE MIPD(X,Y,N,D)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(N), Y(N)
      D = 0.D0
      DIV = DBLE(N*(N-1)/2)
      DO 23284 I = 2,N 
      DO 23286 J = 1,I-1 
      D = D + SQRT((X(I)-X(J))**2 + (Y(I)-Y(J))**2)
23286 CONTINUE
23284 CONTINUE
      D = D/DIV
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      SUBROUTINE INDDUP(X,Y,N,RW,FRAC,DUP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL DUP(N)
      DIMENSION X(N), Y(N), RW(4)
      XTOL = FRAC*(RW(2)-RW(1))
      YTOL = FRAC*(RW(4)-RW(3))
      DUP(1) = .FALSE.
      DO 23182 I = 2,N 
      DUP(I) = .FALSE.
      DO 23184 J = 1,I-1 
      DX = ABS(X(I)-X(J))
      DY = ABS(Y(I)-Y(J))
      IF(.NOT.(DX .LT. XTOL .AND. DY .LT. YTOL))GOTO 23186
      DUP(I) = .TRUE.
      GOTO 23185
23186 CONTINUE
23184 CONTINUE
23185 CONTINUE
23182 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC	  
	  
      SUBROUTINE MATM ( A1, A2B1, B2, A, B, OUT)

C     This subroutine performs matrix multiplication.
C     This should be improved with optimised code (such as from 
C          Linpack, etc.)
      IMPLICIT NONE
      INTEGER A1, A2B1, B2
      DOUBLE PRECISION A(A1,A2B1), B(A2B1,B2), OUT(A1,B2)

C     DUMMIES

      INTEGER I, J, K

      DO 300,J=1,B2
        DO 200,I=1,A1
          OUT(I,J)=0
          DO 100,K=1,A2B1
            OUT(I,J)=OUT(I,J)+A(I,K)*B(K,J)
100       CONTINUE

200     CONTINUE

300   CONTINUE

      END 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE EXIND (Z, X, N, P, NMU, IND)

C     This subroutine returns the indices of EXact shortest distances.
C     Tested against R output -- works
      IMPLICIT NONE
      INTEGER  N, P, NMU, IND(N)
      DOUBLE PRECISION Z(NMU,P), X(N,P)

C     DATA Ind/n*0.0/

C     DUMMIES

      INTEGER I, J, K
      DOUBLE PRECISION TEMP1, RMIN

      DO 300, I=1,N
        Ind(I)=0
        RMIN = 1.0D+09
        DO 200, J=1,NMU
          TEMP1=0.0
          DO 100, K=1,P
            TEMP1=TEMP1+( X(I,K)-Z(J,K) )**2
100       CONTINUE
          IF (TEMP1 .LT. RMIN) THEN
            RMIN=TEMP1
            IND(I)=J
          END IF         
200     CONTINUE
300   CONTINUE

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DOUBLE PRECISION FUNCTION ALFUNC (BVEC, X, N, P, NMU, IND, MU, 
     + PRED, LAMBDA, Y, CONST1, CONST2, U, V, M)

C     This subroutine calculates the loss function.
C     const1 and const2 are the two scaling constants used in the 
C               loss function...
C     ... they are calculated once in R itself.
      IMPLICIT NONE
      INTEGER N, P, NMU, IND(N), U, V
      DOUBLE PRECISION LAMBDA
      DOUBLE PRECISION  X(N,P), MU(NMU), PRED(NMU), 
     + Y(N), CONST1, CONST2, M(NMU,U+V), BVEC((U+V)*P)
C     NOTE Bvec is a vector here

C     common block

C      COMMON /CSHARE/X, N, P, NMU, IND, MU, PRED, LAMBDA, Y
C    +   CONST1, CONST2, U, V, M, BVEC
C     DUMMIES

      INTEGER I, J
      DOUBLE PRECISION   Z(NMU, P), TEMP1, TEMP2

CCCCCCCCCCCC

      ALFUNC=0.0

      CALL MATM( NMU, U+V, P, M, BVEC, Z)

      CALL EXIND(Z,X,N,P,NMU,IND)
      
        DO 100, I=1,N
        PRED(I)=MU(IND(I))
100   CONTINUE

CCCCCCCCCCCC

      TEMP1=0.0      
      DO 500, I=1,N
        TEMP1=TEMP1+ ( Y(I)-PRED(I) )**2
500   CONTINUE

      ALFUNC= TEMP1/CONST1


      IF (LAMBDA .GT. 0.0) THEN
        TEMP2=0.0
        DO 700, J=1,P
          DO 600, I=2,(NMU-1)
            TEMP2=TEMP2+( Z(I-1,J)-2*Z(I,J)+Z(I+1,J) )**2
600       CONTINUE
700     CONTINUE
        ALFUNC=ALFUNC+LAMBDA*TEMP2/CONST2
      END IF       

C     Checked Fortran Loss against R. Matches.

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 

      SUBROUTINE VARSET (BVEC, AP, U, V, P, TAU)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC     This subtroutine sets Ap and Ay for subroutine L. Dirty work.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      IMPLICIT NONE
      INTEGER U, V, P

      DOUBLE PRECISION AP((U+V)*P+1,(U+V)*P), BVEC((U+V)*P), TAU

      INTEGER I,J, ANDIM
        
      ANDIM=(U+V)*P

C     The first Ap is Bvec:
      DO 100, J=1,ANDIM
        AP(1,J)=BVEC(J)
100   CONTINUE
  
C     Adding a number tau to a successive element in each successive
C     row of Ap:
      DO 300, I=2,(ANDIM+1)
        DO 200, J=1,ANDIM
          AP(I,J)=BVEC(J)
200     CONTINUE
      AP(I,I-1)=AP(I,I-1)+TAU
300   CONTINUE

      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      SUBROUTINE L (LOSS, X, N, P, NMU, IND, MU, PRED, LAMBDA, Y, 
     +CONST1, CONST2, U, V, M, BVEC, TAU, AY, TEMPVK, ITER, FTOL, 
     +LOSS1, ITER1, ERRO, CONST3, ITMAX)

      IMPLICIT NONE

C     iter1 sends back the total number of iterations in the first call 
C      to amoeba; iter sends back the total number of iterations in the
C           second call to amoeba.

      EXTERNAL ALFUNC

      DOUBLE PRECISION ALFUNC

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     This is the subroutine actually called from within R.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER N, P, NMU, IND(N), U, V, ITER, ITER1, ERRO, ITMAX
      DOUBLE PRECISION TAU, LAMBDA, CONST3
      DOUBLE PRECISION LOSS, LOSS1, X(N,P), MU(NMU), PRED(NMU), FTOL
      DOUBLE PRECISION Y(N), CONST1, CONST2, M(NMU,U+V), BVEC((U+V)*P)
	  
C     NOTE Bvec is a vector here.

C     ALFUNC IS THE NAME OF THE FUNCTION THAT CALCULATES THE LOSS.

      INTEGER ANDIM
      DOUBLE PRECISION AP((U+V)*P+1,(U+V)*P), AY((U+V)*P+1)
     
C     The Ap's are the N+1=(u+v)*p+1 points defining the simplex; 
C         it's used in amoeba.
C     The Ay's are the N+1=(u+v)*p+1 loss function evaluations for each
C        of the Ap rows; it's used in amoeba.
C     Ap1st stores the first set of Aps. Except for the first row,
C        they are reused for the second call to amoeba.
C     LOSS1 indicates the loss after the first call of amoeba.


      INTEGER I, J
      DOUBLE PRECISION TEMPVK((U+V)*P)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ERRO=0
      ANDIM=(U+V)*P

C     Setting Ap before the first call to amoeba:

      CALL VARSET (BVEC, AP, U, V, P, TAU)
 
C     Evaluating Ap to get Ay:

      DO 200, I=1,(ANDIM+1)
        DO 100, J=1,ANDIM
          TEMPVK(J)=AP(I,J)
100     CONTINUE
C     Evaluating the successive Ay's:
      AY(I)=ALFUNC(TEMPVK, X, N, P, NMU, IND, MU, PRED, LAMBDA, Y,
     +CONST1, CONST2, U, V, M)
200   CONTINUE
C   Checked the AY(I) values against those from R (before creating 
C   the VARSET function).

C     Calling amoeba for the FIRST TIME:

      CALL AMOEBA(AP, AY, ANDIM+1, ANDIM, ANDIM, FTOL, ALFUNC, ITER,
     +X, N, P, NMU, IND, MU, PRED, LAMBDA, Y, CONST1, CONST2, U, V, 
     +M, ERRO, CONST3,ITMAX) 
C     Error check
      IF (ERRO .NE. 0) GO TO 9999
C     Saving the number of iterations in the first call to amoeba:      
      ITER1=ITER

C     Saving the loss from the first call to amoeba:
      LOSS1=AY(1)

C     Setting Ap before the second call to amoeba: 
    
      DO 300, J=1,ANDIM
        BVEC(J)=AP(1,J)
300   CONTINUE

      CALL VARSET (BVEC, AP, U, V, P, TAU)

C  Evaluating Ap to get Ay:

      DO 500, I=1,(ANDIM+1)
        DO 400, J=1,ANDIM
          TEMPVK(J)=AP(I,J)
400     CONTINUE

C Evaluating the successive Ay's:
      AY(I)=ALFUNC(TEMPVK, X, N, P, NMU, IND, MU, PRED,
     +LAMBDA, Y, CONST1, CONST2, U, V, M)
500   CONTINUE

C Checked the AY(I) values against those from R 
C (before creating the VARSET function).

C Calling amoeba for the SECOND time:

      CALL AMOEBA(AP, AY, ANDIM+1, ANDIM, ANDIM, FTOL, ALFUNC, ITER,
     +X, N, P, NMU, IND, MU, PRED, LAMBDA, Y, CONST1, CONST2, U, 
     +V, M, ERRO, CONST3,ITMAX) 

C     Error check
      IF (ERRO .NE. 0) GO TO 9999

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C   RETURN VALUES:    (carefully check)

      LOSS=AY(1)
      DO 600, J=1, ANDIM
        BVEC(J)=AP(1,J)
600   CONTINUE

9999  END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE AMOEBA (PMAT, YVEK, MP, NP, NDIM, FTOL, FUNK, ITER,
     +QX, QN, QP, QNMU, QIND, QMU, QPRED, QLAMBD, QY, QCONS1, QCONS2,
     +QU, QV, QM, ERRO, TINY, ITMAX)
C second and third lines of arguments added myself.

C MP is the number of rows of PMAT.
C NP is the number of columns of PMAT.
    
      IMPLICIT NONE
      INTEGER NMAX, ITMAX, MP, NP, NDIM, ITER, ERRO
      DOUBLE PRECISION TINY, CONST3
      PARAMETER (NMAX=20)
	  
	  DOUBLE PRECISION PMAT(MP, NP), YVEK(MP), FTOL, FUNK

      EXTERNAL FUNK

      INTEGER QN, QP, QNMU, QIND(QN), QU, QV
      DOUBLE PRECISION QX(QN, QP), QMU(QNMU), QPRED(QNMU), QLAMBD, 
     +  QY(QN), QCONS1, QCONS2, QM(QNMU,QU+QV)

	  INTEGER I, IHI, ILO, INHI, J
      DOUBLE PRECISION RTOL, SUM1, SWAP, YSAVE, YTRY, PSUM(NMAX)
	  DOUBLE PRECISION AMOTRY

CCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                             
C      USES AMOTRY, FUNK,
C      Multidimensional minimization of the function funk(x) 
C      where x(1:ndim) is a vector
C      in ndim dimensions, by the downhill simplex method of Nelder 
C      and Mead. The matrix
C      pmat(1:ndim+1,1:ndim) is input. Its ndim+1 rows are 
C      ndim-dimensional vectors which are
C      the vertices of the starting simplex. Also input is the vector 
C      yvek(1:ndim+1), whose compo-
C      nents must be pre-initialized to the values of funk evaluated
C      at the ndim+1 vertices (rows)
C      of p; and ftol the fractional convergence tolerance to be 
C      achieved in the function value
C      (n.b.!). On output, p and y will have been reset to ndim+1 new 
C      points all within ftol of
C      a minimum function value, and iter gives the number of 
C      function evaluations taken.

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	  TINY=CONST3	
      ITER=0
	  ERRO=0
	  IF (NDIM .GT. NMAX) THEN
	  ERRO=5
	  GO TO 999
	  END IF
                                       
10    DO 40, J=1,NDIM
        SUM1=0.0
        DO 30, I=1,NDIM+1
          SUM1=SUM1+PMAT(I,J)
30      CONTINUE 
        PSUM(J)=SUM1
40    CONTINUE 
                                      
50    ILO=1
                                      
      IF (YVEK(1) .GT. YVEK(2)) THEN                                   
        IHI=1
        INHI=2
      ELSE
        IHI=2
        INHI=1
      END IF
                                      
      DO 60, I=1,NDIM+1 
        IF(YVEK(I) .LE. YVEK(ILO)) ILO=I
        IF(YVEK(I) .GT. YVEK(IHI)) THEN
          INHI=IHI
          IHI=I
        ELSE IF(YVEK(I) .GT. YVEK(INHI)) THEN
          IF(I .NE. IHI) INHI=I
        END IF
60    CONTINUE   

      RTOL=2.0*DABS(YVEK(IHI)-YVEK(ILO))/(DABS(YVEK(IHI))
     + +DABS(YVEK(ILO))+TINY)
	 
C Compute the fractional range from highest to lowest and return if 
C satisfactory.
C If returning, put best point and value in slot 1.
                     
      IF (RTOL .LT. FTOL) THEN
        SWAP=YVEK(1)
        YVEK(1)=YVEK(ILO)
        YVEK(ILO)=SWAP
        DO 70, J=1,NDIM
          SWAP=PMAT(1,J)
          PMAT(1,J)=PMAT(ILO,J)
          PMAT(ILO,J)=SWAP
70    CONTINUE
        RETURN
      END IF
      IF (ITER .GE. ITMAX) THEN 
	  ERRO=1
	  GO TO 999
      END IF	  
C	  'ITMAX exceeded in AMOEBA'
      ITER=ITER+2
C Begin a new iteration. First extrapolate by a factor -1 through the
C face of the simplex across from the highest point i.e. reflect simplex
C from highest point
    
      YTRY=AMOTRY(PMAT, YVEK, PSUM, MP, NP, NDIM, FUNK, IHI, -1.0D+00,
     + QX, QN, QP,QNMU, QIND, QMU, QPRED, QLAMBD, QY, QCONS1, QCONS2, 
     + QU, QV, QM)

      IF (YTRY .LE. YVEK(ILO)) THEN
      YTRY=AMOTRY(PMAT, YVEK, PSUM, MP, NP, NDIM, FUNK, IHI, 2.0D+00, 
     + QX, QN, QP,QNMU, QIND, QMU, QPRED, QLAMBD, QY, QCONS1, QCONS2, 
     + QU, QV, QM)
      ELSE IF (YTRY .GE. YVEK(INHI)) THEN
        YSAVE=YVEK(IHI)
        YTRY=AMOTRY(PMAT, YVEK, PSUM, MP, NP, NDIM,FUNK, IHI, 0.5D+00, 
     + QX, QN, QP, QNMU, QIND, QMU, QPRED, QLAMBD, QY, QCONS1, 
     + QCONS2, +  QU, QV, QM)
                                          
        IF (YTRY .GE. YSAVE) THEN                                   
          DO 90, I=1,NDIM+1
            IF(I .NE. ILO) THEN
              DO 80, J=1,NDIM
                PSUM(J)=0.5*(PMAT(I,J)+PMAT(ILO,J))
                PMAT(I,J)=PSUM(J)
80            CONTINUE 
              YVEK(I)=FUNK(PSUM, QX, QN, QP, QNMU, QIND, QMU, QPRED, 
     +                QLAMBD, QY, QCONS1, QCONS2, QU, QV, QM)
            END IF
90        CONTINUE
       
	     ITER=ITER+NDIM

         GOTO 10

        END IF
      ELSE
        ITER=ITER-1
      END IF
      GOTO 50
999   END 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DOUBLE PRECISION FUNCTION AMOTRY(PMAT, YVEK, PSUM, MP, NP, NDIM,
     + FUNK,IHI, FAC,
     + QX, QN, QP, QNMU, QIND, QMU, QPRED, QLAMBD, 
     + QY, QCONS1, QCONS2, QU, QV, QM)
	 
      IMPLICIT NONE
      INTEGER IHI, MP, NDIM, NP, NMAX
      DOUBLE PRECISION FAC, PMAT(MP,NP), PSUM(NP), YVEK(MP), FUNK
 
      PARAMETER (NMAX=20)
      EXTERNAL FUNK
C Extrapolates by a factor FAC through the face of the simplex across 
C from the highest point, tries it, and replaces the high point if 
C the new point is better
      INTEGER J, QN, QP, QNMU, QIND(QN), QU, QV
      DOUBLE PRECISION FAC1, FAC2, YTRY, PTRY(NMAX)
      DOUBLE PRECISION QLAMBD, QX(QN,QP), QMU(QNMU)
      DOUBLE PRECISION QPRED(QNMU), QY(QN), QCONS1, QCONS2  
      DOUBLE PRECISION QM(QNMU,QU+QV) 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      FAC1=(1.0-FAC)/NDIM
      FAC2=FAC1-FAC
      DO 10, J=1,NDIM
        PTRY(J)=PSUM(J)*FAC1-PMAT(IHI,J)*FAC2
10    CONTINUE 

      YTRY=FUNK(PTRY, QX, QN, QP, QNMU, QIND, QMU, QPRED, QLAMBD, QY, 
     +          QCONS1, QCONS2, QU, QV, QM)
                             
      IF (YTRY .LT. YVEK(IHI)) THEN
        YVEK(IHI)=YTRY
        DO 20, J=1,NDIM
          PSUM(J)=PSUM(J)-PMAT(IHI,J)+PTRY(J)
          PMAT(IHI,J)=PTRY(J)
20        CONTINUE
      END IF
	  AMOTRY=YTRY
      
	  END
	  

