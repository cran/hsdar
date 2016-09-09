!       y := alpha*A*x + beta*y
      SUBROUTINE c2dgemv (M, N, AM, X, Y)
      IMPLICIT NONE
      
      INTEGER, intent(in) :: M, N
! *     ..
! *     .. Array Arguments ..
      DOUBLE PRECISION, intent(in)  :: X(N), AM(M,N)
      DOUBLE PRECISION, intent(out) :: Y(M)
            
      DOUBLE PRECISION ALPHA, BETA
      CHARACTER(1) TRANS
      
      
      ALPHA = 1.0D+0
      BETA  = 0.0D+0
      
!       call intpr("M", 1, M, 1)
!       call intpr("N", 1, N, 1)
      
      TRANS = 'N'

      
      CALL DGEMV(TRANS,M,N,ALPHA,AM,M,X,1,BETA,Y,1)
      
      END SUBROUTINE
    
