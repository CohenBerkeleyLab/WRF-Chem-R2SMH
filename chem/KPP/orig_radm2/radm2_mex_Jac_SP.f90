!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 SUBROUTINE mexFunction( nlhs, plhs, nrhs, prhs )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!          Matlab Gateway for the Sparse Jacobian Function Jac_SP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 USE radm2_Model

      INTEGER nlhs, nrhs
      INTEGER plhs(*), prhs(*)
      INTEGER mxGetPr, mxCreateFull, mxGetM, mxgetN
      INTEGER VPtr, FPtr, RPtr, JVSPtr
      REAL(kind=dp) V(63), F(2), RCT(159)
      REAL(kind=dp) JVS(686)

! Check for the right number of input arguments
      IF ( nrhs .ne. 3 ) THEN
         CALL mexErrMsgTxt('Jac_SP requires 3 input vectors: &
     &V(63), F(2), RCT(159)')
      END IF 
! Check for the right number of output arguments
      IF ( nlhs .ne. 1 ) THEN
         CALL mexErrMsgTxt('Jac_SP requires 1 output vector: &
     &JVS(686)')
      END IF 

      plhs(1) = mxCreateDoubleMatrix(686,1,0)

      VPtr = mxGetPr(prhs(1))
      CALL mxCopyPtrToReal8(VPtr,V,63)
      
      FPtr = mxGetPr(prhs(2))
      CALL mxCopyPtrToReal8(FPtr,F,2)
      
      RPtr = mxGetPr(prhs(3))
      CALL mxCopyPtrToReal8(RPtr,RCT,159)

      JVSPtr = mxGetPr(plhs(1))

      CALL Jac_SP( V, F, RCT, JVS )

      CALL mxCopyReal8ToPtr(JVS, JVSPtr, 686)

 END SUBROUTINE mexFunction
