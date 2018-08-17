! WRF:MODEL_LAYER:PHYSICS
!
! Lightning flash rate prediction based on CAPE. Implemented
! for models using convective parameterization. 
!
! Choi, Y., Wang, Y., Zeng, T., Martin, R. V., Kurosu, T. P., & Chance, K. (2005). Evidence of lightning NOx 
! and convective transport of pollutants in satellite observations over North America. Geophysical Research Letters, 32(2).
! Implemented by Qindan Zhu 06/14/2018
!
!**********************************************************************

 MODULE module_ltng_cpmCAPE
 CONTAINS

 SUBROUTINE ltng_cpmCAPE ( &
                          ! Frequently used prognostics
                            dx, dy, xland, ht, z, t,              &
                          ! Scheme specific prognostics
                            CAPEQ,PRATEC,LnoxScale,lnox_scale_opt,  &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            ips, ipe, jps, jpe, kps, kpe,         &
                          ! Mandatory output for all quantitative schemes
                            total_flashrate                       &
                          )
!-----------------------------------------------------------------
! Framework
 USE module_state_description

! Model layer
 USE module_model_constants
 USE module_wrf_error
 USE module_configure, ONLY : grid_config_rec_type
 IMPLICIT NONE
!-----------------------------------------------------------------

! Frequently used prognostics
 REAL,                                            INTENT(IN   ) ::       dx, dy
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: z, t

! Scheme specific prognostics
 REAL, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: CAPEQ,PRATEC,LnoxScale     ! CAPE calculated from (?)
 INTEGER, INTENT(IN   )    ::       lnox_scale_opt

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Mandatory outputs for all quantitative schemes
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(  OUT) :: total_flashrate

! Local variables
 REAL :: dA              ! grid area dx*dy in km2
 REAL :: zkm             ! AGL z in km

 REAL, PARAMETER:: baseArea=1296. ! base-case area, dx = 36 km

 INTEGER :: i,k,j

 CHARACTER (LEN=250) :: message

!-----------------------------------------------------------------

 dA = dx*dy/1E6

 total_flashrate( ips:ipe,jps:jpe ) = 0.
! Compute AGL heights in km
 jloop: DO j=jps,jpe
    iloop: DO i=ips,ipe
        IF (CAPEQ(i,j) .gt. 0 .and. PRATEC(i,j) .gt. 0) THEN
            total_flashrate(i,j) = 1.8E-4 * CAPEQ(i,j) * PRATEC(i,j) 
            IF(lnox_scale_opt == lightningscale)THEN
                total_flashrate(i,j) = total_flashrate(i,j) * LnoxScale(i,j)
                write(*,*)'qindan zhu lnox_scale_opt 4'
                call  wrf_debug(100,'regional scaling factor is added to calculate lightning flashrate')
            ENDIF    
        ENDIF
    ENDDO iloop
 ENDDO jloop

! convert from flash density to flashrate
 total_flashrate(ips:ipe,jps:jpe) = total_flashrate(ips:ipe,jps:jpe) * dA

 END SUBROUTINE ltng_cpmCAPE

 END MODULE module_ltng_cpmCAPE
