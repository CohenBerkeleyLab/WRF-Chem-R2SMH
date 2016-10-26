! WRF:MODEL_LAYER:PHYSICS
!
! Lightning flash rate prediction based on cloud-top height. Implemented
! for models using convective parameterization. Assume use of sub-grid LNB.
!
! Price, C., and D. Rind (1992), A Simple Lightning Parameterization for Calculating
!   Global Lightning Distributions, J. Geophys. Res., 97(D9), 9919–9933, doi:10.1029/92JD00719.
!
! Wong, J., M. Barth, and D. Noone (2012), Evaluating a Lightning Parameterization
!   at Resolutions with Partially-Resolved Convection, GMDD, in preparation.
!
! Contact: J. Wong <johnwong@ucar.edu>
!
!**********************************************************************

 MODULE module_ltng_cpmpr92z
 CONTAINS

 SUBROUTINE ltng_cpmpr92z ( &
                          ! Frequently used prognostics
                            dx, dy, xland, ht, z, t,              &
                          ! Scheme specific prognostics
                            kLNB,                                 &
                          ! Scheme specific namelist inputs
                            cldtop_adjustment,                    &
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

 IMPLICIT NONE
!-----------------------------------------------------------------

! Frequently used prognostics
 REAL,    INTENT(IN   )    ::       dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: z, t

! Scheme specific prognostics
 INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: kLNB     ! model LNB from cu_physics

! Scheme specific namelist inputs
 REAL,    INTENT(IN   )    ::       cldtop_adjustment

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

! Variables used for debugging purposes only
 LOGICAL :: landsucc, seasucc 
 INTEGER :: temp_cold, klnb_in, temp_and_klnb, total_gridcells
!-----------------------------------------------------------------

 dA = dx*dy/1E6
 total_flashrate( ips:ipe,jps:jpe ) = 0.

! Initial state debugging messages - JLL
 write(message, *) 'ltng_cpmpr92z: max total flashrate before loop = ', maxval(total_flashrate(ips:ipe,jps:jpe))
 call wrf_debug(1, message)
 write(message, *) 'ltng_cpmpr92z: kps, kpe = ', kps, kpe
 call wrf_debug(1, message)

 landsucc = .FALSE.
 seasucc = .FALSE.
 temp_cold = 0
 klnb_in = 0
 temp_and_klnb = 0
 total_gridcells = (ipe-ips+1)*(jpe-jps+1)
 
! Compute AGL heights in km
 jloop: DO j=jps,jpe
    iloop: DO i=ips,ipe
        IF ( t(i,kLNB(i,j),j) .lt. 273.15 ) temp_cold = temp_cold + 1
        IF (kLNB(i,j) .ge. kps .and. kLNB(i,j) .le. kpe) klnb_in = klnb_in + 1

        IF ( t(i,kLNB(i,j),j) .lt. 273.15 .and. &
            kLNB(i,j) .ge. kps .and. kLNB(i,j) .le. kpe ) THEN              ! Cloud top must freeze

            temp_and_klnb = temp_and_klnb + 1

            zkm = ( z(i,kLNB(i,j),j) - ht(i,j) )/1E3 + cldtop_adjustment    ! Compute AGL height in km
            IF ( zkm .gt. 0. ) THEN                                         ! Must be above ground
              IF ( xland(i,j) .lt. 1.5 ) THEN
                total_flashrate(i,j) = 3.44E-5 * (zkm**4.9) /60.            ! Continental equation
                landsucc = .TRUE.
              ELSE
                total_flashrate(i,j) = 6.57E-6 * (zkm**4.9) /60.            ! Marine equation (Michalon 99)
                seasucc = .TRUE.
              ENDIF
            ENDIF
        ENDIF
    ENDDO iloop
 ENDDO jloop

 IF (landsucc) call wrf_debug(1, 'ltng_cpmpr92z: land success at least once')
 IF (seasucc) call wrf_debug(1, 'ltng_cpmpr92z: sea success at least once')

 write(message, *) 'ltng_cpmpr92z: grid cells cold enough: ', temp_cold, ' of ', total_gridcells
 call wrf_debug(1, message)
 write(message, *) 'ltng_cpmpr92z: grid cells kLNB in kps,kpe: ', klnb_in, ' of ', total_gridcells
 call wrf_debug(1, message)
 write(message, *) 'ltng_cpmpr92z: grid cells cold and kLNB: ', temp_and_klnb, ' of ', total_gridcells
 call wrf_debug(1, message)
 write(message, *) 'ltng_cpmpr92z: max total flashrate after loop = ', maxval(total_flashrate)
 call wrf_debug(1, message)

! Scale by areal ratio
 total_flashrate(ips:ipe,jps:jpe) = total_flashrate(ips:ipe,jps:jpe) * dA/baseArea

 END SUBROUTINE ltng_cpmpr92z

 END MODULE module_ltng_cpmpr92z
