MODULE MODULE_EMISS_SCALE
!
! Module which give the factors to scale trace gas emissions by
! based on the year of the simulation and EPA total emission
! data available at https://www.epa.gov/air-emissions-inventories/air-pollutant-emissions-trends-data
!
! The scaling is extremely simple, calculating a ratio of total annual
! emissions between the current year and the base year (2011 or 2005
! typically). The base year will default to the value set below unless
! a second argument is passed to the functions contained here.
!
! Although this has the extension .F, it is written to the Fortran 90
! standard. WRF's clean script deletes .f90 files, but during the build
! process, converts .F to .f90 files.
!
! TODO: add error if year out of range.

USE module_wrf_error
USE module_configure, ONLY : grid_config_rec_type

IMPLICIT NONE

PRIVATE YEAR_INDEX, a, def_base_yr, years, e_co, e_nox, e_pm10, e_pm25

INTEGER :: a
INTEGER, PARAMETER :: def_base_yr = 2011
INTEGER, PARAMETER, DIMENSION(25) :: years = (/ (a, a=1990,2014) /)
REAL*8, PARAMETER, DIMENSION(25) :: e_co = (/ 154188.0, 147128.0, 140895.0, &
 135902.0, 133558.0, 126778.0, 128858.0, 117910.0, 115380.0, 114541.0,      &
 114467.0, 106262.0, 102033.0, 99593.0, 97147.0, 88546.0, 85837.0, 83128.0, &
 79655.0, 72753.0, 73771.0, 73762.0, 71760.0, 69758.0, 67756.0 /), &
    e_nox = (/ 25527.0, 25180.0, 25261.0, 25356.0, 25350.0, 24955.0, 24787.0, &
 24705.0, 24348.0, 22845.0, 22598.0, 21549.0, 23959.0, 22651.0, 21331.0,      &
 20355.0, 19227.0, 18099.0, 16909.0, 15772.0, 14846.0, 14519.0, 13657.0,      &
 13072.0, 12412.0 /), &
    e_pm10 = (/ 27753.0, 27345.0, 27098.0, 27364.0, 28608.0, 25820.0, 22857.0,&
 22909.0, 22893.0, 23383.0, 23747.0, 23708.0, 21576.0, 21664.0, 21749.0,      &
 21302.0, 21401.0, 21501.0, 21580.0, 21199.0, 20823.0, 20723.0, 20687.0,      &
 20651.0, 20616.0 /), &
    e_pm25 = (/ 7560.0, 7320.0, 7198.0, 7149.0, 7542.0, 6929.0, 6724.0,  &
 6256.0, 6261.0, 7211.0, 7288.0, 6996.0, 5805.0, 5888.0, 5970.0, 5592.0, &
 5736.0, 5881.0, 6014.0, 5988.0, 5964.0, 6100.0, 6077.0, 6055.0, 6033.0 /), &
    e_so2 = (/ 23077.0, 22375.0, 22082.0, 21773.0, 21346.0, 18619.0, 18385.0, &
 18840.0, 18944.0, 17545.0, 16347.0, 15932.0, 15032.0, 14808.0, 14571.0,      &
 14546.0, 13123.0, 11699.0, 10324.0, 9089.0, 7732.0, 6479.0, 5193.0, 5098.0,  &
 4991.0 /), &
    e_voc = (/ 24108.0, 23577.0, 23066.0, 22730.0, 22570.0, 22042.0, 20871.0, &
 19530.0, 18781.0, 18270.0, 17512.0, 17111.0, 20289.0, 19911.0, 19514.0,      &
 17753.0, 17902.0, 18050.0, 17759.0, 17593.0, 17835.0, 18154.0, 17813.0,      &
 17471.0, 17130.0 /), &
    e_nh3 = (/ 4320.0, 4384.0, 4443.0, 4518.0, 4589.0, 4659.0, 4727.0,        &
 4817.0, 4940.0, 4857.0, 4907.0, 3689.0, 3994.0, 4005.0, 4016.0, 3932.0,      &
 4077.0, 4222.0, 4359.0, 4315.0, 4271.0, 4232.0, 4227.0, 4221.0, 4216.0 /)


CONTAINS

! --------------------------------------------- !
! Public functions ---------------------------- !
! --------------------------------------------- !

FUNCTION CO_SCALE(config_flags, year, base_year_in)

    TYPE (grid_config_rec_type), INTENT(IN)   :: config_flags
    INTEGER, INTENT(IN)           :: year
    INTEGER, INTENT(IN), OPTIONAL :: base_year_in
    INTEGER                       :: base_year, yind, byind
! Output value
    REAL*8                        :: co_scale

    IF (.NOT. config_flags%scale_nei_emis) THEN
        co_scale = 1.0
        RETURN
    ENDIF

    IF(PRESENT(base_year_in)) THEN
        base_year = base_year_in
    ELSE
        base_year = def_base_yr
    ENDIF

    yind = YEAR_INDEX(year, config_flags)
    byind = YEAR_INDEX(base_year, config_flags)

    co_scale = e_co(yind) / e_co(byind)

END FUNCTION


FUNCTION NOX_SCALE(config_flags, year, base_year_in)

    TYPE (grid_config_rec_type), INTENT(IN)   :: config_flags
    INTEGER, INTENT(IN)           :: year
    INTEGER, INTENT(IN), OPTIONAL :: base_year_in
    INTEGER                       :: base_year, yind, byind
! Output value
    REAL*8                        :: nox_scale
    
    IF (.NOT. config_flags%scale_nei_emis) THEN
        nox_scale = 1.0
        RETURN
    ENDIF

    IF(PRESENT(base_year_in)) THEN
        base_year = base_year_in
    ELSE
        base_year = def_base_yr
    ENDIF

    yind = YEAR_INDEX(year, config_flags)
    byind = YEAR_INDEX(base_year, config_flags)

    nox_scale = e_nox(yind) / e_nox(byind)

END FUNCTION


FUNCTION PM10_SCALE(config_flags, year, base_year_in)

    TYPE (grid_config_rec_type), INTENT(IN)   :: config_flags
    INTEGER, INTENT(IN)           :: year
    INTEGER, INTENT(IN), OPTIONAL :: base_year_in
    INTEGER                       :: base_year, yind, byind
! Output value
    REAL*8                        :: pm10_scale

    IF (.NOT. config_flags%scale_nei_emis) THEN
        pm10_scale = 1.0
        RETURN
    ENDIF

    IF(PRESENT(base_year_in)) THEN
        base_year = base_year_in
    ELSE
        base_year = def_base_yr
    ENDIF

    yind = YEAR_INDEX(year, config_flags)
    byind = YEAR_INDEX(base_year, config_flags)

    pm10_scale = e_pm10(yind) / e_pm10(byind)

END FUNCTION


FUNCTION PM25_SCALE(config_flags, year, base_year_in)

    TYPE (grid_config_rec_type), INTENT(IN)   :: config_flags
    INTEGER, INTENT(IN)           :: year
    INTEGER, INTENT(IN), OPTIONAL :: base_year_in
    INTEGER                       :: base_year, yind, byind
! Output value
    REAL*8                        :: pm25_scale
    
    IF (.NOT. config_flags%scale_nei_emis) THEN
        pm25_scale = 1.0
        RETURN
    ENDIF

    IF(PRESENT(base_year_in)) THEN
        base_year = base_year_in
    ELSE
        base_year = def_base_yr
    ENDIF

    yind = YEAR_INDEX(year, config_flags)
    byind = YEAR_INDEX(base_year, config_flags)

    pm25_scale = e_pm25(yind) / e_pm25(byind)

END FUNCTION


FUNCTION SO2_SCALE(config_flags, year, base_year_in)

    TYPE (grid_config_rec_type), INTENT(IN)   :: config_flags
    INTEGER, INTENT(IN)           :: year
    INTEGER, INTENT(IN), OPTIONAL :: base_year_in
    INTEGER                       :: base_year, yind, byind
! Output value
    REAL*8                        :: so2_scale

    IF (.NOT. config_flags%scale_nei_emis) THEN
        so2_scale = 1.0
        RETURN
    ENDIF

    IF(PRESENT(base_year_in)) THEN
        base_year = base_year_in
    ELSE
        base_year = def_base_yr
    ENDIF

    yind = YEAR_INDEX(year, config_flags)
    byind = YEAR_INDEX(base_year, config_flags)

    so2_scale = e_so2(yind) / e_so2(byind)

END FUNCTION

FUNCTION VOC_SCALE(config_flags, year, base_year_in)

    TYPE (grid_config_rec_type), INTENT(IN)   :: config_flags
    INTEGER, INTENT(IN)           :: year
    INTEGER, INTENT(IN), OPTIONAL :: base_year_in
    INTEGER                       :: base_year, yind, byind
! Output value
    REAL*8                        :: voc_scale

    IF (.NOT. config_flags%scale_nei_emis) THEN
        voc_scale = 1.0
        RETURN
    ENDIF

    IF(PRESENT(base_year_in)) THEN
        base_year = base_year_in
    ELSE
        base_year = def_base_yr
    ENDIF

    yind = YEAR_INDEX(year, config_flags)
    byind = YEAR_INDEX(base_year, config_flags)

    voc_scale = e_voc(yind) / e_voc(byind)

END FUNCTION


FUNCTION NH3_SCALE(config_flags, year, base_year_in)

    TYPE (grid_config_rec_type), INTENT(IN)   :: config_flags
    INTEGER, INTENT(IN)           :: year
    INTEGER, INTENT(IN), OPTIONAL :: base_year_in
    INTEGER                       :: base_year, yind, byind
! Output value
    REAL*8                        :: nh3_scale
    
    IF (.NOT. config_flags%scale_nei_emis) THEN
        nh3_scale = 1.0
        RETURN
    ENDIF

    IF(PRESENT(base_year_in)) THEN
        base_year = base_year_in
    ELSE
        base_year = def_base_yr
    ENDIF

    yind = YEAR_INDEX(year, config_flags)
    byind = YEAR_INDEX(base_year, config_flags)

    nh3_scale = e_nh3(yind) / e_nh3(byind)

END FUNCTION
! -------------------------------------------- !
! Private functions -------------------------- !
! -------------------------------------------- !

FUNCTION YEAR_INDEX( year, config_flags )
    INTEGER, INTENT(IN)     :: year
    TYPE (grid_config_rec_type), INTENT(IN)   :: config_flags
    INTEGER                 :: year_index, i

    year_index = -1

    DO i=1,size(years)
        IF(year == years(i)) THEN
            year_index = i
            exit
        ENDIF
    ENDDO

    IF( year_index .lt. 0 ) THEN
        IF ( config_flags%scale_closest_year ) THEN
            IF ( year .lt. years(1) ) THEN
                year_index = 1
            ELSE
                year_index = ubound(years, 1)
            ENDIF
        ELSE
            ! wrf_err_message is defined in module_wrf_error along with wrf_error_fatal()
            write(wrf_err_message, FMT='(A,I4,A)')  &
                'module emiss_scale: no scaling factor available for ', year, '. Set chemistry namelist opt scale_nei_emis to .false. to skip scaling emissions or scale_closest_year to .true. to use the closest available year'
            CALL wrf_debug(0, wrf_err_message)
            CALL WRF_ERROR_FATAL( wrf_err_message )
        ENDIF
    ENDIF

END FUNCTION

END MODULE
