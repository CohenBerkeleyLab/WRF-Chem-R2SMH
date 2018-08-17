! This program can be used to test that module_emiss_scale behaves correctly
! When run, it will read the namelist and use the start year as the emissions
! year. It will then call the emissions scaling functions and report the scaling
! factors calculated.
!
! Intended behavior:
!   1) If scale_nei_emis = .false., all scale factors should be 1
!   2) If scale_nei_emis = .true. and the requested year is defined in module_emiss_scale,
!      then scale factors should be calculated
!   3) If scale_nei_emis = .true. and the requested year is NOT defined, then:
!       3a) if scale_closest_year = .false., an error should be thrown
!       3b) if scale_closest_year = .true., the nearest scaling factor available should be used

PROGRAM test_emiss_scale

   ! Since it is unclear exactly what must be called to set up WRF, I'm just going to call everything
   ! that happens in convert_emiss before getting config_flags

   USE module_machine
   USE module_wrf_error
   USE module_integrate
   USE module_domain
!   USE module_domain, ONLY : domain, alloc_and_configure_domain, &
!        domain_clock_set, domain_clock_get, head_grid, program_name, domain_clockprint
!  USE module_initialize_real, ONLY : wrfu_initialize, rebalance_driver, find_my_parent
   USE module_driver_constants
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, &
        initial_config, get_config_as_buffer, set_config_as_buffer
   USE module_io_domain
   USE module_emiss_scale
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!new for bc
   USE module_bc
   USE module_get_file_names
#if (EM_CORE ==1)
   USE module_big_step_utilities_em
#endif

#ifdef DM_PARALLEL
   USE module_dm
#endif

IMPLICIT NONE

   INTERFACE
     SUBROUTINE init_domain_constants_em_ptr ( parent , nest )
       USE module_domain
       USE module_configure
       TYPE(domain), POINTER  :: parent , nest
     END SUBROUTINE init_domain_constants_em_ptr

   END INTERFACE


   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

INTEGER :: rc
TYPE(domain) , POINTER      :: keep_grid, grid_ptr, null_domain, grid, ingrid
TYPE (grid_config_rec_type) :: config_flags, config_flags_in
#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif
INTEGER :: debug_level = 0
CHARACTER (LEN=80)     :: message

#include "version_decl"

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * rnum8 )

   !  Get the NAMELIST data for input.

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "WRF-CHEM " // TRIM(release_version) // " EMISSIONS PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

!  CALL init_modules
   CALL       wrf_debug ( 100 , 'convert_emiss: calling init_modules ' )
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
#ifdef NO_LEAP_CALENDAR
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_NOLEAP, rc=rc )
#else
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )
#endif
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)


#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

   !  And here is an instance of using the information in the NAMELIST.  

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )
   
   !  Allocated and configure the mother domain.  Since we are in the nesting down
   !  mode, we know a) we got a nest, and b) we only got 1 nest.

   NULLIFY( null_domain )
   
   CALL  wrf_message ( program_name )
   write(message,FMT='(A)') ' allocate for wrfinput_d01 '
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )
   grid => head_grid

   !  Set up time initializations.

   CALL Setup_Timekeeping ( grid )

   CALL domain_clock_set( head_grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL       wrf_debug ( 100 , 'convert_gocart: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL init_wrfio
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!! End boilplate from convert_emiss.F !!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print *, 'config_flags%scale_nei_emis = ', config_flags%scale_nei_emis
   print *, 'config_flags%scale_closest_year = ', config_flags%scale_closest_year
   print *, 'year = ', config_flags%start_year

   print *, 'CO scale factor = ', co_scale(config_flags, config_flags%start_year, 2011)
   print *, 'NOx scale factor = ', nox_scale(config_flags, config_flags%start_year, 2011)
   print *, 'PM10 scale factor = ', pm10_scale(config_flags, config_flags%start_year, 2011)
   print *, 'PM2.5 scale factor = ', pm25_scale(config_flags, config_flags%start_year, 2011)
   print *, 'SO2 scale factor = ', so2_scale(config_flags, config_flags%start_year, 2011)
   print *, 'VOC scale factor = ', voc_scale(config_flags, config_flags%start_year, 2011)
   print *, 'NH3 scale factor = ', nh3_scale(config_flags, config_flags%start_year, 2011)

END PROGRAM
