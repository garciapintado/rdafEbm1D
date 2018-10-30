#module Ebm1DParameters
#    use Sizes
#    use Constants
#    implicit none

    # Ebm1D-specific parameters

    # Physical constants
            ep_rhowat <- NULL   # REAL :: density of pure water at 0 degC/(kg m^-3)
            ep_cp0    <- NULL   # REAL :: specific heat of liquid water at 0 degC/(J kg^-1 K^-1)
            ep_scon0  <- NULL   # REAL :: present-day solar constant/(W m^(-2)) (Hartmann 1994)

    # Orbital parameters
            ep_pyear <- NULL   # REAL :: paleo-year for computing orbital parameters (0 = 1950 AD)
            ep_eccen <- NULL   # REAL :: numerical eccentricity of Earth's orbit
            ep_perih <- NULL   # REAL :: longitude of the perihelion with respect to the 
                               #         moving vernal equinox/deg
            ep_perihp <- NULL  # REAL :: longitude of the perihelion with respect to the 
                               #         moving winter solstice/deg
            ep_obliq <- NULL   # REAL :: obliquity of Earth's axis of rotation/deg
            ep_clipr <- NULL   # REAL :: climatic precession/precessional parameter

    # AP 2011-01-04: moved to Ebm1DVariables.f90
    # REAL :: ep_lambda,      & # REAL :: true longitude with respect to the 
    #                           # REAL :: moving vernal equinox/deg
    #         ep_lambdap        # REAL :: true longitude with respect to the 
    #                           # REAL :: moving winter solstice/deg
     
    # Model parameters 
            ep_hocn  <- NULL    # REAL :: ocean mixed-layer depth/m
            ep_hcrat <- NULL   # REAL :: heat capacity ratio, ocean to land
            ep_tcrit <- NULL   # REAL :: temperature at which the surface becomes ice covered/degC

    # Linearized longwave radiation/(W m^(-2)):
            ep_alw     <- NULL    # REAL :: constant term/(W m^(-2))
            ep_blw     <- NULL    # REAL :: constant factor/(W m^(-2) K^(-1))
            ep_dqco2x2 <- NULL    # REAL :: 2xCO2 radiative forcing/(W m^(-2))
            ep_co2ref  <- NULL    # REAL :: reference atmospheric CO2 concentration/ppmv
            ep_co2ccn  <- NULL    # REAL :: actual atmospheric CO2 concentration/ppmv
               
    # (Co-) Albedo coefficients
            ep_apln0 <- NULL # REAL :: 
            ep_apln1 <- NULL # REAL ::         
            ep_apln2 <- NULL # REAL :: 
            ep_aice  <- NULL # REAL ::    

    # Diffusion coefficients
            ep_diff0 <- NULL     # REAL :: constant factor/(m^2 s^(-1)
            ep_diff2 <- NULL
            ep_diff4 <- NULL

    # Fixed fields
    # real, allocatable, dimension(:) :: ep_solin, & ! insolation/(W m^-2))
    # real, dimension(1:jmt) :: ep_solin, & ! insolation/(W m^-2))
    #                           ep_apln,  & ! planetary albedo
    #                           ep_diff_cnt ! diffusion coefficient/(m^2 s^-1)  
    ep_fland  <- rep(NA,jmt)   # REAL :: land fraction
    ep_focean <- rep(NA,jmt)   # REAL :: ocean fraction
    #                         ep_ceff      ! effective heat capacity of the atmosphere-ocean 
    #                                      ! system/(J m^-2 K^-1)

    # Internal flags
    ep_useIceAlbedoFeedback <- FALSE # LOGICAL :: true if ice-albedo feedback is to be used

    # Input file names
    #character(len=c_MAX_LEN_FILENAME) :: &
        ep_gridFileName      <- '' # ! file name for Ebm1D grid
        ep_fracLandFileName  <- '' # file name for Ebm1D land fraction
        ep_fracCloudFileName <- '' # file name for Ebm1D ocean fraction

#end module Ebm1DParameters
