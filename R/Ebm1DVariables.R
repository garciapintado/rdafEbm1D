#module Ebm1DVariables
#    use Sizes
#    implicit none

#    ! EBM1D-specific variables and fields

    ev_jice_s    <- NULL          # INTEGER :: latitudinal index of ice boundary (southern hemisphere)
    ev_jice_n    <- NULL          # INTEGER :: latitudinal index of ice boundary (northern hemisphere)
    ev_ceff      <- NULL          # REAL ::    effective heat capacity of the atmosphere-ocean system/(J m^-2 K^-1)
    ev_yice_s    <- NULL          # REAL ::    latitude of ice boundary (southern hemisphere)
    ev_yice_n    <- NULL          # REAL ::    latitude of ice boundary (northern hemisphere)
                                  #            AP 2011-01-04: moved here from Ebm1DParameters.f90
    ev_lambda    <- NULL          # REAL :: true longitude with respect to the moving vernal equinox/deg
    ev_lambdap   <- NULL          # REAL :: true longitude with respect to the moving winter solstice/deg
    ev_deltaTBeg <- NULL          # REAL :: day of year at beginning of time step
    ev_deltaTEnd <- NULL          # REAL :: day of year at end       of time step
             
#    # REAL :: real, allocatable, dimension(:) :: ev_tsfc, &      # REAL :: surface temperature/degC
    ev_tsfc      <- rep(NA,jmt)    # REAL :: surface temperature/degC
    ev_netswpln  <- rep(NA,jmt)    # REAL :: net shortwave radiation absorbed byplanet / (W m^(-2)) 
    ev_netlwpln  <- rep(NA,jmt)    # REAL :: net longwave radiation  emitted by planet/ (W m^(-2))  
    ev_heddy     <- rep(NA,jmt)    # REAL :: meridional heat transport by transient eddies/W
    ev_div_heddy <- rep(NA,jmt)    # REAL :: divergence of eddy  meridional heat transport/ (W m^(-2))  

#    # REAL :: real, allocatable, dimension(:) :: ep_solin, & # REAL :: insolation/(W m^-2))
    ev_solin        <- rep(NA,jmt)  # REAL :: insolation/(W m^-2))
    ev_cosz         <- rep(NA,jmt) # REAL :: daily cosine of solar zenith angle, averaged with respect to insolation
    ev_apln_icefree <- rep(NA,jmt) # REAL :: ice-free planetary albedo
    ev_apln         <- rep(NA,jmt) # REAL :: planetary albedo
    ev_diff_cnt     <- rep(NA,jmt) # REAL :: diffusion coefficient/(m^2 s^-1)  

#end module Ebm1DVariables
