# initial uncertainty values as from paper


p <- list() # global parameter definition - [numeric scalar, numeric range, character: file name]
  p$useIceAlbedoFeedback <- TRUE    # logical :: 
  p$rhowat <-   1.0E03          # REAL :: density of pure water at 0 degC/(kg m^-3)
  p$cp0    <-    4218.0         # REAL :: specific heat of liquid water at 0 degC/(J kg^-1 K^-1)
  p$scon0   <-   1365.0         # REAL :: present-day solar constant/(W m^(-2)) (Hartmann 1994)
  p$pyear   <-      0.0         # REAL :: paleo-year for computing orbital parameters (0 = 1950 AD)
  p$hocn    <- c(  70.0,15.0)   # REAL :: ocean mixed-layer depth/m
  p$hcrat   <-     50.0         # REAL :: heat capacity ratio, ocean to land
  p$tcrit   <-    -10.0         # REAL :: temperature at which the surface becomes ice covered/degC
  p$alw     <- c( 205.0,7.0)   # REAL :: linearised outgoing longwave radiation; constant term/(W m^(-2))
  p$blw     <-      2.23        # REAL :: linearised outgoing longwave radiation; constant factor [multiplier of T]/(W m^(-2) K^(-1))
  p$dqco2x2 <-      4.0            # REAL :: 2xCO2 radiative forcing/(W m^(-2))
  p$co2ref  <-     345.0           # REAL :: reference atmospheric CO2 concentration/ppmv
  p$co2ccn  <-     345.0           # REAL :: actual atmospheric CO2 concentration/ppmv   
  p$apln0   <-       0.697         # REAL :: background albedo coefficients       
  p$apln1   <-      0.0
  p$apln2   <-     -0.175 
  p$aice    <-      0.62           # REAL :: albedo of snow and ice
  p$diff0   <- c(   1.5e+05,1.5E05)# REAL :: diffusion coefficient, constant factor/(m^2 s^(-1)
  p$diff2   <- c(  -1.33,0.75)     # REAL ::
  p$diff4   <- c(   0.67,0.6)
  p$gridFileName      <- '20x1x1_grid.dat'
  p$fracLandFileName  <- '20x1x1_land_fraction.dat'
  p$fracCloudFileName <- '20x1x1_cloud_fraction.dat'

  MC <- data.frame(flag=rep(FALSE,length(p)), dis='rnorm', min=-Inf, max=Inf, # range [min,max] just for initial generation
                   row.names=names(p), ispar=TRUE, stringsAsFactors=FALSE)

  # ispar=TRUE indicates here elements which go into Monte Carlo parameter table
  # note this differs from topHSPF_DA, where all elements
  # in MC go to the MonteCarlo (PAWpar) ensemble matrix (whith CHARACTER variables set to NA)
  MC[,'ispar'] <- sapply(p, FUN=function(x){class(x) %in% c("logical","numeric")})
  MC[c('hocn','diff0'),'dis'] <- 'truncnorm'

  # add constraints [e.g. bounds for normally distributed parameters]
  MC['useIceAlbedoFeedback',c('min','max')] <- c(0,1)
  MC['hocn','min']  <- 1.0
  MC['diff0','min'] <- 0.0

  # what is uncertain
  # "Present-day (PD) case no. 1" 
  MC[c('hocn','alw','diff0','diff2','diff4'),'flag'] <- TRUE
  CF$nc <- sum(MC[,'flag']) # 5 :: number of variables to optimise. These are fixed cases hard-coded in F90

  # matrix of constraints [one contraint per row]
  MCcons <- NULL

  CF$p      <- p
  CF$MC     <- MC
  CF$MCcons <- MCcons

  rm(p,MC,MCcons)       
  Pthetab <- Pthetaa <- NULL # [ntheta,ntheta] this should be replaced for iol > 1 in sequential fractional DA [pFKF], as  it overwritte
                             # where ntheta == sum(CF$MC$flag) 
