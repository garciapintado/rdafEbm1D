# general 1D [zonally averaged] domain
# general climate forecast parameters for ebm1d model

CF <- list()
CF$path <- list()
CF$path$data <- dsndat
CF$region    <- region
CF$event     <- event
CF$scn       <- scn

CF$path$hpcscript <- file.path(dsndat, region, event, scn, 'input','cluster_palmod.sh')
CF$modexe    <- 'runEbm1D'
CF$staTStr   <- "1900-01-01 00:00:00 GMT"   #
CF$endTStr   <- "2000-01-01 00:00:00 GMT"   #

CF$m         <- 100                         # ensemble size
CF$npc       <-   1                         # number of processors [coworkers] for HPC forecast
CF$hpc       <- 'geo'                       # %in% ['c2a','cca','essc','mac']
CF$mpi       <- FALSE                       # mpi for R-DA

CF$mcmode         <- 1                # 1 for MonteCarlo simulation | 0 for parameter inheritance
CF$mo             <- CF$m             # for pre-calibrated models (mcmode==0) this has to NULL
CF$inh            <- list()           # non-empty list for mcmode == 0
CF$inh$prior      <- file.path(PERM,'ebm1d/data/general/present/...') # previous simulation just used if CF$mcmode = 0
CF$inh$stat       <- 'E'
CF$inh$decreasing <- TRUE

# INPUT to &run_parameters namelist -> model
rp <- list()
rp$startTime <- 0.0                             # [yr]
rp$endTime   <- rp$startTime+100                # [yr] 100 yr simulation up to present day
rp$deltaT    <- 1*12*3600                       # [s]  model timestep
rp$initialConditionsFileName <- 'modern_climatology.dat'               # input to R  -> *_????.dat to F90 
rp$restartDataFileName       <- 'restart.dat'                          # input to R  -> forward unmodified to F90
rp$referenceDataFileName     <- 'zeros.dat'
rp$observationsFileName      <- 'modern_climatology.dat'
CF$rp <- rp; rm(rp)

# map time into POSIX second times for DA analyses
CF$staT <- as.POSIXct(CF$staTStr, origin=originTimeStampTZ, tz='GMT')
CF$endT <- as.POSIXct(CF$endTStr, origin=originTimeStampTZ, tz='GMT')  
#CF$staT <- CF$rp$startTime * yr # [s]
#CF$endT <- CF$rp$endTime   * yr # [s]

## imStr <- '0001'
## writeRunParameters(CF$rp, file.path(dsnsim,'input',imStr), 'RunParameters.nml')

## grid parameters
gp <- list()
gp$jmt    <- 20             # number of lat grid points (calculated are [2,jmt-1]. End points are boundaries)
gp$dxtdeg <- 360.0
gp$dxudeg <- 360.0
gp$dytdeg <- rep(10.0, gp$jmt)        # REAL, DIMENSION(jmt)
gp$dyudeg <- rep(10.0, gp$jmt)        # REAL, DIMENSION(jmt)
gp$yt     <- seq(-95,95, by=10)       # REAL, DIMENSION(jmt)
gp$yu     <- seq(-90,100,by=10)       # REAL, DIMENSION(jmt)
CF$grp <- gp; rm(gp)

CF$omaskf  <- 'omask.tif'                  # land-sea mask [1 for ocean]. Geometadata is obtained from this map. path relative to regionp
CF$rSphere <- 6378137                      # [m] assumed earth radius for filter localisation
                                        #RF$demf    <- 'demfilled_patch.tif'        # enhanced-connectivity   DEM [m]
#RF$hrusf   <- 'hrus.tif'                   # HRU class map [0=out of domain]
#RF$dproj   <- CRS('+init=epsg:27700')      # region projection. 27700 :: British National Grid, Ordnance Survery 1936

CF$nLongTermPeriods <- 3 # F90 hardcoded (Cost1DParameters.f90)  <-> ncol(y[...))
CF$cp_iAnn          <- 3 # annual mean 

y <- list() # empty list for no observations

s_gen <- list()                     # p-type observation: generic structure
s_gen$fname <- ''                   # If empty string [''], it must be set in readOb.R
s_gen$nto   <- 1                    # maximum number of assimilates times/observation yTYPE
s_gen$qc    <- 'none'               # QC method
s_gen$r     <- 1.0                  # assumed observation variance
s_gen$twin  <- Inf                  # [s] maximum assimilation window from present to past [Inf: all integration run]
s_gen$use   <- TRUE                 # assimilate or not
s_gen$w     <- TRUE                 # TRUE for weigthed observations - via w-scaling R

yKIND <- yTYPE <- 'tsfc_feb'        # [ºC] sea air Temperature - simple case observation type just equal to generic variable KIND
y[[yKIND]][[yTYPE]]$s <- s_gen      # [ºC] monthly data of sea air Temperature - February
yKIND <- yTYPE <- 'tsfc_aug'        # [ºC] sea air Temperature - simple case observation type just equal to generic variable KIND
y[[yKIND]][[yTYPE]]$s <- s_gen      # [ºC] monthly data of sea air Temperature - August

CF$y <- y; rm(s_gen, y)

anagen <- list() # generic analysis item
anagen$u     <- TRUE                    # updated by the analysis?
anagen$ll    <- 0.0                     # global filtering
anagen$infac <- 1.0                     # no inflation
anagen$min   <- NA                      # no lower bound
anagen$pos   <- c(0,0)                  # [0,0] => no localisation
anagen$trf   <- 'none'                  # no transformation

ana <- list()

 ana$i_tsfc          <- anagen          # initial conditions: surface temperature degC
 ana$i_tsfc$u        <- FALSE           # positions later updated
 ana$i_tsfc$ll       <- 4000*km

 ana$tsfc_feb        <- anagen          # surface temperature degC 
 ana$tsfc_feb$ll     <- 4000*km         # positions later updated

 ana$tsfc_aug        <- anagen          # surface temperature degC 
 ana$tsfc_aug$ll     <- 4000*km         # positions later updated

 # model parameters
 ana$hocn            <- anagen          # [m] ocean mixed-layer depth
 ana$hocn$min        <- 0.0

 ana$alw   <- anagen                    # [W m^(-2)] constant term in linearized longwave radiation 

 ana$diff0 <- anagen                    # [m^2 s^(-1)] diffusion coefficients: constant factor
 ana$diff0$trf <- 'nlog'                # natural logarithm

 ana$diff2 <- anagen                    # [-]

 ana$diff4 <- anagen                    # [-]

CF$ana <- ana; rm(ana)
