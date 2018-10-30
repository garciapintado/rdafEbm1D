initializeModel <- function(CF) {
 # INITIALIZE_MODEL Initialize one-dimensional energy budget model.
 # INITIALIZE_MODEL sets the named and physical constants, run parameters,
 # model parameters and initial conditions of the one-dimensional energy
 # budget model.

 # Author:           Andre Paul
 # Written:          2011-11-30
 # Last updated:     2014-01-22
 #
 # Input arguments:
 #    gridsFilename       = file containing grid descriptors 
 #    swradiationFilename = file containing insolation and ice-free
 #                          planetary albedo
 #    diffusivityFilename = file containing meridional diffusion coefficient
 #
 # Dependencies:     initialize_grids.m

 # Set named constants
 deg2rad <- pi/180.0                   # degrees to radians conversion factor
 rSphere <- 6371.0E03                  # [m] radius of earth sphere for a spherical polar grid/m

 # Set physical constants
 pureWaterDensity        <- 1.0E03     # [kg m-3]     density of pure water at 0 degC
 pureWaterSpecificHeat   <- 4218.0     # [J kg-1 K-1] specific heat of liquid water at 0 degC
 pureWaterFreezingPoint  <- 273.15     # [K]          pure water freezing point
 solarConstant           <- 1365.0     # [W m-2]      present-day solar constant (Joussame and Taylor (1995))

 solarFraction           <-    1.0     # [-] current fraction of solar constant
 
 # Set grids
 dsn <- file.path(CF$path,CF$region,CF$event,CF$scn)
 M <- initializeGrids(dsn, CF$gridsf, CF$p$Ho)

 # Retrieve array size from structure array 'Model'
 ny = M$sizes$ny              # no. points in Y [lat] for the total domain

 # Set model parameters
 M$p$efhca <- pureWaterSpecificHeat*pureWaterDensity * CF$p$Ho # [J m-2 K-1] effective heat capacity of the atmosphere-ocean system


 #longwaveCoefficientA <- 205.0   # constant term (control)
 # longwaveCoefficientA = 201.0   # constant term (2xCO2)
 # longwaveCoefficientA = 197.0;  # constant term (4xCO2)
 #longwaveCoefficientB <-  2.23  # constant factor (efficiency of longwave radiative cooling)


                                        # Set other fields

 # Read insolation and ice-free planetary albedo from file
 dat <- read.table(file.path(dsndat,CF$region,CF$event,CF$scn,CF$swradf), skip=7,
                   col.names=c('yC','insolation','ifPA'))
 insolation             <- dat[, 2]                          # [ng]
 icefreePlanetaryAlbedo <- dat[, 3]

 # Read diffusivity from file
 dat <- read.table(file.path(dsndat,CF$region,CF$event,CF$scn,CF$diffuf), skip=6,
                   col.names=c('yG','diffKh'))
 diffKh <- dat[, 2]                                          # [ng+1]  [m s-2] diffusivity

 # Set initial conditions
 initialTracer     <- rep(pureWaterFreezingPoint + 15.0, ny)
 tracer            <- initialTracer                          # surface temperature/degC
    
 # Store parameters and variable(s) in structure array 'M'
 # Store named constants in structure array 'Model'
 M$c <- list()                                              # constants
 M$c$deg2rad <- deg2rad
 M$c$rSphere <- rSphere
 M$c$pwfp    <- pureWaterFreezingPoint
 M$c$pwde    <- pureWaterDensity
 M$c$pwsh    <- pureWaterSpecificHeat
 M$p <- list()                                              # parameters
 M$p$solarConstant          <- solarConstant
 M$p$solarFraction          <- solarFraction
 M$p$insolation             <- insolation
 M$p$icefreePlanetaryAlbedo <- icefreePlanetaryAlbedo
 M$p$lwacA   <- CF$p$lwacA
 M$p$lwacB   <- CF$p$lwacB
 M$p$efhca   <- CF$efhca
 M$p$diffKh                 <- diffKh     
 M$p$initialTracer          <- initialTracer                # initial conditions --- possibly to estimate    
 M$x <- list()                                              # structured model variables
 M$x$T                      <- tracer                       # Temperature

 return(M)

}
