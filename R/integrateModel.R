integrateModel <- function(M, CF) {
 # INTEGRATE_MODEL Integrate energy-budget model for one time step.
 # INTEGRATE_MODEL solves the differential equation of a one-dimensional
 # energy-budget model for one time step.


 # Author:           Andre Paul
 # Written:          2011-11-30
 # Last updated:     2014-10-30
 #
 # Input arguments:
 # - through structure array 'Model':
 #    ...
 #    tracer = tracer concentration
 #                  at old time level
 #
 # Output arguments:
 # - through structure array 'Model':
 #    ...
 #    tracer = tracer concentration
 #                  at new time level
 # Dependencies:    

 # Retrieve parameters and variable(s) from structure array 'Model'

 ny                      <- M$sizes$ny
 pureWaterFreezingPoint  <- M$c$pwfp              # pure water freezing point [ºK]
 dyC                     <- M$grids$dyC   
 yA                      <- M$grids$yA   
 volume                  <- M$grids$volume
 dT                      <- CF$dt
 solarFraction           <- CF$p$sofra
 insolation              <- M$p$insolation
 icefreePlanetaryAlbedo  <- M$p$icefreePlanetaryAlbedo
 longwaveCoefficientA    <- CF$p$lwacA
 longwaveCoefficientB    <- CF$p$lwacB
 effectiveHeatCapacity   <- M$p$efhca
 diffKh                  <- M$p$diffKh
 
 # preallocate arrays for speed
 dfy        <- rep(0.0, ny+1)  # diffusive flux in Y (i.e. meridional)
 absorbedSolarRadiation    <- rep(0.0, ny)
 outgoingLongwaveRadiation <- rep(0.0, ny)

 # Calculate diffusive flux in Y direction
 dfy[2:ny] <- -diffKh[2:ny] * diff(M$x$T) / dyC[2:ny] * yA[2:ny] # yA[2:ny] is cross-sectional areas between between cell boundaries

 # Calculate surface temperature tendency due to internal processes
 # (here [explicit] diffusion only)
 gDiffusion <- - diff(dfy) / volume  # [ng] Divergence of fluxes
 
 # Calculate net shortwave radiation budget
 absorbedSolarRadiation <- solarFraction * insolation *
                           (1.0 - icefreePlanetaryAlbedo) # [ng]
                           
                           
 # Calculate net longwave radiation budget
 outgoingLongwaveRadiation <- longwaveCoefficientA + 
         longwaveCoefficientB * (M$x$T - pureWaterFreezingPoint) # [ng]

 # Calculate surface temperature tendency due to external forcing
 gForcing <- (absorbedSolarRadiation - outgoingLongwaveRadiation) /
              effectiveHeatCapacity     

 # Calculate total surface temperature change rate
 gTracer = gDiffusion + gForcing

 # Step surface tracer forward in time 
 # (by "Euler forward" or "forward-in-time" method)
 M$x$T <- M$x$T + deltaT*gTracer;
 
 # Store variable(s) in structure array 'Model'
 M$v <- list()
 M$v$dfy                       <- dfy                           # [ng+1]
 M$v$gDiffusion                <- gDiffusion                    # [ng]
 M$v$planetaryAlbedo           <- icefreePlanetaryAlbedo
 M$v$absorbedSolarRadiation    <- absorbedSolarRadiation
 M$v$outgoingLongwaveRadiation <- outgoingLongwaveRadiation

 return(M)
}
