# +++ R interface to write files for a single tophspf F90 model run +++

write2model <- function(pid   = 1,
                        dto=NULL, nto=NULL,
                        gpar= NULL,
                        CF)
{
 # INTEGER :: pid                          !unique identifier for parallel simulations (MonteCarlo analysis...)
 #                                         !each call to the function should have a distinct runpid to avoid srambbled results
 # REAL ::    dx                           !cell resolution [m]
 # INTEGER :: nrows, ncols                 !region nrows and ncols
 # REAL ::    dto                          !outer time step [s] - match rainfall & evpt input
 # REAL ::    dti                          !inner time step [s]
 # INTEGER :: nto                       !number of outer timesteps
 # CHARACTER(len=*) simspdist              !'smdis' or 'spdis' spatial resolution for model run
 # CHARACTER(len=*) simspdist              !'k01' [1D kinematic routing cascade] or 'swe' [clawpack-SWE]
 # LOGICAL :: mask                         !integer (1,0) map. Being 1 for values within watershed
 # INTEGER :: hruV                         !hydrological response units map (region to where time series are aggregated, disregarding simspdist.  0 = out of watershed; 1,...,n pixels withineach n hru
 # REAL ::    dem                          !digital elevation model [m]
 # CHARACTER/REAL :: rain                  !rain, may be an 1D array of CHARACTER variables pointing (whole path) to a stack of 2D binary maps [mm/tstep],
 #                                          or a 1D vector of length nto [mm/tstep]
 # CHARACTER/REAL :: petinp                !potential evapotranspiration, either an 1D array of CHARACTER variables pointing (whole path) to a stack of 2D binary maps,
 #                                          or a 1D vector of length nto [mm/tstep]
 # STRUCT    :: gpar                       ! parameter vector

 mainfile  <- "main.run"
 modelfile <- "ebm.run"
 dsnsim <- file.path(CF$path$data,CF$region,CF$event,CF$scn)

 oufldp  <- file.path(dsnsim, "output");   if (!file.exists(oufldp))       dir.create(oufldp)
 oufldp.ts    <- file.path(oufldp,"ts");   if (!file.exists(oufldp.ts))    dir.create(oufldp.ts)      # time series
 oufldp.maps  <- file.path(oufldp,"maps"); if (!file.exists(oufldp.maps))  dir.create(oufldp.maps)    # map stacks
 oufldp.stats <- file.path(oufldp,"stats");if (!file.exists(oufldp.stats)) dir.create(oufldp.stats)   # performance statistics

 if(pid > 99999) stop("pid should be <= 99999")
 pidfmt <- formatC(pid,width=5,flag="0",format="d")
 #ipid <- 0; outfldp.pid <- ""
 #while (ipid == 0 || file.exists(outfldp.pid)) {
 #  ipid <- ipid + 1
 #  rpid <- trunc(runif(1,min=1,max=1.0E8))
 #  pidfmt.pid <- paste(pidfmt,rpid,sep="_")
 #  outfldp.pid <- file.path(outfldp,pidfmt.pid)
 #}; dir.create(outfldp.pid); rm(ipid)
 infldp.pid    <- file.path(dsnsim,'input',pidfmt)
   dir.create(infldp.pid, showWarnings=FALSE)
 oufldp.ts.pid <- file.path(oufldp.ts,pidfmt)                         # scratch output folder to store time series (F90 code)
 if (file.exists(oufldp.ts.pid)) {
  system(paste("rm -rf",oufldp.ts.pid))}
 dir.create(oufldp.ts.pid)

 write(c(modelfile, dsnsim, pidfmt),
       file=file.path(infldp.pid,mainfile))

 #dx    <- RF$G$nsres
 #ncols <- RF$G$cols
 #nrows <- RF$G$rows

 #otimes  <- RF$y$uzs$s$data$time
 #OP4this <- otimes >  RF$timenow &
 #           otimes <= RF$timenex    # index of overpass files within the following time window simulation
 #if (sum(OP4this) > 0)
 #  otimes <- otimes[OP4this]
 #else
 #  otimes <- RF$timenex
 
 # data spatio-temporal distribution: |'lco': lumped constant|'lts': lumped timeseries|'dco': distributed constant|'dts': distributed ts|

 # -> initmod()  :: F90
   # -> initializeFixed()
     # -> readRunParameters()
     writeRunParameters(CF$rp,         dsn=infldp.pid,fname='RunParameters.nml')
     # -> ebm1DReadParameters()
     writeEbm1DParameters(gpar, xfiles=CF$p,
                         dsn=infldp.pid,fname='Ebm1DParametersIn.nml')
     # -> ebm1DInitializeGrids()
     syscmd <- paste('ln -sf',file.path(dsnsim,'input',CF$p$gridFileName),infldp.pid)
     system(syscmd)
     # -> ebm1DInitializeFixed()
       # -> orbInitializeElements()
       syscmd <- paste('ln -sf',file.path(dsnsim,'input','eccentricity.dat'),infldp.pid)
       system(syscmd)
       syscmd <- paste('ln -sf',file.path(dsnsim,'input','obliquity.dat'),infldp.pid)
       system(syscmd)
       syscmd <- paste('ln -sf',file.path(dsnsim,'input','precession.dat'),infldp.pid)
       system(syscmd)
     # -> cost1DReadParameters() :: hardcoded in .f90
   # -> readInitialConditions()
   # browser()
   if (file.exists(file.path(infldp.pid,CF$rp$initialConditionsFileName)))
     file.remove(file.path(infldp.pid,CF$rp$initialConditionsFileName))
   if (length(CF$rini[[it]]) == 1) # no randomisation
     writeInitialConditions(CF$rini[[it]][[1]], dsn=infldp.pid, fname=CF$rp$initialConditionsFileName)
   else
     writeInitialConditions(CF$rini[[it]][[pid]], dsn=infldp.pid, fname=CF$rp$initialConditionsFileName)
   # -> readReferenceData()
   syscmd <- paste('ln -sf',file.path(dsnsim,'input',CF$rp$referenceDataFileName),
                   infldp.pid)
   system(syscmd)
   # -> readObservations() - do not link as it may equal Initial conditions file and create a conflict
   system(paste('cp',file.path(dsnsim,'input',CF$rp$observationsFileName),
              file.path(infldp.pid,'observations.dat')))
 # end -> initmod() 
}
