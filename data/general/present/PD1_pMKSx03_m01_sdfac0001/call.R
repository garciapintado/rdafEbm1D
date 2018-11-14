#!/R
#
# R CMD BATCH --no-save --no-restore-data --no-readline call.R &
#
# PD1 Paul and Losch 2012

#startT <- Sys.time()
 cwd    <- getwd()
 HOME   <- Sys.getenv("HOME")
 PERM   <- Sys.getenv('PERM')
 #PERM   <- Sys.getenv('WORK') # HLRN
 EBM1   <- file.path(HOME,'docs','palmod','ebm1d')

 dsn    <- file.path(EBM1,'data')                 # source backed-up data
 dsndat <- file.path(PERM,'ebm1d','data')         # output
 region <- 'general'
 event  <- 'present'
 scn    <- 'PD1_pMKSx03_m01_sdfac0001'              # no errors added to samples from truth

 dsnscn <- file.path(dsn,    region, event, scn)  # source data in backed-up volume
 dsnsim <- file.path(dsndat, region, event, scn)  # simulation data in non-backed-up volume

 if (file.exists(dsnsim))                         # create IO folders
   system(paste("rm -r -f",dsnsim))
 dir.create(dsnsim, recursive=TRUE)
 dir.create(file.path(dsnsim,'input'))
 dir.create(file.path(dsnsim,'output'))

 # fixed input data to model
 system(paste("ln -sf", file.path(EBM1,'andre','Ebm1D','input','*'),
                        file.path(dsnsim,'input')))

 source(file.path(EBM1,'R','rdafEbm1D.R'))

 cat("Full completion!\n")
 #ellapseT <- difftime(Sys.time(), startT, units="hours")
 #cat("total ellapsed time ::", ellapseT,"hours\n")
