# EBM1D One-dimensional energy-budget model.
# Ebm1D is a one-dimensional energy-budget model that calculates the
# annual-mean surface temperature as a function of latitude and time. This
# driver first initializes the model and then runs it over a specified
# number of time steps.

# Author:           Javier García-Pintado
# Institution:      University of Bremen - MARUM, GEOMOD
# Written:          2016-04-01
# Last updated:     2018-05-15
#

require(tseries)
library(MASS)
library(colorRamps)
library(Matrix)
library(rDAF)

Sys.setenv(TZ='GMT')
Sys.setlocale('LC_ALL','C')
set.seed(1)

options(digits=15)
options(warn=1)

if (!exists('ilR')) # restart loop
  ilR <- 1

# ============================================================

 USER   <- system('whoami', intern=TRUE)
 HOME   <- Sys.getenv('HOME')
 dsnR   <- file.path(HOME,'docs','ebm1d','R')
 dsnDAR <- dsnR

 km <- 1000                    # [m]
 yr <- 365*24*3600             # [s]
 originTimeStampTZ <- '1970-01-01 00:00:00 GMT'

 source(file.path(dsnR,'Constants.R'))                        # global environment
 source(file.path(dsnR,'ebm1DInitializeGrids.R'))             # function :: 
 source(file.path(dsnR,'formatTable.R'))                      # function
 source(file.path(dsnR,'getCost.R'))                          # function
 source(file.path(dsnR,'getCostY.R'))                         # function
 source(file.path(dsnR,'getXebm1Dgr.R'))                      # function :: adapt model output as input to analyse()
 source(file.path(dsnR,'makehpcScript.R'))                    # function
 source(file.path(dsnR,'readCostData.R'))                     # function :: read model output
 source(file.path(dsnR,'readEbm1Data.R'))                     # function
 source(file.path(dsnR,'write2model.R'))                      # function
 source(file.path(dsnR,'writeEbm1DParameters.R'))             # function
 source(file.path(dsnR,'writeInitialConditions.R'))           # function
 source(file.path(dsnR,'writeRunParameters.R'))               # function

# experimentName = '_control_with_ice_refined';
# experimentName = '_2xCO2_with_ice_refined';
# experimentName = '_4xCO2_with_ice';
# experimentName = '_control_without_ice';
# experimentName = '_2xCO2_without_ice';
# experimentName = '_4xCO2_without_ice';

 dsnscn  <- file.path(dsn,    region, event, scn) # data for analysis (whole path) in HOME volume    [backed-up]
 dsnsim  <- file.path(dsndat, region, event, scn) # data for analysis (whole path) in storage volume [non-backed-up]
 dir.create(file.path(dsnsim,'results'))
 dsnplt <- file.path(dsnsim,'results','plots')
 if (!file.exists(dsnplt))
   dir.create(dsnplt)

 CASE_0 <- paste(scn,'000',sep='.')
 CASEROOT_0 <- file.path(dsnsim,CASE_0)                              # common for all members
 cat('CASEROOT_0:',CASEROOT_0,'\n')

 source(file.path(dsnscn,'readCF.R'),        local=TRUE)  # basic environment & forecast information
 source(file.path(dsnscn,'header_mc.R'),     local=TRUE)  # parameters for Monte Carlo analysis & sub-catchment inheritance rules
 source(file.path(dsnscn,'readprm.R'),       local=TRUE)  # parameters for assimilation

 if (!all(rownames(CF$MC[CF$MC$flag,]) %in% names(CF$ana)))
   stop('main:: uncertain parameters not included in analysis definition')
 if (!all.equal(names(CF$p),rownames(CF$MC)))
   stop('CF$p not compliant with CF$MC')
 if (prm$method %in% c('pMKF','pIKF')) {
   if (CF$m %% sum(CF$MC$flag) != 0)
    stop('ensemble size not exact multiplier of number of parameters for parameter-space method')  
 }

 # inherit parameter thresholds into analysis structure
 for (iv in 1:length(CF$MC[CF$MC$flag,])) {
   vname <- rownames(CF$MC[CF$MC$flag,])[iv]
   CF$ana[[vname]]$min <- CF$MC[vname,'min']
 }

 CF$G <- ebm1DInitializeGrids(CF$grp)
 prm$m <- CF$m
 if (prm$method %in% c('pMKF','pIKF')) # prm$m==1 just for single simulation integration
  prm$m <- CF$m + 1 # background + required for ensemble

 if (!prm$method %in% c('ETKF','pMKF','pIKF','oloop'))
  stop('unknown prm$method')

 CF$staT <- as.POSIXct(CF$staTStr, origin=originTimeStampTZ, tz='GMT')
 CF$endT <- as.POSIXct(CF$endTStr, origin=originTimeStampTZ, tz='GMT')  

 # read constants, run parameters, model parameters and initial conditions
 CF$ini <- readEbm1Data(file.path(dsnsim,'input'), CF$rp$initialConditionsFileName)
 names(CF$ini) <- c('lat','tsfc_feb','tsfc_aug','tsfc_ann') # latitude are pixel centers [-85, -75,... 85]

 CF$ref <- readEbm1Data(file.path(dsnsim,'input'), CF$rp$referenceDataFileName)
 names(CF$ref) <- names(CF$ini) # reference data for calculation of anomalies

 # read spatially distributed observations [as 's' yFMT]
 for (iy in 1:length(CF$yfnames)) {
   source(file.path(dsnscn,CF$yfnames[iy]), local=TRUE)
 }; rm(iy)
 # read assimilation scheduling protocol 
 source(file.path(dsnscn,'readRuntimes.R'), local=TRUE) # starttime & launching model times) into CF

 CF$path$dsnscn  <- dsnscn
 CF$path$oufld   <- file.path(dsnsim,'output')
 CF$path$outsfld <- file.path(dsnsim,'output','ts')

 CF$dto   <- CF$rp$deltaT
 CF$nt    <- as.numeric(diff(c(CF$runtimes,CF$endT), units='day')) # vector number of integration steps / forecast

 if (max(abs(CF$nt - as.integer(CF$nt))) > 1.0E-10)
  stop('emb1d:: integration times not exact multipliers of CF$dt')
 CF$nt <- as.integer(CF$nt)

 # randomize initial conditions (if uncertain)
 #if (CF$ana$i_tsfc$u && prm$method == 'ppKF')
 #  stop('prm$method ppKF cannot estimate initial conditions')
 it <- 1
 CF$rini <- list() # [[nrt]] components
 CF$rini[[it]] <- list()
 if (CF$ana$i_tsfc$u) {
  # CF$rini[[1]] <- genIni(CF$ini, sd=)
 } else {
  CF$rini[[it]][[1]] <- CF$ini
 }

 nrt <- length(CF$runtimes)                                      # number of forecast loops
 Ets    <- vector('list',nrt)                                    # ensemble timeseries forecast [mm]

 CF$sm <- rep(TRUE,prm$m)                                         # init surviving [stable] members
 DAmn  <- NULL # DA assimilation method for the it/iol loop

 MCgpar   <- NULL
 Pthetaa  <- NULL

 ## start looping
 for (it in 1:nrt) {
   thetal <- list()                                                    # list to store all theta list for each specific loop for posterior analysis
   itStr <- formatC(it, width=4, flag='0')
   for (il in ilR:(prm$maxiter+1)) {                                   # + 1 as confirming step
     ilStr <-  paste('f',formatC(il,width=2,flag='0'),sep='')
     cat('Loop number: ',it,' | ',ilStr,'\n')
     cat('surviving members:',sum(CF$sm),'out of',CF$m,'\n',sep=' ')
     cat('survivors:',which(CF$sm),'\n')
     cat('casualties:',which(!CF$sm),'\n')
     
     if (prm$method %in% c('pMKF','METKF') && il <= prm$maxiter) {
       prm$rfactor <- ((prm$maxiter:1) * sum(1/(prm$maxiter:1)))[il]        # [1] scaling for R for multistep filters
     } else {
       prm$rfactor <- 1
     }
     
     CF$it <- it                                                       # just required for debug mode in analyse
     CF$timenow <- CF$runtimes[it]                                     # "POSIXct" "POSIXt" 
     if (it < nrt) {
       CF$timenex <- CF$runtimes[it+1]
       CF$freerun <- FALSE
     } else {
       CF$timenex <- CF$endT
       CF$freerun <- TRUE
     }

     timenowStr <- paste(datePOSIXct(CF$timenow),'00000',sep='-')
     timenexStr <- paste(datePOSIXct(CF$timenex),'00000',sep='-')

     seqfT <- seq(trunc(CF$timenow,'day'), CF$timenex, by=CF$dto)          # model input | this forecast time sequence
     if (max(seqfT) < CF$timenex)
       seqfT <- c(seqfT,max(seqfT) + CF$dto)                  # assure it covers next interruption point (timenex)
     ntof <- length(seqfT)                                    # forecast outer timesteps
     
     # parameter sampling --RESTART option not considered
     Pthetab <- Pthetaa                                   # calculate Pthetaa always at the end of each il loop - except for 'oloop' for which it does not evolve
     dsnrest <- file.path(CASEROOT_0,'rest', timenowStr) 
     p <- CF$p
     if (it != 1 || il != 1) {  
       vKINDs <- names(theta$b)
       for (iv in 1:length(vKINDs)) {
         vKIND <- vKINDs[iv]
         p[[vKIND]][1] <- theta$a[iv]                  # mean
         if (prm$method == 'pMKF')                     # for 'pIKF' keep background0
           p[[vKIND]][2] <- sqrt(Pthetab[iv,iv])       # sd
       }
     }
     if (prm$method %in% c('pMKF','pIKF')) {                                  # parameter perturbation Kalman Filter - just parameter estimation
         #prm$sdfac <- abs(prm$sdfac) * sign(as.numeric(theta$a - theta$b)) # take previous increment signs to guide perturbations
         #prm$sdfac[prm$sdfac == 0] <- 1
         gparTheta <- orthoEnsemble(p, CF$MC, CF$m, CF$MCcons, prm$sdfac)    # background plus 1-perturbed column per uncertain parameter
         thetaNam  <- gparTheta$thetaNam
         MCgpar <- do.call(cbind,gparTheta[c('b',thetaNam)])
         ntheta <- length(thetaNam)
         colnames(MCgpar) <- c('b',rep(thetaNam, each=CF$m/ntheta))
     } else if (prm$method %in% c('ETKF','oloop')) {
         m0 <- ifelse(il == 1, CF$m, 1) # just mean integration
         MCgpar   <- mcEnsemble(p, CF$MC, m0, CF$MCcons, Pthetab)               # initially assume diagonal Pthetab
         thetaNam <- rownames(MCgpar)[which(CF$MC$flag)]                        # [ntheta]
         ntheta   <- length(thetaNam)
     } else {
         stop("main:: prm$method not in ['ETKF','pMKF','pIKF','oloop']")
     } # end parameter sampling

     if (!file.exists(dsnrest))
       dir.create(dsnrest, recursive=TRUE)
     if (prm$method %in% c('pMKF','pIKF'))
       saveRDS(gparTheta, file=file.path(dsnrest,paste(CASE_0,'gparTheta',timenowStr,ilStr,'rds',sep='.')))        # save to be able to restart 
     saveRDS(MCgpar, file=file.path(dsnrest,paste('MCgpar.b',timenowStr,ilStr,'rds',sep='.')))         # save MCgpar as background parameter space

     ntheta   <- length(thetaNam)
     theta    <- list()  
     theta$MCgpar <- MCgpar     
     if (prm$method %in% c('pMKF','pIKF')) {                       # parameter only estimate - 1st ensemble member is background
       CF$sm[] <- TRUE                                                     # sample is regenerated  
       theta$b <- gparTheta$b[thetaNam]
       theta$d <- gparTheta$dtheta                                        # NULL for m==1 |[ntheta,mperpar]
     }  else {    # ETKF
       CF$sm[] <- TRUE   
       theta$b <- sapply(p[thetaNam],FUN=function(x){x[1]})   
     } 
     if (!is.null(Pthetab)) {
       theta$Pthetab <- Pthetab
     } else {
       theta$Pthetab <- diag(sapply(CF$p[thetaNam],FUN=function(x){x[2]})^2)
     }
     if (il == 1 && it == 1) {
       theta$b0       <- sapply(CF$p[thetaNam],FUN=function(x){x[1]})
       theta$Pthetab0 <- diag(sapply(CF$p[thetaNam],FUN=function(x){x[2]})^2)
     } else {
       theta$b0       <- thetal[[1]]$b
       theta$Pthetab0 <- thetal[[1]]$Pthetab
     }

     # write model input
     m0 <- ncol(MCgpar)                                                 # applies to both parameter-space methods and ETKF, may include failed members
     err <- NULL
     for (im in 1:m0) {                                                 # serial writing of model files
       imStr <- formatC(im,width=5,flag="0",format="d")
       system(paste("rm -r -f",file.path(dsnsim,'input',imStr)))
       if (prm$method %in% c('pMKF','pIKF') && !CF$sm[im])
         stop('failed members for parameter space estimation')
       if (CF$sm[im]) {
         err[im]  <- write2model(pid = im,
                                 dto=CF$dto, nto=ntof,
                                 gpar = MCgpar[,im], CF)
       }
     } # end for im
     if (CF$hpc == 'hlrn') {
       makeHLRNscript(CF$path$hpcscript, CF$path$data,                  # create High Performance Computing .sh script -- RocksCluster
                      CF$region, CF$event, CF$scn,
                     'input', 'main.run', CF$modexe, m0, CF$nc)     
     } else {
       makehpcScript(CF$path$hpcscript, CF$path$data,                   # create High Performance Computing .sh script -- RocksCluster
                     CF$region, CF$event, CF$scn,
                     'input', 'main.run', CF$modexe, m0, CF$npc)
     }

     # runmodel
     cat('submitting climate-forecast array job |',as.character(Sys.time()),'\n')
     #cwd <- getwd()
     #setwd(dirname(CF$path$hpcscript))
     system(paste('chmod u+x',CF$path$hpcscript))
     if (CF$hpc == 'hlrn') {
       system(paste('msub ',CF$path$hpcscript))
     } else {
       system(CF$path$hpcscript)  # local at GEO
     }

     # model output files:
     # cost_function_termns.dat
     # Ebm1DParametersOut.nml
     # energy_balance_1d.dat
     # energy_transport_1d.dat
     # reference.dat
     # restart.dat

     # collect background (forecast) & calculate cost function

     Sys.sleep(30)                                  # wait some second to assure mounting in HLRN
     dthetab  <- theta$MCgpar[thetaNam,] - theta$b0 # [n,m] perturbation matrix
     costa <- readCostData(m0, dsnsim)              # note: average for last 10 years [ cp_lastInterval = 10.0yr F90 hardcoded]
     theta <- c(theta,getCost(CF$G, costa, CF$y, CF$ref, vKINDs=c('tsfc_feb','tsfc_aug'), dx=dthetab, Pb=theta$Pthetab0))
     if (prm$method %in% c('ETKF')) {
       cat('main:: il:',il,'n:survivors preNA:   ',sum(!is.na(theta$cost)), '\n') 
       cfNA             <- which(theta$cost/min(theta$cost,na.rm=TRUE) > 10) # simple QC: outlier detection
       theta$cost[cfNA] <- NA
       cfNA <- order(theta$cost)[-(1:CF$mmax)]
       theta$cost[cfNA] <- NA
       cfNA <- is.na(theta$cost) # LOGICAL
       
       if (sum(!cfNA) < CF$mmax)
         cat('main:: ',sum(!cfNA),'< CF$mmax surviving members\n')
       costa[,-1,cfNA]  <- NA
     }
     if (prm$method %in% c('pMKF','pIKF')) {
       theta$cym   <- matrix(theta$cy[-1],   ntheta, CF$m/ntheta, byrow=TRUE)
       theta$cbm   <- matrix(theta$cb[-1],   ntheta, CF$m/ntheta, byrow=TRUE)
       theta$com   <- matrix(theta$cost[-1], ntheta, CF$m/ntheta, byrow=TRUE)
       if (is.na(theta$cost[1]))
         stop('background unstable in parameter space method')
       if (any(apply(theta$com,1,FUN=function(x){all(is.na(x))})))
         stop('no member available for sensitivity calculation in parameter space method')
     } else {
       saveRDS(theta, file=file.path(dsnsim,"results", paste("theta_",it,"_il",il,".rds",sep="")))  
       #if (CF$m == 1)
       if (il > 1)
         next    
     }
     CF$sm <- !is.na(theta$cost)  
     rm(m0) 

     if (prm$method %in% c('pMKF','pIKF'))
       theta$G <- getThetaWLM(theta)             # conditional sensitivity [by weighted linear regression]
     else                # ETKF: not needed. Only for comparison with local sensitivity schemes
       theta$G <- getThetaG(theta)               # ensemble sensitivity [by SVD]  

     # Els :: gridded variables to be updated OR mapped into yls
     # Els <- getEbm1Dgr(CF, costa)                       # Els: named list with $time [POSIXct] & $stack [list with grid ensemble matrices] components. 
     X   <- getXebm1Dgr(CF$G, costa,
                        timelabs=sapply(CF$y,function(x){x[[1]]$s$data$timelab})) # input to analyseUG()
     if (!all(names(X) %in% names(CF$ana)))
       stop('main:: X vKINDs not included in analysis definition CF$ana')
   
     xu  <- sapply(CF$ana[names(X)],function(x){x$u})   # grids with matching y need to be updated via this mechanism
                                                        # xu: LOGICAL vector - whether to update each variable type in CF$ana  
                                                        # Note: alternatively they could be set up as a gauDA object
                                                        # 1 <-> 1 x-y mapping by state vector augmentation to include observations

     # gauDA structure for observations in 'g' or 'p' yFMT
     gauDA <- NULL
   
     # aug:: unobserved model parameters, boundary conditions...
     # E augmentation blocks \in [,m]
     # aug[[i]] <- list('value[.,m]','pos[.,2]','time[.]')

     aug <- list()
     for (i in 1:nrow(MCgpar)) {
       vKIND <- rownames(MCgpar)[i]
       if (is.character(CF$p[[vKIND]]) || !CF$MC[vKIND,'flag'] || !CF$MC[vKIND,'ispar'])
         next
       if (!(vKIND %in% names(CF$ana)))
         stop('main:: ',vKIND,' global parameter not in CF$ana')         
       if (!CF$ana[[vKIND]]$u)
         stop('main:: all initially random parameters should be analysed')
       aug[[vKIND]] <- list()
       aug[[vKIND]]$E       <- MCgpar[vKIND,]                 # global parameters
       aug[[vKIND]]$pos     <- CF$ana[[vKIND]]$pos            #    "       "      default == [NA,NA]
       aug[[vKIND]]$z       <- NA                             #    "       "      default == [NA,NA]     
       aug[[vKIND]]$timelab <- format(CF$timenex, format='%Y-%m-%d %H:%M:%S', usetz=TRUE) # dummy
     }

     cat('running analysis |', as.character(Sys.time()),'\n')
     if (prm$method != 'oloop') {
       Elst_a <- analyseUG(CF$G, fit=paste(it,il,sep='_il_'), prm=prm, X=X, yls=CF$y, gauDA=NULL,          # list with assimilation information
                           aug=aug, ana=CF$ana,                                                              # if prm$method=='ppKF' the returned list is theta, augmented with the $a slot
                           dsn=dsnsim, debugmode=TRUE, mpi=CF$mpi,
                           assimOLsrc=file.path(dsnDAR,'analysis','assimOL.R'), theta=theta)

       if (prm$method %in% c('pMKF','pIKF')) {    #        Elst_a: $a, $Pthetaa, $K...
         theta   <- Elst_a
         Elst_a  <- NULL                                                                                    # deallocate memory
       } else if (prm$method == 'ETKF') {         # ETKF:: Elst_a: $E, $xdf, $dydf
         vKINDs <- rownames(CF$MC)[CF$MC$ispar & CF$MC$flag]                                               # control variables
         xids <- match(vKINDs,Elst_a$xdf[,'xKIND'])
         saveRDS(MCgpar, file=file.path(dsnsim,"results", paste("MCgpar_b_it",it,"_il",il,".rds",sep=""))) # also saved as theta$MCgpar         
         MCgpar[vKINDs,] <- Elst_a$E[xids,]                               # NA columns for failed members [!CF$sm]
         MCgpar[,!CF$sm] <- NA                                            # NA completion for all parameters
         saveRDS(MCgpar, file=file.path(dsnsim,"results", paste("MCgpar_a_it",it,"_il",il,".rds",sep="")))
         theta$a       <- Elst_a$xdf[xids,'xa']
         theta$Pthetaa <- cov(t(MCgpar[vKINDs,CF$sm]))
       } else { 
         stop('main:: ---ERR002: prm$method not found---') 
       }
       Pthetaa <- theta$Pthetaa                                                                            # obtain by any method
       saveRDS(theta, file=file.path(dsnsim,"results", paste("theta_",it,"_il",il,".rds",sep="")))
     } # end if (prm$method != 'oloop') {
     thetal[[il]] <- theta
     saveRDS(thetal, file=file.path(dsnsim,"results", paste("thetal_",it,".rds",sep="")))
   } # end for (il)
   if (it == 1)
     break                     # just one model integration needed in this exercise.
 } # end for it



