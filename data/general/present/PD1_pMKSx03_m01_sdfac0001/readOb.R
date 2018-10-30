# read observations for cost function and ensemble assimilation
# here, the observation influence in the cost function is modulated (weighted) by
# the (zonal) area at the corresponding latitude. For EnKF this is mapped into an inflation of
# the observation error variance 

readOb <- function(dsn, fname, yls) {

  y_gen <- list()                     # observations: generic structure
  y_gen$fname <- ''                   # If empty string [''], it must be set in readOb.R
  y_gen$nto   <- Inf                  # maximum number of assimilates times/observation yTYPE
  y_gen$qc    <- 'none'               # QC method
  y_gen$r     <- NA                   # scalar observation variance
  y_gen$twin  <- Inf                  # [s] maximum assimilation window from present to past [Inf: all integration run]
  y_gen$use   <- TRUE                 # assimilate or not
  y_gen$w     <- TRUE                 # TRUE for weigthed observations - via w-scaling R

  # read table of [ºC] sea air Temperature.
  # These are 10-yr average monthly zonal mean data of sea air Temperature for winter & summer
  ytbl <- readEbm1Data(dsn, fname,
                       onames=c('lat','tsfc_feb','tsfc_aug','tsfc_ann'))
  ypos <- matrix(0.0, nrow=nrow(ytbl), ncol=2) # [lon,lat]
  ypos[,2] <- ytbl[,1]
  xpos     <- CF$grp$yt
  xpid     <- match(ypos[,2], xpos)      # x location [lat] indexes into y
  w        <- CF$G$cst[xpid]             # area-based weights
  p <- nrow(ypos)

  if (length(w) != p)
    stop('readOb -- ERR01--')

  yKINDs <- c('tsfc_feb','tsfc_aug')     # match xnames
  yTYPE  <- 'NCEP_NCAR_reanalysis'       # just this one for each yKIND
  yFMT   <- 's'                          # densely distributed
  timelabs <- paste('1990-1999',c('feb','aug'),sep='-') # just one here for each yKIND

  wsum <- sum(w) * length(yKINDs) # weights sum to one in Paul$losch
  cat('readOb:: wsum=',wsum,'\n')
  w <- w / wsum
  
  y <- list()
  for (k in 1:length(yKINDs)) {
    yKIND <- yKINDs[k]
    y[[yKIND]]                  <- list()
    y[[yKIND]][[yTYPE]]         <- list()
    y[[yKIND]][[yTYPE]][[yFMT]] <- y_gen
    
    sdata <- list() 
    sdata$names   <- 'modern'
    sdata$sensor  <- 'NCEP/NCAR reanalysis - surface air temperature'
    sdata$time    <- CF$endT - 3600*24
    sdata$timelab <- timelabs[k]
    sdata$units   <- 'degC'
    sdata$yfield  <- 'y'
    sdata[[timelabs[k]]] <- list()
    sdata[[timelabs[k]]]$pos <- ypos
    sdata[[timelabs[k]]]$z   <- rep(0,p)
    sdata[[timelabs[k]]]$y   <- ytbl[,yKIND]
    sdata[[timelabs[k]]]$r   <- rep(1,p) # as Paul & Losch (2012)
    sdata[[timelabs[k]]]$w   <- w
    y[[yKIND]][[yTYPE]][[yFMT]]$data <- sdata 
  }
  
  return(y)
}

CF$y <- readOb(file.path(dsnsim,'input'),'modern_climatology.dat', CF$y) # mean zonal surface air Tª at Feb & August

rm(readOb)
