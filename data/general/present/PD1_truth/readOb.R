# read observations for cost function and ensemble assimilation
# here, the observation influence in the cost function is modulated (weighted) by
# the (zonal) area at the corresponding latitude. For EnKF this is mapped into an inflation of
# the observation error variance 

readOb <- function(dsn, fname, yls) {
  ytbl <- readEbm1Data(dsn, fname,
                       onames=c('lat','tsfc_feb','tsfc_aug','tsfc_ann'))
  #browser()
  ypos <- matrix(0.0, nrow=nrow(ytbl), ncol=2) # [lon,lat]
  ypos[,2] <- ytbl[,1]
  xpos     <- CF$grp$yt
  xpid     <- match(ypos[,2], xpos)      # x location [lat] indexes into y
  w        <- CF$G$cst[xpid]             # area-based weights
  p <- nrow(ypos)
  #browser()
  if (length(w) != p)
    stop('readOb -- ERR01--')

  onames <- c('tsfc_feb','tsfc_aug')     # match xnames
  ocol   <- 2:3                          # in ytlb

  for (i in 1:length(onames)) {
    dat <- list()  
    dat$time   <- CF$endT - 3600*24
    dat$names  <- 'modern'
    dat$sensor <- 'NCEP/NCAR reanalysis - surface air temperature/degC'

    dat[['modern']] <- data.frame(ypos,ytbl[,ocol[i]],w)
    names(dat[['modern']]) <- c('lon','lat',onames[i],'w')
    yls[[onames[i]]]$s$data <- dat 
  }
  return(yls)
}

CF$y <- readOb(file.path(dsnsim,'input'),'modern_climatology.dat', CF$y) # mean zonal surface air Tª at Feb & August
