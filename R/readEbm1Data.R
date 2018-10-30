readEbm1Data <- function(dsn, fname, onames=NULL) {
  # Filename:     modern_climatology.dat
  # No. of header lines:  8
  # No. of columns:       4
  # Column  1: lat     = latitude/degN
  # Column  2: tobs_jfm = NH winter (Feb) surface air temperature/degC
  # Column  3: tobs_jas = NH summer (Aug) surface air temperature/degC
  # Column  4: tobs_ann = annual-mean surface air temperature/degC
  # From NCEP/NCAR reanalysis data

  syscmd <- paste("grep -c '^%' ",file.path(dsn,fname)) # cost_function_terms.dat 
  nh0    <- as.numeric(system(syscmd, intern=TRUE))
  
  zi <- file(file.path(dsn,fname),'r')
  fnam <- strsplit(readLines(zi, n=1),': ',fixed=TRUE)
  nh   <- as.numeric(strsplit(readLines(zi, n=1),': ',fixed=TRUE)[[1]][2])
  if (nh != nh0) {
    cat('readEbm1Data: WARNING - nh0=',nh0,
        'nh=',nh,' - correcting...\n')
    if (nh > nh0)
      stop('readEbm1Data -ERR001')
    nh <- nh0
  }

  nc   <- as.numeric(strsplit(readLines(zi, n=1),': ',fixed=TRUE)[[1]][2])
  tnames <- rep('',nc)
  # gather comment lines
  ih <- 4
  ic <- 0
  while (ih <= nh) {
   strin <- readLines(zi, n=1)
   if (substr(strin,1,1) != '%')
     stop('readEbm1Data:: ih=',ih,' | header line does not start by %')
   ih <- ih + 1
   if (!grepl('Column',strin)) 
     next
   ic <- ic + 1   
   tnames[ic] <- gsub(' ','',strsplit(strin,':',fixed=TRUE)[[1]][2])
  }
  close(zi)
  if (ic != nc)
    stop('readEbm1Data --ER001--')
  #browser()
  dat <- read.table(file.path(dsn,fname), skip=nh,
                   col.names=tnames, stringsAsFactors=FALSE)

  if (!is.null(onames)) {
    if (length(onames) != ncol(dat))
       stop('readEbm1Data --ER002--')
    names(dat) <- onames
  }
  
  #syscmd <- paste("cut -c 1",file.path(dsn,fname))
  #ch1 <- scan(pipe(syscmd), what="", quiet=TRUE)
  return(dat)
}
