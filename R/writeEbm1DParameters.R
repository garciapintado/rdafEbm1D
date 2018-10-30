writeEbm1DParameters <- function(x, xfiles, dsn, fname) {
 # write input to F90 SUBROUTINE readRunParameters

 # x     :: ebm1d_parameters namelist input list   : random parameters
 # x     :: ebm1d_parameters namelist input list   : input files
 # dsn   :: path to file
 # fname :: filename
 if (!file.exists(dsn))
  dir.create(dsn, recursive=TRUE)
    
 zo <- file(file.path(dsn,fname),'w')
  cat('&ebm1d_parameters useIceAlbedoFeedback=',
     ifelse(x['useIceAlbedoFeedback'], 'T','F'),',\n',sep='', file=zo)
 for (ip in 2:length(x)) {
  cat('                  ',names(x)[ip],'=',x[ip],',\n',sep='', file=zo)
 }
  cat("                  gridFileName='",
     file.path(dsn,xfiles$gridFileName),      "',\n",sep="", file=zo) 
  cat("                  fracLandFileName='",
     file.path(dsn,xfiles$fracLandFileName),  "',\n",sep="", file=zo)
  cat("                  fracCloudFileName='",
     file.path(dsn,xfiles$fracCloudFileName), "'/\n",sep="", file=zo)
 close(zo)
}
