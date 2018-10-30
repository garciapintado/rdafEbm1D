writeRunParameters <- function(x, dsn, fname) {
 # write input to F90 SUBROUTINE readRunParameters

 # x     :: run_parameters namelist input list   
 # dsn   :: path to file
 # fname :: filename
 if (!file.exists(dsn))
  dir.create(dsn, recursive=TRUE)
    
 zo <- file(file.path(dsn,fname),'w')
 cat('&run_parameters   startTime=',
     formatC(x$startTime, digits=1, format='f'),',\n',sep='', file=zo)
 cat('                  endTime=',
     formatC(x$endTime, digits=1, format='f'), ',\n',sep='', file=zo) 
 cat('                  deltaT=',
     formatC(x$deltaT, digits=1, format='f'),  ',\n',sep='', file=zo) 
 cat("                  initialConditionsFileName='",
     file.path(dsn,x$initialConditionsFileName),  "',\n",sep="", file=zo) 
 cat("                  restartDataFileName='",
     file.path(dsn,x$restartDataFileName),        "',\n",sep="", file=zo)
 cat("                  referenceDataFileName='",
     file.path(dsn,x$referenceDataFileName),      "',\n",sep="", file=zo)
 cat("                  observationsFileName='",
 #    file.path(dsn,x$observationsFileName),       "'/\n",sep="", file=zo)
     file.path(dsn,'observations.dat'),       "'/\n",sep="", file=zo)
 close(zo)
}
