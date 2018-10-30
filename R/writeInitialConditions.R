writeInitialConditions <- function(x, dsn, fname) {
 # write input to F90 SUBROUTINE readInitialConditions

 # x     :: 2D matrix of initial conditions
 # dsn   :: path to file
 # fname :: filename
 if (!file.exists(dsn))
  dir.create(dsn, recursive=TRUE)


 zo <- file(file.path(dsn,fname),'w')
 cat('% Filename: ',fname,'\n', sep='', file=zo)
 cat('% No. of header lines:  7\n', file=zo)
 cat('% No. of columns:       ',ncol(x),'\n',sep='', file=zo)
 for (i in 1:ncol(x)) {
   cat('% Column  ',i,': ',colnames(x)[i],'\n', file=zo)    
 }
 # write.table(x, file=zo, col.names=FALSE, row.names=FALSE, append=TRUE)
 # browser()
 printf <- c('%15.4f',rep('%15.2f',ncol(x)-1))
 write(formatTable(x,printf), file=zo, append=TRUE)

 close(zo)
}
