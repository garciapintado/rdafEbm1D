readCostData <- function(m, dsn, col.names=c('y','tsfc_feb','tsfc_aug')) {
  getmeta <- TRUE
  im <- 0
  while (getmeta && im < m) {
    im <- im + 1
    imStr <- formatC(im,width=5,flag="0",format="d")  
    dirname <- file.path(dsn,'input',imStr)
    if (!file.exists(dirname))
      stop('readCostData:: input folder not founf for im:',im,'\n')
    
    if (!file.exists(file.path(dirname,'cost_function_terms.dat'))) {
      Sys.sleep(10)
      im <- im - 1
      next # try again 
    }
    co  <- as.matrix(readEbm1Data(dirname, fname='cost_function_terms.dat'))
    getmeta <- FALSE
  }
  nr <- nrow(co)
  nc <- ncol(co)

  co <- array(NA,dim=c(nr,nc,m),
              dimnames=list(1:nr,col.names,1:m))
  #browser() 
  for (im in 1:m) {
    imStr <- formatC(im,width=5,flag="0",format="d")
    cat('reading simulation member:',im,'\n')
    dirname <- file.path(dsn,'input',imStr)
    if (!file.exists(dirname))
      stop('readCostData:: input folder not founf for im:',im,'\n')
    if (!file.exists(file.path(dirname,'cost_function_terms.dat'))) {
      Sys.sleep(30)
      im <- im - 1
      next # try again
    }
    co[,,im] <- data.matrix(readEbm1Data(dirname, fname='cost_function_terms.dat'))
  }
 #browser()
  return(co)
}
