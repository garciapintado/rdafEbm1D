getEbm1Dgr <- function(CF, costa, von=NULL) {
  # get gridded centered output from Andre's Ebm1D model from a 'costa' array
  #
  # INTENT(IN) :: costa        [n,1+nv,m] array with model results
  #               being nv the number or variables, and 1st column in 1-nv coordinates
  # output is a list with named elements, each with two S3 slots:
  # $time  :: POSIXct vector of times
  # $stack :: list with length(time) [ng,m] matrices

  originTimeStampTZ <- '1970-01-01 00:00:00 UTC'

  n <- CF$G$cells
  m <- CF$m

  nv <- dim(costa)[2] - 1

  coslat <- costa[,1,1]
  gpos   <- cbind(0,CF$G$ryseq) # 2D fix grid geographical coordinates [2-col matrix: lon,lat]
  cpid   <- match(gpos[,2],coslat) 
  
  El <- list()
  for (i in 1:nv) {
    El[[i]] <- list()
    El[[i]]$time       <- CF$timenex
    El[[i]]$stack      <- list()
    El[[i]]$stack[[1]] <- costa[cpid,1+i,] 
  }
  
  if (!is.null(von)) {
    if (length(von != nv))
      stop('readEbm1Dgr -- ERR001 ---')
    names(El) <- von
  } else {
    names(El) <- dimnames(costa)[[2]][-1]
  }

  return(El)

} # end function readEbm1Dgr
