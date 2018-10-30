getXebm1Dgr <- function(G, costa, timelabs, von=NULL) {
  # get gridded centered output from Andre's Ebm1D model from a 'costa' array
  #
  # INTENT(IN) :: costa        [n,1+nv,m] array with model results
  #               being nv the number or variables, and 1st column in 1-nv coordinates
  # output is a list with named elements, each with two S3 slots:
  # $time  :: POSIXct vector of times
  # $stack :: list with length(time) [ng,m] matrices

  originTimeStampTZ <- '1970-01-01 00:00:00 UTC'

  n <- G$cells
  m <- dim(costa)[3]

  nv <- dim(costa)[2] - 1

  coslat <- as.numeric(costa[,1,1])
  #gpos   <- cbind(0,G$ryseq) # 2D fix grid geographical coordinates [2-col matrix: lon,lat]
  gpos   <- getCooG(G)
  cpid   <- match(gpos[,2],coslat)     # must match gpos in analyseUG() 
  xKINDs <- dimnames(costa)[[2]][-1]

  
  X <- list()
  for (i in 1:nv) {
    vKIND <- xKINDs[i]
    X[[vKIND]] <- list()
    X[[vKIND]]$att <- list()
    X[[vKIND]]$z   <- 0                     # no vertical coordinate - assume surface layer
    X[[vKIND]]$val <- list()
    X[[vKIND]]$val[[timelabs[i]]] <- list()
    X[[vKIND]]$val[[timelabs[i]]] <- costa[cpid,1+i,] 
  }
  
  if (!is.null(von)) {
    if (!all(von %in% names(X)))
      stop('readXebm1Dgr -- ERR001 ---')
    X <- X[von]
  }
  return(X)
} # end function readEbm1Dgr
