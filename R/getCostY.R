getCostY <- function (G, costa, ylst, xref,
                      vKINDs=c('tsfc_feb','tsfc_aug'), as.na=-99.99) {
   # get a Cost statistics matrix for a simple Ebm1D output
   # gp: grid parameter 
   # list, INTENT(IN) :: costa      system array [possibly ensemble]
   # list, INTENT(IN) :: ylst    observation list: one variable / element
    
   # note: if xref != 0, y must also refers to anomalies


   
   jmt <- length(G$dyt) # also G$cells

   if (dim(costa)[1] != jmt)
     stop('getCost -- ERR01--')
   if (length(G$cst) != jmt)
     stop('getCost -- ERR02--')  
   if (!all(vKINDs %in% dimnames(costa)[[2]]))
     stop('getCost -- ERR03--')  
   if (!all(vKINDs %in% names(ylst)))
     stop('getCost -- ERR04--')  
   if (!all(vKINDs %in% names(xref)))
     stop('getCost -- ERR05--')  

   # useSeasonalMeansOnly = .true.     <- as F90 hardcoded
   useLongTermMean <- TRUE             # as F90 hardcoded

   m <- dim(costa)[3]
   cfm <- matrix(0,nrow=length(vKINDs),ncol=m) # cost function matrix [1 row / term
   rownames(cfm) <- vKINDs
   HEstack <- NULL
   
   for (iv in 1:length(vKINDs)) {
     vKIND <- vKINDs[iv]
     yTYPE <- names(ylst[[vKIND]])
     if (length(yTYPE) > 1)
       stop('getCost: function not ready for several yTYPE values')
     if (!('s' %in% names(ylst[[vKIND]][[yTYPE]])))
         stop("getCost: function expect yFMT=='s'")
     # get observation locations
     timelab <- ylst[[vKIND]][[yTYPE]]$s$data$timelab       # assume just one overpass
     if (length(timelab) > 1)
       stop('getCost: function not ready for several overpasses')
     ypos <- ylst[[vKIND]][[yTYPE]]$s$data[[timelab]]$pos          # 1D, pos.x==0
     xpos <- as.numeric(costa[,'y',1])      # only latitude
     xpid <- match(ypos[,2], xpos)          # x location indexes into y
     HE   <- costa[xpid,vKIND,]             # warning! needs exact location co-location
     xr   <- xref[match(ypos[,2],xref[,'lat']),vKIND]
     w    <- ylst[[vKIND]][[yTYPE]]$s$data[[timelab]]$w             # relative area of the zonal band (\in [0,1], begin 1 at the equator)
     y    <- ylst[[vKIND]][[yTYPE]]$s$data[[timelab]]$y
     R    <- ylst[[vKIND]][[yTYPE]]$s$data[[timelab]]$r               
     p    <- length(y)

     HEstack <- rbind(HEstack,HE)
         
     if (length(R) != p)
       stop('getCost --ERR05: R vector length do not match number of observations')
     
     if (any(c(length(w),length(xr),nrow(HE)) != p))
      stop('getCost -- ERR06--')
     #browser()
     if (useLongTermMean) {
       dy <- y - (HE - xr)                                      # innovations ensemble
       if (!is.matrix(dy))                                      # m=1  
         dy <- as.matrix(dy)
       # wvarsum <- function(x,w,R){0.5 * sum(w*x^2/R)/sum(w)}    # weighted 'variational' ssqsum
        wvarsum <- function(x,w,R){sum(w*x^2/R)}               # weighted 'variational' ssqsum
       cfm[iv,] <- apply(dy, MARGIN=2,FUN=wvarsum,w,R)
     } else {
       stop('getCost --ERR07 :: option not set--')
     }
   }
   #browser()
   co <- 0.5 * colSums(cfm)
   return(list(co=co,HE=HEstack))
} # end function getCost()
