ebm1DInitializeGrids <- function(gp) {

  # gp :: grid parameters list

  G <- list()
  #  Set up longitudinal grids
  G$dxt <- gp$dxtdeg * c_DEG2DIST
  G$dxu <- gp$dxudeg * c_DEG2DIST

  # Set up latitudinal grids
  G$dyt <- gp$dytdeg * c_DEG2DIST
  G$dyu <- gp$dyudeg * c_DEG2DIST   

  # Compute arrays derived from the latitudinal grid spacing
  G$cst  <- cos(gp$yt * c_DEG2RAD)
  G$csu  <- cos(gp$yu * c_DEG2RAD)
  G$sint <- sin(gp$yt * c_DEG2RAD)
  G$sinu <- sin(gp$yu * c_DEG2RAD)

  # mapping to adapt to analyse()
  G$LOCATION <- ''
  G$MAPSET   <- ''
  G$rows  <- as.integer(gp$jmt - 2)
  G$cols  <- as.integer(1)
  G$cells <- G$rows * G$cols  # centered variables 
  G$xseq  <- 0
  G$yseq  <- gp$yt[2:(gp$jmt-1)] # e.g [-85...85]
  G$ryseq <- rev(G$yseq)
  G$w     <- -180 # º
  G$e     <-  180 #
  G$s     <- -90
  G$n     <-  90
  G$xlims <- c(G$w, G$e)
  G$ylims <- c(G$s, G$n)
  G$ewres <- 360
  G$nsres <-  10

  attr(G, "class") <- "gmeta6"
  
  return(G)
}
