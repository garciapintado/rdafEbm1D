initializeGrids <- function(dsn, gridsf, mlayd) {

    
 # mlayd :: ocen mixed layr depth
    
 zz <- file(file.path(dsn,gridsf), open='r')
 ny  <- scan(zz, what=integer(), nmax=1)  # [ng] number of grid cells
 dxG <- scan(zz, what=double(),nlines=1)  # [ng+1] lengths between cell faces in X [earth small circle lenghts at latitudes: -90:90]
 dyC <- scan(zz, what=double(),nlines=1)  # [ng] lengths between cell centers in Y [set to 0.0 at -85, then constant]
 yG  <- scan(zz, what=double(),nlines=1)  # [ng+1] latitudes of grid-lines/cell faces
 yC  <- scan(zz, what=double(),nlines=1)  # [ng] latitudes of grid cell centers
 rA  <- scan(zz, what=double(),nlines=1)  # [ng] vertical face areas (normal to sun)
 
 #dxG <- readLines(zz, 1); dxG <- as.numeric(strsplit(dxG, split=' ')[[1]])
 #  dxG <- dxG[!is.na(dxG)]
 #dyC <- readLines(zz, 1); dyC <- as.numeric(strsplit(dyC, split=' ')[[1]])
 #  dyC <- dyC[!is.na(dyC)]
 close(zz)
 
 # calculate face areas normal to X direction
 yA     <- mlayd * dxG                  # [ng+1]
 volume <- mlayd * rA                   # [n]

 M <- list()
 M$sizes <- list()
 M$sizes$ny <- ny
 M$grids <- list()
 M$grids$yG     <- yG
 M$grids$yC     <- yC
 M$grids$dyC    <- dyC
 M$grids$yA     <- yA
 M$grids$volume <- volume
 
 return(M)
 } # end function initializeGrids
 
