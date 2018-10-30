calculateGlobalAverage <- function(M) {
 
 # Author:           Andre Paul
 # Written:          2011-11-30
 # Last updated:     2014-01-07
 # Input arguments:
 #    Model               = structure array containing grid-cell volumes
 #                          and tracer concentration
 #M
 # Output arguments:
 #    globalAveragetracer = globally-averaged temperature field
 #
 # Dependencies:     none

 # Introduce local variables for readability
 ny     <- M$sizes$ny
 volume <- M$grids$volume
 tracer <- M$variables$tracer

 # Compute volume-weighted global average
 wmean <- sum(volume * tracer) / sum(volume)

 return(wmean)
}

