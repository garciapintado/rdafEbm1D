model <- function(M, CF) {
 # MODEL Direct model integration.
 # MODEL directs the integration of the one-dimensional energy-budget model.

 # Author:           Andre Paul
 # Written:          2011-11-30
 # Last updated:     2014-01-07
 #
 # Dependencies:     integrate_model.m or
 #                   integrate_model_with_ice.m and calculate_ice_albedo.m,
 #                   calculate_global_average.m
 #

 # time loop
  for (it in 1:CF$nt) {
   CF$timenow <-  CF$timenow  + CF$dt                     # [s]
   CF$timenowY <- CF$timenowY + CF$dt / (24*3600*365)     # [yr]
   M <- integrateModel(M, CF)

   # write output to file
   TbarK <- wmean(w=M$grids$volume, x=M$x$T) 
   cat(sprintf('%12.4f %12.4f\n', M$runParameters$myTime,
       TbarK - M$c$pwfp),
       file=M$zoh)
      
 }
 return(M) 
} # end function model
