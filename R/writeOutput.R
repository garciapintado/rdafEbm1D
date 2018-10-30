writeOutput <- function(M, fname1, fname2) {
 # WRITE_OUTPUT Write final snapshot to output files (after time loop).

 # Author:           Andre Paul
 # Written:          2011-11-30
 # Last updated:     2014-10-30
 #
 # Input arguments:
 #    Model           = structure array containing all model parameters and
 #                      variables
 #    filename1       = filename for output at grid-cell centers
 #    filename2       = filename for output at grid-cell boundaries
 #
 # Dependencies:     none

 # Retrieve parameters and variable(s) from structure array 'Model'

 # Write output file no. 1 (data at grid-cell centers)
 df1 <- data.frame(yC                        = M$grids$yC,
                   insolation                = M$parameters$insolation,
                   planetaryAlbedo           = M$variables$planetaryAlbedo,
                   absorbedSolarRadiation    = M$variables$absorbedSolarRadiation,
                   outgoingLongwaveRadiation = M$variables$outgoingLongwaveRadiation,
                   gDiffusion                = M$parameters$effectiveHeatCapacity * M$variables$gDiffusion,  
                   temperature               = M$variables$tracer - M$constants$pureWaterFreezingPoint)

 zo <- file(fname1, open='w')
 cat('% Filename: snapshot_yc.txt\n', file=zo)
 cat('% No. of header lines: 10\n', file=zo)
 cat('% No. of columns:       7\n', file=zo)
 cat('% Column  1: yC                        = latitude at grid-cell centers/degN\n', file=zo)
 cat('% Column  2: insolation                = incoming solar radiation/(W m-2)\n', file=zo)
 cat('% Column  3: planetaryAlbedo           = planetary albedo\n', file=zo)
 cat('% Column  4: absorbedSolarRadiation    = net shortwave radiation absorbed by planet/(W m-2)\n', file=zo)
 cat('% Column  5: outgoingLongwaveRadiation = net longwave radiation emitted by planet/(W m-2))\n', file=zo)
 cat('% Column  6: gDiffusion                = divergence of diffusive meridional energy transport/(W m-2)\n', file=zo)
 cat('% Column  7: temperature               = surface temperature/degC\n', file=zo)
 printf <- c('%12.4f','%20.6E','%20.6E','%20.6E','%20.6E','%20.6E','%20.6E')
 write(formatTable(df1,printf), file=zo, append=TRUE)
 close(zo)

 # Write output file no. 2 (data at grid-cell boundaries)
 df2 <- data.frame(yG     = M$grids$yG,
                   diffKh = M$parameters$diffKh,
                   met    = M$constants$pureWaterSpecificHeat * M$constants$pureWaterDensity *
                            M$variables$dfy)
 
 zo <- file(fname2, open='w')
 cat('% Filename: snapshot_yg.txt\n', file=zo)
 cat('% No. of header lines:  6\n', file=zo)
 cat('% No. of columns:       3\n', file=zo)
 cat('% Column  1: yG     = latitude at grid-cell boundaries/degN\n', file=zo)
 cat('% Column  2: diffKh = diffusivity/(m s-2)\n', file=zo)
 cat('% Column  3: dfy    = meridional energy transport/W\n', file=zo)
 printf <- c('%12.4f','%20.6E','%20.6E')
 write(formatTable(df2,printf), file=zo, append=TRUE)
 close(zo)
 
 return(0)
} # end function writeOutput
