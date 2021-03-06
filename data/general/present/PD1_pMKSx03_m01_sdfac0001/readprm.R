# readprm.R (filter parameters)
#
#readprm <- function(){

# Notes:
# loc_distype:
# - 'earth' assumes lat lon input coordinates for grid and observation positions
#   then uses great circle distances fr localisation. Still $ll (localisation distance)
#   has to be input in [m]

prm <- list()
prm$method  <- rep('pMKF')                   # DA method 'oloop' for no assimilation ['pMFK','pIKF','FETKF','IETKF']
prm$maxiter <-  3                            # maximum number of iterations for nonlinear assimilation
prm$ifrac   <-  1                            # this fraction [1 <= ifrac <= nfrac]
prm$loc_boo      <- TRUE                     # whether to conduct localization 0 or 1 [apply to all elements in the augmented state vector]
prm$loc_method   <- 'LA'                     # localization method. 'CF' (covariance filtering), or 'LA' (local analysis)
prm$loc_function <- 'Gaspari_Cohn'           # tag for the localisation function (calc_loccoeffs.m)
prm$loc_distype  <- 'earth'                  # '2D' for Euclidean, or 'SG' for along network distances [apply to all elements in the augmented state vector]
prm$rotate       <- FALSE                    # 0 = do not rotate. 1 = rotate. The code has to prepared to set prm.rotate each some assimilation steps
prm$rotate_ampl  <- 1                        # always 1. Code not prepared for other values
prm$sdfac        <- 0.001                    # OPT, used only for parameter-space methods. Scaler of standard deviations for parameter perturbation.
#return(prm)
#}
