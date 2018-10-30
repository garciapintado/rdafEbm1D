#module RunParameters : global variables
#    use Constants
#    implicit none

    # Time stepping parammeters
 #rp_myIter                 # INTEGER ::current model iteration count

 rp_startTime  <- NULL             # REAL :: integration start time/s
 rp_endTime    <- NULL             # REAL :: integration ending time/s
 rp_deltaT     <- NULL             # REAL :: time step/s
 rp_nTimeSteps <- NULL            # INTEGER :: maximum number of time steps
##rp_myTime                 # REAL :: current model time/s

    # Input file names
    #character(len=c_MAX_LEN_FILENAME) :: &
 rp_initialConditionsFileName  <- NULL   # CHARACTER :: file name for inital conditions
 rp_restartDataFileName        <- NULL   # CHARACTER :: file name for final or "pickup" values
 rp_referenceDataFileName      <- NULL   # CHARACTER :: file name for reference data
 rp_observationsFileName       <- NULL   # CHARACTER :: file name for observations
   
#end module RunParameters
