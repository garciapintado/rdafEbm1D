src <- '/home/jgp/docs/palmod/tests/R/ebm_1dDA/src'
dsninput <-       '/home/jgp/docs/palmod/tests/F90/Ebm1D/input' 
dsnx <- file.path('/home/jgp/docs/palmod/tests/F90/Ebm1D/experiments')
scn <- 'LGM1'
rp <- readRunParameters(src,dsnx,scn,dsninput)

readRunParameters <- function(src, dsnx, scn, dsninput) {
    source(file.path(src,'Constants.R'))
    # use RunParameters
    # implicit none
    # Read run parameters from file "RunParameters.nml"
    fname <- paste(scn,'RunParameters.nml',sep='_')
    
    # Local variables
    #errIO     # INTEGER :: IO error flag
    
    datfile <- file.path(dsnx,scn,fname)

    rp <- readNameList(datfile, 'run_parameters')
    #startTime # REAL :: time to start/a
    #endTime   # REAL :: time to stop/a
    #deltaT    # REAL :: time step/s

    #parameterFileName          # character(len=c_MAX_LEN_FILENAME) ::  
    #initialConditionsFileName  #  "
    #restartDataFileName        #  "
    #referenceDataFileName      #  "
    #observationsFileName       #  
      
    # Namelist variables
    #nm_run_parameters <- c('startTime', 'endTime', 'deltaT',
    #           'initialConditionsFileName',
    #           'restartDataFileName',
    #           'referenceDataFileName',
    #           'observationsFileName')
    #run_parameters <- sapply(as.character(nm_run_parameters), function(x) NULL) 


    # Format statements
    #9000 format (1X,10A)
   
    # Open parameter file
    #parameterFileName = "RunParameters.nml"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000)      & 
    #    "Run parameters are read from namelist file '", &
    #     trim( parameterFileName),"':"
    #open(unit=c_MODEL_DATA_UNIT, file= parameterFileName, status="old", iostat=errIO)
    #read (unit=c_MODEL_DATA_UNIT, nml=run_parameters, iostat=errIO)
    
    #close(c_MODEL_DATA_UNIT)

    # Report contents of model parameter file
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "startTime = ", startTime, " a"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "endTime   = ", endTime  , " a"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "deltaT    = ", deltaT   , " s"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, a, a)') "initialConditionsFileName = '", &
    #    trim(initialConditionsFileName), "'"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a ,a, a)') "restartDataFileName       = '", &
    #    trim(restartDataFileName), "'"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, a, a)') "referenceDataFileName     = '", &
    #    trim(referenceDataFileName), "'"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, a ,a)') "observationsFileName      = '", &
    #    trim(observationsFileName), "'"
   
    # Convert units
    rp_startTime <<- rp$startTime <- rp$startTime*c_DAYS_PER_YEAR*c_SECONDS_PER_DAY # [s]
    rp_endTime   <<- rp$endTime   <- rp$endTime  *c_DAYS_PER_YEAR*c_SECONDS_PER_DAY # [s]
 
    # Set run parameters
    rp_deltaT     <<- rp$deltaT                                   # time step/s
    rp_nTimeSteps <<- rp$nTimeSteps <- (rp_endTime - rp_startTime) / rp_deltaT    # maximum number of time steps
    # rp_myTime     = rp_startTime                             # model time/s, see core
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f20.1, a)') "startTime  = ", rp_startTime, " s"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f20.1, a)') "endTime    = ", rp_endTime  , " s"
    #write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, I20)')      "nTimeSteps = ", rp_nTimeSteps
    rp$initialConditionsFileName <- file.path(dsninput,basename(rp$initialConditionsFileName))
    rp$referenceDataFileName     <- file.path(dsninput,basename(rp$referenceDataFileName))
    rp$observationsFileName      <- file.path(dsninput,basename(rp$observationsFileName))

    rp_initialConditionsFileName <<- rp$initialConditionsFileName # file name for inital conditions
    rp_restartDataFileName       <<- rp$restartDataFileName       # file name for final or "pickup" values
    rp_referenceDataFileName     <<- rp$referenceDataFileName     # file name for reference data
    rp_observationsFileName      <<- rp$observationsFileName      # file name for observations
    return(rp)
} # end subroutine readRunParameters
