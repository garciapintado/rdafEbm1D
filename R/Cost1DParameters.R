#module Cost1DParameters
#    implicit none

#    ! Parameters for computing the cost function

#    ! Number of periods for taking long-term means 
    cp_nLongTermPeriods <- 3   # integer, parameter :: 

    # IDs for time periods
    cp_iFeb = 1    # INTEGER, parameter :: Northern Hemisphere winter (February)
    cp_iAug = 2    # INTEGER, parameter :: Northern Hemisphere summer (August)
    cp_iAnn = 3    # INTEGER, parameter :: annual mean

    # IDs for variables
    cp_iyice_s    = 1 # INTEGER, PARAMETER :: ! scalar values
    cp_iyice_n    = 2 # INTEGER, PARAMETER ::
    cp_isolin     = 1 # INTEGER, PARAMETER :: ! on T grid
    cp_idiv_heddy = 2 # INTEGER, PARAMETER ::
    cp_iapln      = 3 # INTEGER, PARAMETER ::
    cp_inetswpln  = 4 # INTEGER, PARAMETER ::
    cp_inetlwpln  = 5 # INTEGER, PARAMETER ::
    cp_itsfc      = 6 # INTEGER, PARAMETER :: 
    cp_iheddy     = 1 # INTEGER, PARAMETER :: ! on U grid  

    # Dimensions (no. of variables) for taking long-term means  

    cv_nLongTermMeans      = 2 # INTEGER, PARAMETER ::
    cv_nLongTermMeansTGrid = 6 # INTEGER, PARAMETER ::
    cv_nLongTermMeansUGrid = 1 # INTEGER, PARAMETER :: 

    # AP 2011-01-4: new variable names
    cp_longitudeBeg <- rep(NA,cp_nLongTermPeriods) # REAL :: beginnning true longitude for taking long-term means
    cp_longitudeEnd <- rep(NA,cp_nLongTermPeriods) # REAL :: ending true longitude for taking long-term means   

    # AP 2011-01-4: old variable names
    # real, dimension(1:cp_nLongTermPeriods) :: cp_startLongitude, & ! beginnning true longitude for taking long-term means
    #                                           cp_endLongitude      ! ending     true longitude for taking long-term means
    cp_lastInterval <- NULL # REAL :: length of interval for accumulating long-term mean

#end module Cost1DParameters
