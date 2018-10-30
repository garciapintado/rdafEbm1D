# module Cost1DVariables
#    use Sizes
#    use Cost1DParameters
#    implicit none

  # Variables and fields for computing the cost function
  cv_seasonBeg <-                               # REAL :: beginnning day of year for taking long-term (seasonal) means
  cv_seasonEnd <- rep(NA,cp_nLongTermPeriods)   # REAL :: ending     day of year for taking long-term (seasonal) means

  cv_longTermTimes      <- rep(NA,cp_nLongTermPeriods)                        # REAL
  cv_longTermMeans      <- matrix(NA, cp_nLongTermPeriods, cv_nLongTermMeans) # REAL
  cv_longTermMeansTGrid <- array(NA, dim=c(jmt,cp_nLongTermPeriods,cv_nLongTermMeansTGrid))    # REAL
  cv_longTermMeansUGrid <- array(NA, dim=c(jmt,cp_nLongTermPeriods,cv_nLongTermMeansUGrid))    # REAL
 
# end module Cost1DVariables
