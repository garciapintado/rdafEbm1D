#module Observations
#    use Sizes
#    use Cost1DParameters
#    implicit none

    # Observed variables and fields
  ob_tsfc <- matrix(NA,jmt,cp_nLongTermPeriods) # REAL ::  surface temperature/degC

    # Better move to Cost1DParameters and initialize in cost1DInitializeFixed?
                              

#end module Observations
