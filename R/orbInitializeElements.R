#   SUBROUTINE orbInitializeElements()
orbInitializeElements <- function(dsninput) {
 #use Constants
 #     use OrbParameters
 #     implicit none
      #------------------------------------------------------------------------
      # This subroutine converts the mean rates and phases in the sine and 
      # cosine expansions of the orbital elements from arc seconds/year and
      # degrees, respectively, to radian.
      #
      # Input arguments:
      #
      # Output arguments:
      #
      # Reference:
      #     A. Berger: A simple algorithm to compute long term variations
      #     of daily or monthly insolation. Institut d'Astronomique  et de 
      #     Geophysique, Universite Catholique de Louvain. Contribution 
      #     No. 18 (1978a).
      #
      # A second reference is:
      #     A. Berger: Long-Term Variations of Daily Insolation and 
      #     Quaternary Climate Changes. J. Atmos. Sci. 35, 2362-2367 (1978b). 
      #                                  
      #------------------------------------------------------------------------
       
      # Dummy arguments
      
      # Local variables
        # INTEGER :: errIO, &
        #         i, j
        #character(len=c_MAX_LEN_FILENAME) :: eccentricityDataFileName      
        #character(LEN=80) :: record

      # Format statements
      #9000 format (1X, 10A)

      # Read eccentricity data
      ecc <- read.table(file.path(dsninput,'eccentricity.dat'),
                        col.names=c('ae','be','ce'))
      if (nrow(ecc) != nef)
        stop('orbInitializeElements -- ERR01')
      
      # Read obliquity data
      obl <- read.table(file.path(dsninput,'obliquity.dat'),
                        col.names=c('aob','bob','cob'))
      if (nrow(obl) != nob)
        stop('orbInitializeElements -- ERR02')

      # Read precession data
      pre  <- read.table(file.path(dsninput,'precession.dat'),
                         col.names=c('aop','bop','cop'))
      if (nrow(pre) != nop)
        stop('orbInitializeElements -- ERR02')

      # Convert eccentricity data
      ecc$be <- ecc$be * c_SEC2RAD
      ecc$ce <- ecc$ce * c_SEC2RAD

      # Convert obliquity data
      obl$bob <- obl$bob * c_SEC2RAD
      obl$cob <- obl$cob * c_SEC2RAD
      
      # Convert precession data
      pre$bop <- pre$bop * c_SEC2RAD
      pre$cop <- pre$cop * c_DEG2RAD

      # export to global environment
      ae  <<- ecc$ae
      be  <<- ecc$be
      ce  <<- ecc$ce
      aob <<- obl$aob
      bob <<- obl$aob
      cob <<- obl$aob
      aop <<- pre$aop
      bop <<- pre$bop
      cop <<- pre$cop

      return(0)
} #   END SUBROUTINE orbInitializeElements










