#module Constants
#    implicit none

    # Set named constants
    #c_pi       = 3.14159265358979 #  real, parameter

    # Conversion factors
    c_DEG2RAD  = pi/180.0       # real, parameter :: degrees to radians 
    c_RAD2DEG  = 1.0/c_DEG2RAD    # real, parameter :: radians to degrees
    c_DEG2DIST = 111194.9         # real, parameter :: distance that corresponds
                                  #                    to one degree of latitude/m
    c_SEC2RAD = c_DEG2RAD/3600.0  # real, parameter :: seconds to radians
   
    c_DAYS_PER_YEAR   =   365.0   # real, parameter :: 
    c_SECONDS_PER_DAY = 86400.0   # real, parameter :: 
    c_MAX_LEN_FILENAME      = 512 # integer, parameter :: maximum length of a file name 
    c_STANDARD_MESSAGE_UNIT = 6   #  "           "
    c_MODEL_DATA_UNIT       = 14  #  "           " 
    c_ERROR_MESSAGE_UNIT    = 15  #  "           "
#end module Constants
