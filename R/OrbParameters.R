#MODULE OrbParameters
#   USE Constants
#   IMPLICIT NONE

  # Constant declarations (Berger 1978)  
  # Numbers of terms available for computation of orbital elements
  nef=19; nob=47; nop=78           # INTEGER, PARAMETER ::   
  # INTEGER, PARAMETER :: nef=19, nob=18, nop=9
  # Numbers of terms kept for computation of orbital elements
  neff=19; nobb=47; nopp=78        # INTEGER, PARAMETER ::   
  # INTEGER, PARAMETER :: neff=19, nobb=18, nopp=9
  # Some reference values
  xod=23.320556; xop=3.392506; prm=50.439273 # REAL, PARAMETER ::

  # Global variable declarations
  # op_scon0 = solar constant/(W m^(-2))
  # op_eccen = numerical eccentricity of the Earth's orbit (past value)
  # op_perih = longitude of the perihelion with respect to the 
  #            moving vernal equinox (past value/deg)
  # op_obliq = obliquity of the Earth's axis of rotation (past value/deg)
  # op_latit   = latitude/degN

  op_scon0 <- NULL; op_eccen <- NULL; op_perih <- NULL; op_obliq <- NULL; op_latit <- NULL  # REAL :: 

  # Cosine series data for computation of obliquity:
  # amplitude (arc seconds), rate (arc seconds/year), phase (degrees).
  aob <- rep(NA,nob) #  REAL, DIMENSION(nob) ::
  bob <- rep(NA,nob) #  REAL, DIMENSION(nob) ::
  cob <- rep(NA,nob) #  REAL, DIMENSION(nob) ::

  # Cosine/sine series data for computation of eccentricity and
  # fixed vernal equinox longitude of perihelion (fvelp):
  # amplitude, rate (arc seconds/year), phase (degrees).
  ae  <- rep(NA,nef) #  REAL, DIMENSION(nef) ::
  be  <- rep(NA,nef) #  REAL, DIMENSION(nef) ::
  ce  <- rep(NA,nef) #  REAL, DIMENSION(nef) ::
 
  # Sine series data for computation of moving vernal equinox
  # longitude of perihelion:
  # amplitude (arc seconds), rate (arc seconds/year), phase (degrees).
  aop <- rep(NA,nop) #  REAL, DIMENSION(nop) ::
  bop <- rep(NA,nop) #  REAL, DIMENSION(nop) ::
  cop <- rep(NA,nop) #  REAL, DIMENSION(nop) ::

#END MODULE OrbParameters


































