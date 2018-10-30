#   SUBROUTINE orbComputeElements(t2,ecc,perh,xob,pre)
#      use Constants
#      use OrbParameters
#      implicit none
      # This subroutine computes the Earth's orbital elements. It is based on 
      # a program by A. Berger as given in:
      # 
      #     A. Berger: A simple algorithm to compute long term variations
      #     of daily or monthly insolation. Institut d'Astronomique  et de 
      #     Geophysique, Universite Catholique de Louvain. Contribution 
      #     No. 18 (1978a).
      #
      # A second reference is:
      #     A. Berger: Long-Term Variations of Daily Insolation and 
      #     Quaternary Climate Changes. J. Atmos. Sci. 35, 2362-2367 (1978b). 
     
      # Dummy arguments  
orbComputeElements <- function(t2,ecc,perh,xob,pre)
  
  #  REAL, INTENT(IN) :: t2   # long-term time in years
  #  REAL, INTENT(OUT) :: ecc,perh,xob,pre

      # Local variables
  #    INTEGER :: i,j
  #    REAL :: arg,xes,xec,rp,prg
      
      # Eccentricity e (here: ecc)
      # Since the series expansions of ecc, ecc*SIN(omegat), and 
      # ecc*COS(omegat) are so slowly convergent, Berger recommends
      # computing ecc through ecc*SIN(Pi) and ecc*COS(Pi), where Pi is the
      # longitude of the perihelion relative to the reference vernal equinox 
      # of 1950 AD.
      # From Equation (1) of Berger (1978a) or Equation (4) of Berger (1978b):
      xes = 0.0
      xec = 0.0

        #arg=be(j)*t2+ce(j)           # Phase g(j)*t2 + beta(j) of jth term
      xes <- sum(ae[1:neff]*sin(be[1:neff]*t2+ce[1:neff]))  # jth term of ecc*SIN(Pi) with amp. M(j)
      xec <- sum(ae[1:neff]*cos(be[1:neff]*t2+ce[1:neff]))  # jth term of ecc*Cos(Pi) with amp. M(j) 

      # From Equation (19) of Berger (1978a):
      ecc <- sqrt(xes**2 + xec**2)
      
      # Longitude of perihelion omegat (here: perh)
      # measured from moving vernal equinox 
      # For the reason of slow convergence, omegat is obtained from its 
      # definition omegat = Pi + Psi, where Psi is the annual general 
      # precession in longitude. 
      # Pi is determined from ecc*SIN(Pi) and ecc*Cos(Pi):

  if (abs(xec) > 1.0E-08) {
    rp <- atan(xes/xec)
    if (xec < 0.0)
      rp <- rp + c_pi
    else if (xes < 0.0)
      rp <- rp + 2.0*c_pi
  } else {
    if (xes < 0.0)
      rp <- 1.5 * c_pi
    else if (xes > 0.0) 
      rp <- 0.5*c_pi
    else
      rp <- 0.0
  }

      perh <- rp*c_RAD2DEG              # This is Pi.
      
      # From Equation (9) of Berger (1978a) or Equation (7) of Berger (1978b):
      prg = prm*t2   # prm and xop (see below) are constants of integration 
                     # obtained from initial conditions. 
      DO i=1,nopp
         arg = bop(i)*t2 + cop(i)      # Phase fp(i)*t2 + deltap(i) of ith term 
         prg = prg + aop(i)*SIN(arg)   # ith term of Psi itself
      END DO
      prg = prg/3600.0 + xop           # This is Psi.

      # From Equation (6) of Berger (1978b):
      perh = perh + prg                # This is omegat = Pi + Psi.
      # Restrict perh to the interval [0,360]
      IF (perh < 0.0) THEN
         DO 
            perh = perh + 360.0
            # EXIT statement not handled by TAMC
            # IF (perh > 0.0) EXIT
            IF (perh > 0.0) GOTO 10
         END DO
      ELSE IF (perh > 360.0) THEN
         DO 
            perh = perh - 360.0
            # EXIT statement not handled by TAMC
            # IF (perh < 360.0) EXIT
            IF (perh < 360.0) GOTO 10
         END DO
      END IF
      10 CONTINUE
      
      # Precessional parameter ecc*SIN(omegat)
      pre = ecc*SIN(perh*c_DEG2RAD)
      
      # Obliquity epsilon (here: xob)
      # From Equation (8) of Berger (1978a) or Equation (1) of Berger (1978b):
      xob = xod                                 # Constant of integration 
                                                # deduced from initial cond.
      DO i=1,nobb
         arg = bob(i)*t2 + cob(i)               # Phase f(i)*t2 + delta(i) 
                                                # of ith term
         xob = xob + aob(i)/3600.0*COS(arg)   # ith term of epsilon itself
      END DO

} #   END SUBROUTINE orbComputeElements
