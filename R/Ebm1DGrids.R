#module Ebm1DGrids
#    use Sizes
#    implicit none

#    ! Ebm1D grids
    eg_jmt  <- NULL # integer :: 
    eg_dxt  <- NULL # real
    eg_dxu  <- NULL # real
#    ! real, allocatable, dimension(:) :: eg_dyt, &
    eg_dyt  <- rep(NA,jmt) # real, dimension(1:jmt) ::
    eg_dyu  <- rep(NA,jmt) # real, dimension(1:jmt) ::
    eg_yt   <- rep(NA,jmt) # real, dimension(1:jmt) ::
    eg_yu   <- rep(NA,jmt) # real, dimension(1:jmt) ::
    eg_cst  <- rep(NA,jmt) # real, dimension(1:jmt) ::
    eg_csu  <- rep(NA,jmt) # real, dimension(1:jmt) ::
    eg_sint <- rep(NA,jmt) # real, dimension(1:jmt) ::
    eg_sinu <- rep(NA,jmt) # real, dimension(1:jmt) ::

#end module Ebm1DGrids
