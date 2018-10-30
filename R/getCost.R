getCost <- function (G, costa, ylst, xref,
                     vKINDs=c('tsfc_feb','tsfc_aug'), as.na=-99.99, dx, Pb) {
   # get a Cost statistics matrix for a simple Ebm1D output
   # gp: grid parameter 
   # list, INTENT(IN) :: costa      system array [possibly ensemble]
   # list, INTENT(IN) :: ylst    observation list: one variable / element

   costY <- getCostY(G, costa, ylst, xref, vKINDs, as.na) # list(cf,HE)

   isdiag <- all(Pb[lower.tri(Pb)] == 0, Pb[upper.tri(Pb)] == 0)
   
   if (isdiag) {                                                        # note:  Paul & Losch (2012) do not multiply by 0.5 the terms J_y, J_b
     costB <- 0.5 * colSums(as.matrix(dx^2/diag(Pb)))
   } else {
     costB <- 0.5 * rowSums(crossprod(dx,solve(Pb)) * t(dx))
   }
   cost <- list(cost=costB+costY$co, cb=costB, cy=costY$co, HE=costY$HE)
   return(cost)
} # end function getCost()

