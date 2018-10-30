formatTable <- function(x,printf, collapse=TRUE) {

 if (length(printf) != ncol(x))
   stop('formatTable:: --ERR01--')
     
 xf <- sprintf(printf[1], x[,1])
 for (i in 2:ncol(x)) {
   if (collapse) {
     xf <- paste(xf, sprintf(printf[i], x[,i]),sep='')
   } else {
     xf <- cbind(xf, sprintf(printf[i], x[,i]))
   }
 }
 return(xf)
 
}
