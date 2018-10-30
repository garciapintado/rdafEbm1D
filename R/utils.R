trynumeric <- function(x){
  
  if(all(is.na(suppressWarnings(as.numeric(x)))))
    x
  else
    as.numeric(x)
  
}


parsePARline <- function(s){
  
  if(length(s) > 1)s <- paste(s, collapse= " ")
  
  sp <- strsplit(s, "=")[[1]]
  if(length(sp) != 2)stop("Fatal error in parsePAR.")
  
  parname <- str_trim(sp[1])
  parval <- str_trim(sp[2])
  parval <- substr(parval,1,nchar(parval)-1)
  
  # Single numeric
  tp <- trynumeric(parval)
  if(is.numeric(tp))return(tp)
  
  # Not returned yet - try splitting
  spl <- strsplit(parval, "[[:space:]]")[[1]]
  
  # Single character
  if(length(spl) == 1){
    v <- gsub("'","", parval)
    return(v) 
  }
  
  # Multiple numeric or character
  if(!is.numeric(trynumeric(spl))){
    parval <- gsub("'","", spl)
  } else {
    parval <- trynumeric(spl)
  }
  
  return(parval)
}

readNameList <- function (datfile, namelist) {
    r <- str_trim(readLines(datfile))
    nmStart <- grep(paste0("&", namelist), r, ignore.case = TRUE)
    r <- r[nmStart[1]:length(r)]
    #r <- r[1:grep("^/$", r)[1]]
    #r <- r[-c(1, length(r))]
    r[1] <- substring(r[1],nchar(namelist)+3)
    #r <- delempty(r)
    parloc <- grep("=", r)
    last <- length(r) - parloc[length(parloc)] + 1
    nlines <- c(diff(parloc), last)
    l <- list()
    for (i in 1:length(parloc)) {
        ind <- parloc[i]:(parloc[i] + nlines[i] - 1)   
        l[[i]] <- parsePARline(r[ind])
    }
    parnames <- str_trim(sapply(strsplit(r[parloc], "="), "[",1))
    #names(l) <- tolower(parnames)
    names(l) <- parnames
    return(l)
}

are.null <- function(x) {
 # character vector
 sapply(x, function(x) {is.null(eval(parse(text=x)))})
} # e.g. are.null(ls())

}
