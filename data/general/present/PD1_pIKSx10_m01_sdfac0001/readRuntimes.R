yt <- CF$y[[1]][[1]]$s$data$time             # [POSIXct] SAT snapshot times
yt <- yt[yt > CF$staT & yt < CF$endT]
CF$runtimes <- c(CF$staT, yt)                # POSIXct
rm(yt)
