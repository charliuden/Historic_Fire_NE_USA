#function to clean up fraction of burned area output. 

cleanup_BA <- function(data=x, fireprob=y, distinterval=v, grc=z, fsm=w) {
  #data = lai
  names(data) <- c("Lon","Lat","year","BurntFr")
  data <- data[-c(1),] 
  data[,1] <- as.numeric(paste(data$Lon))
  data[,2] <- as.numeric(paste(data$Lat))
  data[,3] <- as.numeric(paste(data$year))
  data[,4] <- as.numeric(paste(data$BurntFr))
  fireprob <- rep(fireprob, times=nrow(data))
  distinterval <- rep(distinterval, times=nrow(data))
  grc <- rep(grc, times=nrow(data))
  fsm <- rep(fsm, times=nrow(data))
  data <- cbind(data, fireprob, distinterval, grc, fsm)
  return(data)
}