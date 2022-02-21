#-------
#Function to cleaen up lpj-guess output
#-------


cleanup <- function(lai=x, fireprob=y, distinterval=z, gcr=w, fsm=v) { 
  #lai=read.table("/Users/charlotteuden/Desktop/trees/data/LPJ-GUESS_output/GRC_FSM/GCR0.1_FSM1.1_fire0.1_climate1200/fire0-02/lai.out")
  #fireprob=0.0
  #distinterval=0
  #gcr=0.2
  #fsm=1.2
  #annualBurn=read.table("/Users/charlotteuden/Desktop/trees/data/LPJ-GUESS_output/GRC_FSM/GCR0.1_FSM1.1_fire0.1_climate1200/fire0-02/annual_burned_area.out")
  grc=gcr #messed up the acronym
  names(lai) <- c("Lon","Lat","year","BNT","BNI","TBT","BBI","TBI","TBIR", "C3G", "C4G","Total")
  lai <- lai[-c(1),] 
  lai[,1] <- as.numeric(paste(lai$Lon))
  lai[,2] <- as.numeric(paste(lai$Lat))
  lai[,3] <- as.numeric(paste(lai$year))
  lai[,4] <- as.numeric(paste(lai$BNT))
  lai[,5] <- as.numeric(paste(lai$BNI))
  lai[,6] <- as.numeric(paste(lai$TBT))
  lai[,7] <- as.numeric(paste(lai$BBI))
  lai[,8] <- as.numeric(paste(lai$TBI))
  lai[,9] <- as.numeric(paste(lai$TBIR))
  lai[,10] <- as.numeric(paste(lai$C3G))
  lai[,11] <- as.numeric(paste(lai$C4G))
  lai[,12] <- as.numeric(paste(lai$Total))
  x <- c("Lon","Lat","year","BNT","BNI","TBT","BBI","TBI","TBIR", "C3G", "C4G","Total")
  lai <- lai[x]
  
  fireprob <- rep(fireprob, times=nrow(lai))
  distinterval <- rep(distinterval, times=nrow(lai))
  grc <- rep(grc, times=nrow(lai))
  fsm <- rep(fsm, times=nrow(lai))
  lai <- cbind(lai, fireprob, distinterval, grc, fsm)
  
  return(lai)
}


#test:
  #lai <- cleanup(lai=read.table("/Users/charlotteuden/Desktop/trees/data/LPJ-GUESS_output/GRC_FSM/GCR0.1_FSM1.1_fire0.1_climate1200/fire0-02/lai.out"),fireprob=0.0, distinterval=0, gcr=0.2, fsm=1.2, annualBurn=read.table("/Users/charlotteuden/Desktop/trees/data/LPJ-GUESS_output/GRC_FSM/GCR0.1_FSM1.1_fire0.1_climate1200/fire0-02/annual_burned_area.out"))
#head(lai)
