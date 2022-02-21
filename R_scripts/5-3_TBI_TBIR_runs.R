
#December 15 2021
#runs to look at just tbi and tbir

#read in data 
lai_1patch = read.csv("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/lai.out")

palette <- c("TBI"="#0A3DFA","TBIR"="#0BA186")

#-----
#Libraries
library(ggplot2) #for plotting
library(dplyr) #changing data structure
library(plyr)#use aggreggate() funciton for changing data format
library(reshape2)#
library(tidyr)
library(tibble)
library(raster)
library(gstat)
library(caret) 
library(ggthemes)
library(ggplot2)
library(spdplyr)
library(patchwork)
#-----



cleanup <- function(lai=x, fireprob=y, distinterval=z, grc=w, fsm=v) { 
  #grc=-0.7 #messed up the acronym
  #fsm = 1.4
  #fireprop=0.02
  #distinterval = 200
  #lai = as.data.frame(lai, ncol=8)
  names(lai) <- c("Lon","Lat","year", "TBI", "TBIR", "C3G", "C4G", "Total")
  lai <- lai[-c(1),] 
  lai[,1] <- as.numeric(paste(lai$Lon))
  lai[,2] <- as.numeric(paste(lai$Lat))
  lai[,3] <- as.numeric(paste(lai$year))
  lai[,8] <- as.numeric(paste(lai$TBI))
  lai[,9] <- as.numeric(paste(lai$TBIR))
  lai[,10] <- as.numeric(paste(lai$C3G))
  lai[,11] <- as.numeric(paste(lai$C4G))
  lai[,12] <- as.numeric(paste(lai$Total))
  x <- c("Lon","Lat","year","TBI","TBIR", "Total")
  lai <- lai[x]
  
  fireprob <- rep(fireprob, times=nrow(lai))
  distinterval <- rep(distinterval, times=nrow(lai))
  grc <- rep(grc, times=nrow(lai))
  fsm <- rep(fsm, times=nrow(lai))
  lai <- cbind(lai, fireprob, distinterval, grc, fsm)
  
  return(lai)
}

lai_1patch = cleanup(lai=read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-0-fsm-01/lai.out"),  grc=-0.7, fsm = 1.4, fireprob=0.02, distinterval = 200)
head(lai_1patch)

lai_1patch <- as.data.frame(apply(lai_1patch, 2, as.numeric))

melt <- melt(data = lai_1patch, id.vars = "year", measure.vars = c("TBI","TBIR"))
melt[,3] <- as.numeric(paste(melt$value))
melt[is.na(melt)] <- 0
melt <- with(melt, aggregate(value, by = list(year, variable), 
                             FUN = "mean"))
names(melt) <- c("year", "PFT", "mean.lai")

p1 <- ggplot() + geom_point(data=melt, aes(x=year, y=mean.lai, col=PFT)) + scale_color_manual(values=palette) + xlab("mean leaf area index")

melt <- melt(data = filter(lai_1patch, Lat==47.25 & Lon==-69.25), id.vars = "year", measure.vars = c("TBI","TBIR"))
melt[,3] <- as.numeric(paste(melt$value))
melt[is.na(melt)] <- 0
names(melt) <- c("year", "PFT", "lai")

p2 <- ggplot() + geom_point(data= melt, aes(x=year, y=lai, col=PFT)) + ggtitle("Lat=47.25 & Lon=-69.25") + scale_color_manual(values=palette) + xlab("mean leaf area index")

melt <- melt(data = filter(lai_1patch, Lat==41.75 & Lon==-73.75), id.vars = "year", measure.vars = c("TBI","TBIR"))
melt[,3] <- as.numeric(paste(melt$value))
melt[is.na(melt)] <- 0
names(melt) <- c("year", "PFT", "lai")

p3 <- ggplot() + geom_point(data=melt, aes(x=year, y=lai, col=PFT)) + ggtitle("Lat=41.75 & Lon=-73.75") + scale_color_manual(values=palette) + xlab("mean leaf area index")

p1 + p2 + p3




####----- Fire Line Intensity ---

fli = read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-0-fsm-01/yearly_blaze_fli.out")

names(fli) <- c("Lon","Lat","year", "max_potfli", "mean_potfli")
fli <- fli[-c(1),] 
fli <- as.data.frame(apply(fli, 2, as.numeric))

ggplot() + geom_point(data=fli, aes(x=year, y=mean_potfli, color=Lat, alpha = 0.1))


melt <- melt(data = fli, id.vars = "year", measure.vars = c("mean_potfli"))
melt[,3] <- as.numeric(paste(melt$value))
melt[is.na(melt)] <- 0
melt <- with(melt, aggregate(value, by = list(year, variable), 
                             FUN = "mean"))
names(melt) <- c("year", "fli", "mean_potfli")

ggplot() + geom_point(data=melt, aes(x=year, y=mean_potfli)) + ylab("mean fire line intensity")




####----- Annual Burned Area ---
                     
ba = read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-0-fsm-01/annual_burned_area.out")
                     
names(ba) <- c("Lon","Lat","year", "BurntFr")
ba <- ba[-c(1),] 
ba <- as.data.frame(apply(ba, 2, as.numeric))                 

melt <- melt(data = ba, id.vars = "year", measure.vars = c("BurntFr"))
melt[,3] <- as.numeric(paste(melt$value))
melt[is.na(melt)] <- 0
melt <- with(melt, aggregate(value, by = list(year, variable), 
                             FUN = "mean"))
names(melt) <- c("year", "ba", "mean_BurntFr")

ggplot() + geom_point(data=melt, aes(x=year, y=mean_BurntFr)) + ylab("mean annual burned area")

ggplot() + geom_point(data=ba, aes(x=year, y=BurntFr)) + ylab("burned area")
                     
#### ----- MAP ------

library(scatterpie) #make map of pie charts


ggplot() + geom_scatterpie(data=filter(lai_1patch, year==2009), aes(x=Lon, y=Lat, r=0.1), cols=c("TBI", "TBIR"), color=NA) + scale_fill_manual(values=palette) + ggtitle("year = 2009")

ggplot() + geom_point(data=filter(fli, year==2009), aes(x=Lon, y=Lat, size=mean_potfli)) + ggtitle("year = 2009")

ggplot() + geom_point(data=filter(ba, year==2009), aes(x=Lon, y=Lat, size=BurntFr)) + ggtitle("year = 2009")


#----- FIA ------

fia <- read.csv("../data/FIA/FIA_PFTs.csv")[,-1]
head(fia)
melt <- melt(data = fia[,c("age", "TBI", "TBIR")], id.vars = "age", measure.vars = c("TBI", "TBIR"))
melt[is.na(melt)] <- 0
melt <- with(melt, aggregate(value, by = list(age, variable), 
                             FUN = "mean"))
names(melt) <- c("stand_age", "PFT", "mean_proportion_basal_area")
ggplot() + geom_point(data=melt, aes(x=stand_age, y=mean_proportion_basal_area, color=PFT)) + scale_color_manual(values=palette)
