#Charlotte Uden
#November 30 2021

#Script to sort the Forest Inventory and Analysis data into PFT's.

#------------------------
#Libraries
#------------------------
library(ggplot2)
library(dplyr) 
library(plyr)
library(reshape2)
library(tidyr) 
library(tibble)
library(raster)
library(gstat)
library(caret) 
library(ggthemes)
#------------------------
#------------------------
# FIA data
#------------------------
fia <- as.data.frame(read.csv("../data/FIA/fia_sept29_2021_allPoints.csv")[,2:84])
# take a look at the data
head(fia)
# filter for only records from 2010
fia <- filter(fia, INVYR==2010)
# sort species into PFT's
fia_pft <- mutate(fia, 
                  BNI=whitePine+redPine, 
                  BNT=whiteCedar+balsamFir+blackSpruce+redSpruce+whiteSpruce+whiteCedar, 
                  BBI=quakingAspen+paperBirch, 
                  TBT=sugarMaple+americanBeech+yellowBirch+americanBasswood+whiteAsh+americanElm+bitternutHickory+mockernutHickory+pignutHickory+shagbarkHickory+easternHophornbeam+redMaple, 
                  TBI=blackCherry, 
                  TBIR=redOak+blackOak+whiteOak)
# select relevant columns
cols <- c("LAT", "LON", "STDAGE", "BNT", "BBI", "TBT", "TBI", "TBIR", "BNI")
fia_pft <- fia_pft[,cols]
# rename the columns
colnames(fia_pft) <- c("Lat", "Lon", "age", "BNT", "BBI", "TBT", "TBI", "TBIR", "BNI")
# standardise
fia_pft <- mutate(fia_pft, total=BNT+BBI+TBT+TBI+TBIR+BNI)
fia_pft <- mutate(fia_pft, BNT=BNT/total,BBI=BBI/total,TBT=TBT/total,TBI=TBI/total,TBIR=TBIR/total,BNI=BNI/total)
# replace NA values with 0
sum(is.na(fia_pft))
fia_pft[is.na(fia_pft)] <- 0
sum(is.na(fia_pft))
# take a look at the data
head(fia_pft)
#------------------------
write.csv(fia_pft, "../data/FIA/FIA_PFTs.csv")
#------------------------
