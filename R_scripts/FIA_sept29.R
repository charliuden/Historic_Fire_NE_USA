#September 29 2021
#taking a look at how I tidied up the FIA data - is there a better way of summarizing it?
#Charlotte Uden

#FIA data mart: https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html. Choose file nawm 'STATE_TREE.csv'

#For information on what each column is and the codes within each column (ie. species names is by number: 541=white ash, 531=american beech), go to:

#https://www.fia.fs.fed.us/library/database-documentation/index.php

library(ggplot2) #for plotting
library(dplyr) #changing data structure
library(plyr)#use aggreggate() funciton for changing data format
library(reshape2)#
library(tidyr)
library(tibble)


vtTrees <- read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/VT_TREE.csv") #tree data
vtAge <- read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/VT_COND.csv") #stand age
vtCoords <- read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/VT_PLOT.csv") #lat lon



#vtTrees$CN <- format(vtTrees$CN, scientific=FALSE)
#vtTrees$CN <- as.numeric(vtTrees$CN)

str(vtTrees)

FIACleanUp <- function(table=x){
  #table2 <- data.frame(CN=table$CN, INVYR=table$INVYR, PLOT=table$PLOT, TREE=table$TREE, SPCD=table$SPCD, DIA=table$DIA)
  #names(table2) <- c("CN", "INVYR", "PLOT", "TREE", "SPCD", "DIA")
  #take out CN and merge by PLOT only 
  table2 <- data.frame(INVYR=table$INVYR, PLOT=table$PLOT, COUNTYCD=table$COUNTYCD, TREE=table$TREE, SPCD=table$SPCD, DIA=table$DIA)
  names(table2) <- c("INVYR", "PLOT", "COUNTYCD", "TREE", "SPCD", "DIA")
  table2$PLOT <- as.numeric(table2$PLOT) 
  #table2$CN <- format(table2$CN, scientific=FALSE)
  #table2$CN <- as.numeric(table2$CN)
  table2$INVYR <- as.numeric(table2$INVYR)
  table2$COUNTYCD <- as.numeric(table2$COUNTYCD)
  table2$TREE <- as.numeric(table2$TREE)
  table2$SPCD <- as.numeric(table2$SPCD)
  table2$DIA <- as.numeric(table2$DIA)
  #table2 <- filter(table2, INVYR==2010)
  
  #get Basal area
  table2 <- mutate(table2, BA = 0.005454 * DIA^2)

  return(table2)
}
vt1 <- FIACleanUp(table=vtTrees)
head(vt1)

#calcBA <- function(table=x){
  #table2 <- mutate(table, BA = 0.005454 * DIA^2)
  #return(table2)
#}
#vt2 = calcBA(table=vt1)
#head(vt2)

head(vtAge)
AgeCleanUp <- function(table=x){
  #table <- filter(table, INVYR>=2010)
  #table2 <- data.frame(CN=table$CN, PLOT=table$PLOT, STDAGE=table$STDAGE)
  #names(table2) <- c("CN", "PLOT", "STDAGE")
  #take out CN
  table2 <- data.frame(PLOT=table$PLOT, COUNTYCD=table$COUNTYCD, INVYR=table$INVYR, STDAGE=table$STDAGE)
  names(table2) <- c("PLOT", "COUNTYCD", "INVYR", "STDAGE")
  #table2 <- na.omit(table2, cols = c(STDAGE))
  table2$PLOT <- as.numeric(table2$PLOT) 
  #table2$CN <- format(table2$CN, scientific=FALSE)
  #table2$CN <- as.numeric(table2$CN)
  table2$STDAGE <- as.numeric(table2$STDAGE)
  table2$COUNTYCD <- as.numeric(table2$COUNTYCD)
  table2$INVYR <- as.numeric(table2$INVYR)
  table2 <- filter(table2, STDAGE>0)
  return(table2)
}
vtAge <- AgeCleanUp(table=vtAge)
head(vtAge)

head(vtCoords)
CoordsCleanUp <- function(table=x){
  #table <- filter(table, INVYR>=2010)
  #table2 <- data.frame(CN=table$CN, PLOT=table$PLOT, LAT=table$LAT, LON=table$LON)
  #names(table2) <- c("CN", "PLOT", "LAT", "LON")
  #not going to use CN:
  table2 <- data.frame(PLOT=table$PLOT, COUNTYCD=table$COUNTYCD, INVYR=table$INVYR, LAT=table$LAT, LON=table$LON)
  names(table2) <- c("PLOT", "COUNTYCD", "INVYR", "LAT", "LON")
  table2$PLOT <- as.numeric(table2$PLOT) 
  #table2$CN <- format(table2$CN, scientific=FALSE)
  #table2$CN <- as.numeric(table2$CN)
  table2$LAT <- as.numeric(table2$LAT)
  table2$COUNTYCD <- as.numeric(table2$COUNTYCD)
  table2$LON <- as.numeric(table2$LON)
  table2$INVYR <- as.numeric(table2$INVYR)
  table2 <- na.omit(table2, cols=c(LAT, LON))
  return(table2)
}
vtCoords <- CoordsCleanUp(table=vtCoords)

#what rows are duplicated?
arrange(vtAge, PLOT)
arrange(vtCoords, PLOT)
dup <- duplicated(vtAge[,c("INVYR", "COUNTYCD", "PLOT")])
vtAge[dup,]
duplicated(vtCoords[,c("INVYR","COUNTYCD","PLOT")])


mergecols = c("INVYR", "COUNTYCD", "PLOT")
data1 <- merge(vtCoords, vtAge, by=mergecols)
head(data1)
data2 <- merge(vt1, data1, by=mergecols, all.x=FALSE)
head(data2)



merge_attributes <- function(trees=x, coords=y, age=z){
  mergecols <- c("PLOT", "INVYR", "COUNTYCD")
  data1 <- merge(age, coords, by=mergecols)
  data2 <- merge(trees, data1, by=mergecols)
  return(data2)
}

vt <- merge_attributes(trees=FIACleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/VT_TREE.csv")), coords=CoordsCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/VT_PLOT.csv")), AgeCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/VT_COND.csv")))
vt <- cbind(vt, state=rep("vt", times=nrow(vt)))
head(vt)

ct <- merge_attributes(trees=FIACleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/CT_TREE.csv")), coords=CoordsCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/CT_PLOT.csv")), AgeCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/CT_COND.csv")))
ct <- cbind(ct, state=rep("ct", times=nrow(ct)))

ma <- merge_attributes(trees=FIACleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/MA_TREE.csv")), coords=CoordsCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/MA_PLOT.csv")), AgeCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/MA_COND.csv")))
ma <- cbind(ma, state=rep("ma", times=nrow(ma)))

me <- merge_attributes(trees=FIACleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/ME_TREE.csv")), coords=CoordsCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/ME_PLOT.csv")), AgeCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/ME_COND.csv")))
me <- cbind(me, state=rep("me", times=nrow(me)))

ny <- merge_attributes(trees=FIACleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/NY_TREE.csv")), coords=CoordsCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/NY_PLOT.csv")), AgeCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/NY_COND.csv")))
ny <- cbind(ny, state=rep("ny", times=nrow(ny)))

ri <- merge_attributes(trees=FIACleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/RI_TREE.csv")), coords=CoordsCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/RI_PLOT.csv")), AgeCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/RI_COND.csv")))
ri <- cbind(ri, state=rep("ri", times=nrow(ri)))

nh <- merge_attributes(trees=FIACleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/NH_TREE.csv")), coords=CoordsCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/NH_PLOT.csv")), AgeCleanUp(table=read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/NH_COND.csv")))
nh <- cbind(nh, state=rep("nh", times=nrow(nh)))

fia <- rbind(vt, nh, ny, ri, ct, ma, me)

speciesNamesFIA <- function(table=x){
  table$SPCD[table$SPCD==12] <- "balsamFir"
  table$SPCD[table$SPCD==43] <- "whiteCedar"
  table$SPCD[table$SPCD==68] <- "redCedar"
  table$SPCD[table$SPCD==70] <- "larch"
  table$SPCD[table$SPCD==71] <- "tamarack"
  table$SPCD[table$SPCD==91] <- "norwaySpruce"
  table$SPCD[table$SPCD==94] <- "whiteSpruce"
  table$SPCD[table$SPCD==95] <- "blackSpruce"
  table$SPCD[table$SPCD==96] <- "blueSpruce"
  table$SPCD[table$SPCD==97] <- "redSpruce"
  table$SPCD[table$SPCD==105] <- "jackPine"
  table$SPCD[table$SPCD==125] <- "redPine"
  table$SPCD[table$SPCD==129] <- "whitePine"
  table$SPCD[table$SPCD==130] <- "scotchPine"
  table$SPCD[table$SPCD==241] <- "whiteCedar"
  table$SPCD[table$SPCD==261] <- "easternHemlock"
  table$SPCD[table$SPCD==299] <- "Unk.deadConifer"
  table$SPCD[table$SPCD==310] <- "maple"
  table$SPCD[table$SPCD==313] <- "boxelder"
  table$SPCD[table$SPCD==315] <- "stripedMaple"
  table$SPCD[table$SPCD==316] <- "redMaple"
  table$SPCD[table$SPCD==317] <- "silverMaple"
  table$SPCD[table$SPCD==318] <- "sugarMaple"
  table$SPCD[table$SPCD==319] <- "mountainMaple"
  table$SPCD[table$SPCD==341] <- "ailanthus"
  table$SPCD[table$SPCD==355] <- "europeanAlder"
  table$SPCD[table$SPCD==356] <- "serviceberry"
  table$SPCD[table$SPCD==357] <- "commoneServiceberry"
  table$SPCD[table$SPCD==371] <- "yellowBirch"
  table$SPCD[table$SPCD==372] <- "sweetBirch"
  table$SPCD[table$SPCD==375] <- "paperBirch"
  table$SPCD[table$SPCD==379] <- "grayBirch"
  table$SPCD[table$SPCD==391] <- "musclewood"
  table$SPCD[table$SPCD==400] <- "hickory"
  table$SPCD[table$SPCD==402] <- "bitternutHickory"
  table$SPCD[table$SPCD==403] <- "pignutHickory"
  table$SPCD[table$SPCD==407] <- "shagbarkHickory"
  table$SPCD[table$SPCD==409] <- "mockernutHickory"
  table$SPCD[table$SPCD==500] <- "hawthorn"
  table$SPCD[table$SPCD==531] <- "americanBeech"
  table$SPCD[table$SPCD==540] <- "ash"
  table$SPCD[table$SPCD==541] <- "whiteAsh"
  table$SPCD[table$SPCD==543] <- "blackAsh"
  table$SPCD[table$SPCD==544] <- "greenAsh"
  table$SPCD[table$SPCD==552] <- "honeylocust"
  table$SPCD[table$SPCD==601] <- "butternut"
  table$SPCD[table$SPCD==602] <- "blackWalnut"
  table$SPCD[table$SPCD==660] <- "apple"
  table$SPCD[table$SPCD==680] <- "mulberry"
  table$SPCD[table$SPCD==701] <- "easternHophornbeam"
  table$SPCD[table$SPCD==741] <- "balsamPoplar"
  table$SPCD[table$SPCD==742] <- "easternCottonwood"
  table$SPCD[table$SPCD==743] <- "bigtoothAspen"
  table$SPCD[table$SPCD==746] <- "quakingAspen"
  table$SPCD[table$SPCD==761] <- "pinCherry"
  table$SPCD[table$SPCD==762] <- "blackCherry"
  table$SPCD[table$SPCD==763] <- "chockecherry"
  table$SPCD[table$SPCD==771] <- "sweetCherry"
  table$SPCD[table$SPCD==800] <- "oak"
  table$SPCD[table$SPCD==802] <- "whiteOak"
  table$SPCD[table$SPCD==804] <- "swampWhiteOak"
  table$SPCD[table$SPCD==823] <- "burOak"
  table$SPCD[table$SPCD==832] <- "chestnutOak"
  table$SPCD[table$SPCD==833] <- "redOak"
  table$SPCD[table$SPCD==837] <- "blackOak"
  table$SPCD[table$SPCD==901] <- "blackLocust"
  table$SPCD[table$SPCD==920] <- "willow"
  table$SPCD[table$SPCD==922] <- "blackWillow"
  table$SPCD[table$SPCD==934] <- "mountainAsh"
  table$SPCD[table$SPCD==935] <- "americanMountainAsh"
  table$SPCD[table$SPCD==950] <- "basswood"
  table$SPCD[table$SPCD==951] <- "americanBasswood"
  table$SPCD[table$SPCD==972] <- "americanElm"
  table$SPCD[table$SPCD==975] <- "slipperyElm"
  table$SPCD[table$SPCD==977] <- "rockElm"
  table$SPCD[table$SPCD==998] <- "whiteMangrove"
  table$SPCD[table$SPCD==999] <- "americanMangrove"
  return(table)
}


fia <- speciesNamesFIA(table=fia)
fia <- na.omit(fia) 

fia <- fia
fia <- with(fia, aggregate(BA, by = list(PLOT, INVYR, COUNTYCD, SPCD, STDAGE, LAT, LON, state), 
                                 FUN = "sum")) 
head(fia)
names(fia) <- c("PLOT", "INVYR", "COUNTYCD", "SPCD", "STDAGE", "LAT", "LON", "state", "BA.sum") #rename columns
fia <- dcast(data=fia, PLOT + INVYR + COUNTYCD + STDAGE + LAT + LON ~ SPCD, fun.aggregate = sum)
fia <- fia[,-c(7:69)]
fia <- fia %>% mutate(total.BA=apply(select(.,7:82),1,sum))
  
  
head(fia)

write.csv(fia, "/Users/charlotteuden/Desktop/trees/data/FIA_data/fia_sept29_2021_allPoints.csv")

#for the year 2010, interpolate to the PaLEON grid:

fia <- read.csv("/Users/charlotteuden/Desktop/trees/data/FIA_data/fia_sept29_2021_allPoints.csv")
fia <- filter(fia, INVYR==2010)

head(fia)




