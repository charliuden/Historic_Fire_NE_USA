
#November 18 2021 
#starter code for greta to compare witness tree dataset with forest inventory and analysis dataset

#QUESTION: how much have PFT's changed since the witness tree dataset was recorded?

#------------------------
#Libraries
#------------------------
library(ggplot2) #for plotting
library(dplyr) #changing data structure, ie mutate and filter
library(plyr)#use aggreggate() funciton for changing data format
library(reshape2)#more funcitons for wrangling data
library(tidyr) # ..... the rest of these libraries may not be necesary for htis script....
library(tibble)
library(raster)
library(gstat)
library(caret) 
library(ggthemes)
library(patchwork)

# plot themes and colors (if you plot PFT by color, these are the colors I have been using...):
palette <- c("BNT" = "#FE630A","TBT"="#FAC10A", "BNI" = "lightblue", 'BBI'='#97BB02', "TBI"="#0A3DFA","TBIR"="#0BA186")

#------------------------
# FIA data
#------------------------
fia <- read.csv("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/FIA/FIA_PFTs.csv")
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
# Read in witness tree data
#------------------------
wit <- read.csv("/Users/charlotteuden/Desktop/trees/R_scripts/interpolation/witnessTreeNY&NE.csv") #raw from charlie cogbill

#multiple genus by 'Trees' column to get total tree count for each data point
wit <- mutate(wit, Beech=Beech*Trees, Maples=Maples*Trees, Birches=Birches*Trees,Ashs=Ashs*Trees, Hemlock=Hemlock*Trees, Basswood=Basswood*Trees, Elms=Elms*Trees, Pines=Pines*Trees, Hickories=Hickories*Trees, Spruces=Spruces*Trees, Fir=Fir*Trees, Cedar=Cedar*Trees, Oaks=Oaks*Trees, Chestnut=Chestnut*Trees, Ironwoods=Ironwoods*Trees, Poplars=Poplars*Trees, Tamarack=Tamarack*Trees, Cherries=Cherries*Trees, Chamae.is=Chamae.is*Trees, Nyssa=Nyssa*Trees, Juglans=Juglans*Trees, Buttonwood=Buttonwood*Trees, Liriodendron=Liriodendron*Trees, Magnolia=Magnolia*Trees)
# sort into pfts
wit <- mutate(wit, BNT=Fir+Spruces,
              BNI=Cedar+Pines,
              TBT=Ironwoods+Basswood+Elms+Ashs+Chestnut+Birches+Maples+Beech,
              BBI=Poplars,
              TBI=Hickories+Cherries,
              TBIR=Oaks)
# select relevant columns
wit <- wit[,c("Ver..2010.09","Lat.","Long.","Trees","BNT","BNI","TBT","BBI","TBI","TBIR")]
# rename columns
colnames(wit) <- c("state","Lat","Lon","Total","BNT","BNI","TBT","BBI","TBI","TBIR")

# Unfortunatesly there are no dates attached to each individual datapoint. 
# Instead, there is a table in cogbil's 2002 paper that gives a year interval in whcih surveys were taken or each state
# Add start and end dates:
wit <- wit %>%
  mutate(year_start = case_when(
    state=="NY" ~ 1760,
    state=="VT" ~ 1763,
    state=="MA" ~ 1623,
    state=="ME" ~ 1662,
    state=="CT" ~ 1642,
    state=="RI" ~ 1642,
    state=="NH" ~ 1673
  ))
wit <- wit %>%
  mutate(year_end = case_when(
    state=="NY" ~ 1811,
    state=="VT" ~ 1820,
    state=="MA" ~ 1835,
    state=="ME" ~ 1835,
    state=="CT" ~ 1818,
    state=="RI" ~ 1818,
    state=="NH" ~ 1850
  ))

head(wit)


##### comparison using boxplots

fia <- read.csv("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/FIA/FIA_PFTs.csv")
wit <- read.csv("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/witness_trees/witness_treees_PFTs.csv")
wit = cbind(wit, dataset=rep("WitnessTrees_1623_1850", nrow(wit)))
fia = cbind(fia, dataset=rep("ForestInventory_2010", nrow(fia)))

head(fia)
head(wit)
mean_lai_pft = function(data=x){
  #fire prob must be character string, ie: "fore_0"
  pft_means = cbind(mean(data$BNT), mean(data$BBI), mean(data$TBT), mean(data$TBI), mean(data$TBIR), mean(data$BNI))
  mean_total = sum(sum(data$BNT), sum(data$BBI), sum(data$TBT), sum(data$TBI), sum(data$TBIR), sum(data$BNI)) /(nrow(data[,4:9])*ncol(data[,4:9]))
  #PFT = c("BNT", "BBI", "TBT", "TBI", "TBIR", "BNI")
  data = cbind(pft_means, mean_total, fire, fsm, grc)
  names(data) = c("BNT", "BBI", "TBT", "TBI", "TBIR", "BNI", "total")
  return(data)
}

df = rbind(fia[,c("BNT", "BBI", "TBT", "TBI", "TBIR", "BNI", "dataset")], wit[,c("BNT", "BBI", "TBT", "TBI", "TBIR", "BNI", "dataset")])

melt <- melt(data = df, id.vars = c("dataset"), measure.vars = c("BNT","BNI","TBT","BBI","TBI","TBIR"))
melt[is.na(melt)] <- 0
names(melt) <- c("dataset", "PFT", "proportion")

head(melt)

#-----
palette <- c("BNT" = "#FE630A","TBT"="#FAC10A", "BNI" = "lightblue", 'BBI'='#97BB02', "TBI"="#0A3DFA","TBIR"="#0BA186")
#------


ggplot() + geom_boxplot(data=filter(melt,PFT=="BNT"), aes(x=dataset, y=proportion, fill="#FE630A"))

ggplot(melt, aes(x=PFT, y=proportion, fill=dataset)) + 
  geom_boxplot()

ggplot(d, aes(x=reorder(fac, y, mean), y=y))

ggplot(melt, aes(x=reorder(dataset, proportion, mean), y=proportion, fill=PFT)) + 
  geom_boxplot() +
  scale_fill_manual(values=palette) +
  xlab("")

#----
#bar charts to compare genera

#map points
source("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/R_scripts/functions/base_map.R")
map = base_map()
fia = read.csv("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/FIA/fia_sept29_2021_allPoints.csv")
wit <- read.csv("/Users/charlotteuden/Desktop/trees/R_scripts/interpolation/witnessTreeNY&NE.csv") 
c = map + geom_point(data=wit, aes(x=Long., y=Lat.))
d = map + geom_point(data=fia, aes(x=LON, y=LAT))

head(wit)
wit = cbind(Oaks=sum(wit$Oaks), Beech=sum(wit$Beech), Maples=sum(wit$Maples), Hemlock=sum(wit$Hemlock), Pines=sum(wit$Pines), Birches=sum(wit$Birches), Spruces=sum(wit$Spruces), Hickories=sum(wit$Hickories), Chestnut=sum(wit$Chestnut), Ashs=sum(wit$Ashs), Fir=sum(wit$Fir), Elms=sum(wit$Elms), Basswood=sum(wit$Basswood), Ironwood=sum(wit$Ironwood), Poplars=sum(wit$Poplars), Cedar=sum(wit$Cedar), Cherries=sum(wit$Cherries))

head(fia)
fia = cbind(
Oaks = sum(fia$redOak, fia$whiteOak, fia$blackOak, fia$chestnutOak), 
Beech = sum(fia$americanBeech), 
Maples = sum(fia$redMaple, fia$sugarMaple), 
Hemlock = sum(fia$easternHemlock), 
Pines = sum(fia$whitePine, fia$redPine), 
Birches = sum(fia$yellowBirch, fia$paperBirch), 
Spruces = sum(fia$redSpruce, fia$whiteSpruce, fia$blackSpruce),
Hickories = sum(fia$mockernutHickory, fia$shagbarkHickory, fia$bitternutHickory, fia$pignutHickory),
Chestnut = 0, 
Ashs = sum(fia$whiteAsh), 
Fir = sum(fia$balsamFir),
Elms = sum(fia$americanElm),
Basswood = sum(fia$americanBasswood), 
Ironwoods = sum(fia$easternHophornbeam), 
Poplars = sum(fia$quakingAspen, fia$bigtoothAspen, fia$easternCottonwood), 
Cedar = sum(fia$whiteCedar), 
Cherries = sum(fia$blackCherry)
)

df = as.data.frame(rbind(wit, fia))
df = cbind(df, dataset=c("historic", "modern"))

#standardise
df = mutate(df, total = Oaks+Beech+Maples+Hemlock+Pines+Birches+Spruces+Hickories+Chestnut+Basswood+Ironwood+Poplars+Cedar+Cherries)
df = mutate(df, Oaks=Oaks/total, Beech=Beech/total, Maples=Maples/total, Hemlock=Hemlock/total, Pines=Pines/total, Birches=Birches/total, Spruces=Spruces/total, Hickories=Hickories/total, Chestnut=Chestnut/total, Basswood=Basswood/total, Ironwood=Ironwood/total, Poplars=Poplars/total, Cedar=Cedar/total, Cherries=Cherries/total)

melt <- melt(data = df, id.vars = c("dataset"), measure.vars = c("Oaks","Beech","Maples","Hemlock","Pines","Birches","Spruces","Hickories","Chestnut","Basswood","Ironwood","Poplars","Cedar","Cherries"))
melt[is.na(melt)] <- 0
names(melt) <- c("dataset", "Genus", "proportion")

melt

 a = ggplot() + geom_bar(stat='identity', data=filter(melt, dataset=="historic"), aes(x=reorder(Genus, -proportion), y=proportion)) + xlab("") + ggtitle("Observed Historc: 1623 - 1850") + scale_y_continuous(breaks=seq(from=0, to=0.3, by=0.1))
 b = ggplot() + geom_bar(stat='identity', data=filter(melt, dataset=="modern"), aes(x=reorder(Genus, -proportion), y=proportion)) + xlab("") + ggtitle("Observed Modern: 2010") + scale_y_continuous(breaks=seq(from=0, to=0.3, by=0.1))

(a + c) / (b + d)
 map
