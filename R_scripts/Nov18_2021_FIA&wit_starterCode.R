
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

# plot themes and colors (if you plot PFT by color, these are the colors I have been using...):
palette <- c("BNT" = "#FE630A","TBT"="#FAC10A", "BNI" = "lightblue", 'BBI'='#97BB02', "TBI"="#0A3DFA","TBIR"="#0BA186")

#------------------------
# FIA data
#------------------------
fia <- read.csv("fia_sept29_2021_allPoints.csv")
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

#u  se Trees column to get total tree count for each data point
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












