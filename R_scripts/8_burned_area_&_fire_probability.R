

#Is fire probability reflected in burned area?

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
palette <- c("BNT" = "#FE630A","TBT"="#FAC10A", "BNI" = "lightblue", 'BBI'='#97BB02', "TBI"="#0A3DFA","TBIR"="#0BA186")
#------
# read in LPJ-GUESS LAI data
#------
source("../R_scripts/functions/cleanup_BA.R")
  
fire0 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0, distinterval=1, grc=-0.7, fsm=1.4) 
fire1 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire1-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=1, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.1 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-1-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.1, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.01 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-01-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.01, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.02 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-02-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.02, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.05 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-05-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.05, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.001 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-001-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.001, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.002 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-002-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.002, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.005 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-005-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.005, distinterval=1, grc=-0.7, fsm=1.4) 
head(fire0.05)
#------
#read in FIA tree data
fia <- read.csv("../data/FIA/FIA_PFTs.csv")[,-1]
head(fia)
df = rbind(fire0, fire0.001, fire0.002, fire0.005, fire0.01, fire0.02, fire0.05, fire0.1, fire1)
head(df)

melt <- with(df, aggregate(BurntFr, by = list(year, fireprob), 
                             FUN = "max"))
melt[is.na(melt)] <- 0
names(melt) <- c("year", "fireprob", "BurntFr")

ggplot() + geom_point(data=filter(melt, year>1810), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1))

ggplot() + geom_point(data=filter(melt, year>1810), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1))

