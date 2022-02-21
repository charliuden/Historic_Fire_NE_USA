
#Script to build bar plots comparing FIA and LPJ-GUESS best fit

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
#-----
palette <- c("BNT" = "#FE630A","TBT"="#FAC10A", "BNI" = "lightblue", 'BBI'='#97BB02', "TBI"="#0A3DFA","TBIR"="#0BA186")
#------

#------
# read in LPJ-GUESS LAI data
#------
source("../R_scripts/functions/cleanup.R")
fire0.02 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-02-singledist-grc-07-fsm-04/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.7, fsm=1.4) 
#------
#Read in FIA data
fia <- read.csv("../data/FIA/FIA_PFTs.csv")[,-1]
head(fia)
#------













