
#DEcember 18 2021
#finding best fit from precentage residuals

# ** to account for biased from big/more abundant PFTs
#The observed differences in smaller PFT’s have more weight
#Allows us to pay morea ttention to the smaller pfts 

#--we want to get tbir, but when small movements happen in tbir, we don’t see it
#Standardise PFTs after summing the residuals to account for weight of dominant PFT’s. 
#     •Abs(Obs – pred at every point for every PFT) 
#     •Sum each pft across all points
#     •Standardize by dividing by sum(observed pft)


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
#read in FIA tree data
fia <- read.csv("../data/FIA/FIA_PFTs.csv")[,-1]
head(fia)

source("../R_scripts/functions/cleanup.R")

lai = cleanup(lai=read.table("/Users/charlotteuden/Desktop/trees/data/LPJ-GUESS_output/final_runs_11-01-2021/fire0-005-climate1200-grc-06-fsm-05/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.6, fsm=1.5)
head(lai)
#-----

#function to get residuals of best neighbor
weighted_residuals_best_neighbor_in_neighborhood_FIA <- function(obs=x, pred=y, neighbors=z) {
  #pred = lai
  #obs = fia
  #neighbors = 5
  #standardise pred
  pred <- mutate(pred, year=year-1809)
  pred <- mutate(pred, total=BNT+BBI+TBT+TBI+TBIR+BNI)
  pred <- mutate(pred, BNT=BNT/total,BBI=BBI/total,TBT=TBT/total,TBI=TBI/total,TBIR=TBIR/total,BNI=BNI/total)
  pred[is.na(pred)] <- 0
  
  #get fire probability, grc adn fsm
  fireprob = pred[1,c("fireprob")]
  grc = pred[1,c("grc")]
  fsm = pred[1,c("fsm")]
  
  # 1. make obs and pred spatial objects
  sp.obs <- obs#obs
  sp.pred <- pred#pred
  coordinates(sp.obs) <- ~Lon+Lat
  coordinates(sp.pred) <- ~Lon+Lat
  
  #2. make an empty data frame to hold the residuals
  resid <- data.frame(Lat=as.numeric(), 
                      Lon=as.numeric(), 
                      age=as.numeric(),
                      BNT=as.numeric(),
                      BBI=as.numeric(),
                      TBT=as.numeric(),
                      TBI=as.numeric(),
                      TBIR=as.numeric(),
                      BNI=as.numeric())
  
  # 3. for loop to take each row in fia data
  for (i in seq(from=1, to=nrow(obs))){
    #i=1
    coord = sp.obs[i,]
    # 2. find the stand age of that row
    age = coord[,1]
    age = as.data.frame(age)[1,1]
    # 3. filter lai for the age/year 
    sp.pred.age = filter(sp.pred, year==age)
    #make sure there are values in sp.pred.age 
    if (nrow(as.data.frame(sp.pred.age)) == 0) {
      next
    }
    # 4. find the distance of all these lai points to the fia point
    distances = pointDistance(coord, sp.pred.age, lonlat=TRUE, allpairs=FALSE)
    # 5. add distances to the predicted sp dataframe and plot to make sure it worked
    sp.pred.age$distances <- c(distances)
    #ggplot() + geom_point(data=as.data.frame(sp.pred.age), aes(x=Lon, y=Lat, size=distances))
    # 6. get the minimum n distances and their associated coordinates and variables 
    #(n=neighbors as input for this funciton)
    sp.pred.age = as.data.frame(sp.pred.age) %>% arrange(distances)
    neighbors_in_neighborhood = sp.pred.age[1:neighbors,]
    #get best neighbor from neighbors in neighborhood using SSR
    
    SSR_test = 100000000000
    
    for (j in seq(from=1, to=nrow(neighbors_in_neighborhood))){
      #j=1
      Lat = as.data.frame(coord)$Lat
      Lon = as.data.frame(coord)$Lon
      age = as.data.frame(coord)$age
      
      BNT = ((as.data.frame(coord)$BNT) - (neighbors_in_neighborhood[j,]$BNT))
      BBI = ((as.data.frame(coord)$BBI) - (neighbors_in_neighborhood[j,]$BBI))
      TBT = ((as.data.frame(coord)$TBT) - (neighbors_in_neighborhood[j,]$TBT))
      TBI = ((as.data.frame(coord)$TBI) - (neighbors_in_neighborhood[j,]$TBI))
      TBIR = ((as.data.frame(coord)$TBIR) - (neighbors_in_neighborhood[j,]$TBIR))
      BNI = ((as.data.frame(coord)$BNI) - (neighbors_in_neighborhood[j,]$BNI))
      
      BNT = abs(BNT) 
      BBI = abs(BBI) 
      TBT = abs(TBT) 
      TBI = abs(TBI) 
      TBIR = abs(TBIR) 
      BNI = abs(BNI) 
      
      SSR = sum(BNT, BBI, TBT, TBI, TBIR, BNI)
      
      if (SSR < SSR_test) {
        SSR_test = SSR
        current_resid <- cbind(Lat, Lon, age, BNT, BBI, TBT, TBI, TBIR, BNI)
      }
    }
    resid <- rbind(resid, current_resid)
  }
  resid = cbind(resid, fireprob=rep(fireprob, times=nrow(resid)), fsm=rep(fsm, times=nrow(resid)), grc=rep(grc, times=nrow(resid)))
  return(resid)
}

test <- weighted_residuals_best_neighbor_in_neighborhood_FIA(obs=fia, pred=lai, neighbors=5)

#function to get weighted residuals
get_wighted_best_fit <- function(residuals=resid, obs=y) {
  #residuals=test
  #obs = fia
  #sum all residuals for each pft
  BNT_weighted = sum(residuals$BNT)/sum(obs$BNT)
  BBI_weighted = sum(residuals$BBI)/sum(obs$BBI) 
  TBT_weighted = sum(residuals$TBT)/sum(obs$TBT) 
  TBI_weighted = sum(residuals$TBI)/sum(obs$TBI) 
  TBIR_weighted = sum(residuals$TBIR)/sum(obs$TBIR) 
  BNI_weighted = sum(residuals$BNI)/sum(obs$BNI) 
  
  weighted_fit = sum(BNT_weighted, BBI_weighted, TBT_weighted, TBI_weighted, TBIR_weighted, BNI_weighted)
  
  fireprob = residuals[1,c("fireprob")]
  grc = residuals[1,c("grc")]
  fsm = residuals[1,c("fsm")]
  
  df <- as.data.frame(cbind(fireprob, grc, fsm, weighted_fit))
  
  return(df)
}

get_wighted_best_fit(residuals=test, obs=fia)

