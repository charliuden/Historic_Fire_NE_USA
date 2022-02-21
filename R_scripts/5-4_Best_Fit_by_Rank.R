#January 11 2021
# get best fit based on rank -TBIR is always the least abundant PFT in the lpj-guess output. But the FIA data, when summarised across all points, is the third most abundant. 
#rather than look at squared sum of residualt, I am going to try looking at rank.

# 1. convert the observed and predicted data to rank based on abundance
# 2. for each FIA data point, find rank with closest lpj-gues output in the same simulation year. 
# 3. for each pft, get the absolute differenece in rank between the observed and predicted
# 4. sum all the differences
# 5. lpj-guess output with lowest sum is the winner!

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

#convert FIA dataset to ranked data

fia_rank = fia
for (i in seq(from=1, to=nrow(fia_rank))){
  fia_rank[i,c("BNT","BBI","TBT","TBI","TBIR","BNI")] <- rank(fia_rank[i,c("BNT","BBI","TBT","TBI","TBIR","BNI")], na.last = TRUE, ties.method = c("min"))
}


#function to get residuals of best neighbor
ranked_fit_nearest_neighbor_FIA <- function(obs=x, pred=y) {
  #pred = lai
  #obs = fia_rank

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
    #i=3
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
    nearest_neighbor = as.data.frame(sp.pred.age[1,])
    #convert pred to ranked data
    nearest_neighbor[1,c("BNT","BBI","TBT","TBI","TBIR","BNI")] <- rank(nearest_neighbor[1,c("BNT","BBI","TBT","TBI","TBIR","BNI")], na.last = TRUE,ties.method = c("min"))
    #convert the current fia coordinate to a data frame
    coord = as.data.frame(coord)
    #subtract rank of pred from rank of obs
    BNT= abs(coord$BNT-nearest_neighbor$BNT)
    BBI = abs(coord$BBI-nearest_neighbor$BBI)
    TBT = abs(coord$TBT-nearest_neighbor$TBT)
    TBI = abs(coord$TBI-nearest_neighbor$TBI)
    TBIR = abs(coord$TBIR-nearest_neighbor$TBIR) 
    BNI = abs(coord$BNI-nearest_neighbor$BNI)
    
    current_resid = cbind(BNT, BBI, TBT, TBI, TBIR, BNI)
    
    resid <- rbind(resid, current_resid)
  }
  resid = cbind(resid, fireprob=rep(fireprob, times=nrow(resid)), fsm=rep(fsm, times=nrow(resid)), grc=rep(grc, times=nrow(resid)))
  return(resid)
}

test <- ranked_fit_nearest_neighbor_FIA(obs=fia_rank, pred=lai)

#function to get sum of all ranked residuals
get_sum_ranked_residuals <- function(residuals=resid) {
  
  residuals=test
  sum_ranked_residuals = sum(residuals[,c("BNT","BBI","TBT","TBI","TBIR","BNI")])
  fireprob = residuals[1,c("fireprob")]
  grc = residuals[1,c("grc")]
  fsm = residuals[1,c("fsm")]
  
  df <- as.data.frame(cbind(fireprob, grc, fsm, sum_ranked_residuals))
  
  return(df)
}

get_sum_ranked_residuals(residuals=test)
