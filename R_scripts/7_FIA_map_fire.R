

#Let's get a better look at that 200 year run with the lowest squared sum of residuals:

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
source("../R_scripts/functions/cleanup.R")
fire0 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-singledist-grc-07-fsm-04/lai.out"),fireprob=0, distinterval=1, gcr=-0.7, fsm=1.4) 
fire1 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire1-singledist-grc-07-fsm-04/lai.out"),fireprob=1, distinterval=1, gcr=-0.7, fsm=1.4) 
fire0.1 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-1-singledist-grc-07-fsm-04/lai.out"),fireprob=0.1, distinterval=1, gcr=-0.7, fsm=1.4) 
fire0.01 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-01-singledist-grc-07-fsm-04/lai.out"),fireprob=0.01, distinterval=1, gcr=-0.7, fsm=1.4) 
fire0.02 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-02-singledist-grc-07-fsm-04/lai.out"),fireprob=0.02, distinterval=1, gcr=-0.7, fsm=1.4) 
fire0.05 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-05-singledist-grc-07-fsm-04/lai.out"),fireprob=0.05, distinterval=1, gcr=-0.7, fsm=1.4) 
fire0.001 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-001-singledist-grc-07-fsm-04/lai.out"),fireprob=0.001, distinterval=1, gcr=-0.7, fsm=1.4) 
fire0.002 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-002-singledist-grc-07-fsm-04/lai.out"),fireprob=0.002, distinterval=1, gcr=-0.7, fsm=1.4) 
fire0.005 = cleanup(lai=read.table("../data/LPJ-GUESS_output/fire0-005-singledist-grc-07-fsm-04/lai.out"),fireprob=0.005, distinterval=1, gcr=-0.7, fsm=1.4) 
head(fire0.05)
#------
#read in FIA tree data
fia <- read.csv("../data/FIA/FIA_PFTs.csv")[,-1]
head(fia)


#-------
#LAI by time, FIA by time
df_melt <- melt(data = fire0.05, id.vars = c("year", "fireprob","fsm", "grc", "Lat", "Lon"), measure.vars = c("BNT","BNI","TBT","BBI","TBI","TBIR"))
df_melt[is.na(df_melt)] <- 0
names(df_melt) <- c("year", "fire_prob", "fsm", "grc", "Lat", "Lon", "PFT", "mean_lai")
head(df_melt)
ggplot() + geom_point(data=filter(df_melt, Lat==42.75, Lon==-73.75), aes(x=year, y=mean_lai, col=PFT, alpha=0.1)) + scale_color_manual(values=palette)
ggplot() + geom_point(data=filter(df_melt, Lat==46.25, Lon==-69.25), aes(x=year, y=mean_lai, col=PFT, alpha=0.1)) + scale_color_manual(values=palette)                                           
                                           
#-------              
# Map the FIA fire probabilities that fit best:

get_SSR_byLPJpoint <- function(pred=x, obs=y, neighbors=z){
  #pred=fire0
  #obs=fia
  #neighbors=5
  #
  #becuase lpj-guess simulation is 200 years long, only want stand ages less than 200
  obs = filter(obs, age<200)
  #get rid of rows with no trees:
  obs = filter(obs, total>0)
  
  #get fire probability, grc adn fsm
  fireprob = pred[1,c("fireprob")]
  grc = pred[1,c("grc")]
  fsm = pred[1,c("fsm")]
  #standardise pred
  pred <- mutate(pred, total=BNT+BBI+TBT+TBI+TBIR+BNI)
  pred <- mutate(pred, BNT=BNT/total,BBI=BBI/total,TBT=TBT/total,TBI=TBI/total,TBIR=TBIR/total,BNI=BNI/total)
  pred[is.na(pred)] <- 0
  pred <- mutate(pred, year=year-1809)
  
  # 1. make obs and pred spatial objects
  sp.obs <- obs#obs
  sp.pred <- pred#pred
  coordinates(sp.obs) <- ~Lon+Lat
  coordinates(sp.pred) <- ~Lon+Lat
  
  #2. make an empty data frame to hold the squared sum of residuals
  ssr <- data.frame(Lat=as.numeric(), 
                    Lon=as.numeric(), 
                    BNT=as.numeric(),
                    BBI=as.numeric(),
                    TBT=as.numeric(),
                    TBI=as.numeric(),
                    TBIR=as.numeric(),
                    BNI=as.numeric(),
                    SSR=as.numeric())
  #get lpj-guess coordinates
  coords = unique(pred[,c("Lat","Lon")])
  coordinates(coords) <- ~Lon+Lat
  # 3. for loop to take each lpj-guess coordinate
  for (i in seq(from=1, to=123)){
    #i=14
    coord = coords[i,]
    # 2. find the n closest neighbors
    distances = pointDistance(coord, sp.obs, lonlat=TRUE, allpairs=FALSE)
    obs_distances = as.data.frame(sp.obs)
    obs_distances$distances <- distances
    obs_distances = obs_distances %>% arrange(distances)
    neighbors_in_neighborhood = obs_distances[1:neighbors,]
    # 3. filter for lpj-guess data points that are the same simulation year as the stand ages of those five points
    ages = c(neighbors_in_neighborhood$age)
    pred.age = filter(pred, Lat==as.data.frame(coord)[,2], Lon==as.data.frame(coord)[,1], (year==ages[1] | year==ages[2] | year==ages[3] | year==ages[4] | year==ages[5]))
    # 4. find which neighbor in the neighborhood has the lowest SSR
    # something to hold the smallest SSR and something to hold residuals and coords for current lowest ssr
    SSR_smallest = 1000000000
    current_resid = as.numeric()
    
    for (j in seq(from=1, to=length(ages), by=1)){
      #j = 1
      #filter both data sets for j age in ages
      neighbor = filter(neighbors_in_neighborhood, age==ages[j])
      pred_point = filter(pred.age, year==ages[j])
      #get residuals
      BNT = (neighbor$BNT-pred_point$BNT)^2
      BBI = (neighbor$BBI-pred_point$BBI)^2
      TBT = (neighbor$TBT-pred_point$TBT)^2
      TBI = (neighbor$TBI-pred_point$TBI)^2
      TBIR = (neighbor$TBIR-pred_point$TBIR)^2
      BNI = (neighbor$BNI-pred_point$BNI)^2
      #find squared sum of residuals
      SSR = BNT+BBI+TBT+TBI+TBIR+BNI
      
      if (SSR < SSR_smallest) {
        SSR_smallest = SSR
        current_resid = cbind(Lat = as.data.frame(coord)[,2], Lon = as.data.frame(coord)[,1], BNT, BBI, TBT, TBI, TBIR, BNI, SSR) 
      }
    }
    # after looping through neighbors, add neighbor with lowest SSR to ssr dataframe (lat, lon, residuals for each PFT and SSR for that grid point):
    ssr <- rbind(ssr, current_resid)
  }
  ssr = cbind(ssr, fireprob=rep(fireprob, times=nrow(ssr)), fsm=rep(fsm, times=nrow(ssr)), grc=rep(grc, times=nrow(ssr)))
  
  return(ssr)
}

ssr1 = get_SSR_byLPJpoint(pred=fire0, obs=fia, neighbors=5)
ssr2 = get_SSR_byLPJpoint(pred=fire1, obs=fia, neighbors=5)
ssr3 = get_SSR_byLPJpoint(pred=fire0.1, obs=fia, neighbors=5)
ssr4 = get_SSR_byLPJpoint(pred=fire0.01, obs=fia, neighbors=5)
ssr5 = get_SSR_byLPJpoint(pred=fire0.02, obs=fia, neighbors=5)
ssr6 = get_SSR_byLPJpoint(pred=fire0.05, obs=fia, neighbors=5)
ssr7 = get_SSR_byLPJpoint(pred=fire0.001, obs=fia, neighbors=5)
ssr8 = get_SSR_byLPJpoint(pred=fire0.002, obs=fia, neighbors=5)
ssr9 = get_SSR_byLPJpoint(pred=fire0.005, obs=fia, neighbors=5)

fia_fire = rbind(ssr1, ssr2, ssr3, ssr4, ssr5, ssr6, ssr7, ssr8, ssr9)

df <- data.frame(Lat=as.numeric(),
                 Lon=as.numeric(),
                 BNT=as.numeric(), 
                 BBI=as.numeric(),
                 TBT=as.numeric(),
                 TBI=as.numeric(),
                 TBIR=as.numeric(),
                 BNI=as.numeric(), 
                 SSR=as.numeric(),
                 fire=as.numeric())
#for each unique coordinate, find lowest SSR 
coords = unique(fia_fire[,1:2])
for (i in seq(1, nrow(coords))){
  #i=10
  coord = coords[i,]
  x = filter(fia_fire, Lat==coord[1,1] & Lon==coord[1,2])
  x = filter(x, SSR==min(x$SSR))
  #make sure there arent runs with the same SSR 
  #if (nrow(x) > 1) {
    #filter(x, fireprob==min(x$fireprob))
  #}
  #add coordinate and its associated fire probability and SSR to a data frame 
  df = rbind(df, x)
}

df = mutate(df, fire_as_factor = as.factor(fireprob))
# plot the results
ggplot() + geom_point(data=df, aes(x=Lon, y=Lat, size=fireprob, col=fire_as_factor, alpha=0.1)) + scale_size('fire probability', breaks=c(1, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0), range=c(0,20)) + ggtitle("FIA Fire Best Fit")











                             
                                           
                                           