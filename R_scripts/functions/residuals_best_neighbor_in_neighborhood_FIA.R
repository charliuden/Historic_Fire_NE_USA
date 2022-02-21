#Funciton to find residuals of best of n neighbors in neighborhood
#input: 
    #obs = observed data (Forest inventory and analysis. must be sorted into PFTs with values for each as a proportion of the total
    #pred = predicted data (LPJ-GUESS output as leaf area index). must inlude fire proability and values for grc (grwoth_resp_cost) and fsm (fire_survival_mod).
    #neighbors = number of neighbors to search from 
residuals_best_neighbor_in_neighborhood_FIA <- function(obs=x, pred=y, neighbors=z) {
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
  for (i in seq(from=1, to=nrow(fia_pft))){
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
      Lat = as.data.frame(coord)$Lat
      Lon = as.data.frame(coord)$Lon
      age = as.data.frame(coord)$age
      BNT = as.data.frame(coord)$BNT - neighbors_in_neighborhood[j,]$BNT
      BBI = as.data.frame(coord)$BBI - neighbors_in_neighborhood[j,]$BBI
      TBT = as.data.frame(coord)$TBT - neighbors_in_neighborhood[j,]$TBT
      TBI = as.data.frame(coord)$TBI - neighbors_in_neighborhood[j,]$TBI
      TBIR = as.data.frame(coord)$TBIR - neighbors_in_neighborhood[j,]$TBIR
      BNI = as.data.frame(coord)$BNI - neighbors_in_neighborhood[j,]$BNI
      
      SSR = sum(BNT, BBI, TBT, TBI, TBIR, BNI)^2
      
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