
#Funciton to find residuals of nearest neighbor
#input: 
    #obs = observed data (Forest inventory and analysis. must be sorted into PFTs with values for each as a proportion of the total
    #pred = predicted data (LPJ-GUESS output as leaf area index). must inlude fire proability and values for grc (grwoth_resp_cost) and fsm (fire_survival_mod).

residuals_nearest_neighbor_FIA <- function(obs=x, pred=y) {
  
  #standardise pred
  pred <- mutate(pred, year=year-1809)
  pred <- mutate(pred, total=BNT+BBI+TBT+TBI+TBIR+BNI)
  pred <- mutate(pred, BNT=BNT/total,BBI=BBI/total,TBT=TBT/total,TBI=TBI/total,TBIR=TBIR/total,BNI=BNI/total)
  pred[is.na(pred)] <- 0
  
  # 1. make obs and pred spatial objects
  sp.obs <- obs
  sp.pred <- pred
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
    distances <- pointDistance(coord, sp.pred.age, lonlat=TRUE, allpairs=FALSE)
    # 5. add distances to the predicted sp dataframe and plot to make sure it worked
    sp.pred.age$distances <- c(distances)
    #ggplot() + geom_point(data=as.data.frame(sp.pred.age), aes(x=Lon, y=Lat, size=distances))
    #get the minimum distance and its associated coordinates and attributes
    min_dist <- min(distances)
    nearest_neighbor = filter(as.data.frame(sp.pred.age), distances==min_dist)
    #get residual for each PFT and put it into a dataframe
    Lat = as.data.frame(coord)$Lat
    Lon = as.data.frame(coord)$Lon
    age = as.data.frame(coord)$age
    BNT = as.data.frame(coord)$BNT - nearest_neighbor$BNT
    BBI = as.data.frame(coord)$BBI - nearest_neighbor$BBI
    TBT = as.data.frame(coord)$TBT - nearest_neighbor$TBT
    TBI = as.data.frame(coord)$TBI - nearest_neighbor$TBI
    TBIR = as.data.frame(coord)$TBIR - nearest_neighbor$TBIR
    BNI = as.data.frame(coord)$BNI - nearest_neighbor$BNI
    #resid[i,] <- cbind(Lat, Lon, age, BNT, BBI, TBT, TBI, TBIR, BNI)
    resid <- rbind(resid, cbind(Lat, Lon, age, BNT, BBI, TBT, TBI, TBIR, BNI))
  }
  
  return(resid)
}