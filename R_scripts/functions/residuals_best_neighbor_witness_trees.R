# funciton to find residuals for each pft at each witness tree point. 
# inputs:
    #obs = witness tree data, must be sorted into PFTs and each values must be a proportion of the total
    #pred = LPJ-GUESS output. 

residuals_best_neighbor_witness_trees <- function(obs=x, pred=y, neighbors=z) {
  # for testing the function:
  #obs = wit
  #pred = lai
  #neighbors = 5
  
  #standardise pred
  pred <- mutate(pred, total=BNT+BBI+TBT+TBI+TBIR+BNI)
  pred <- mutate(pred, BNT=BNT/total,BBI=BBI/total,TBT=TBT/total,TBI=TBI/total,TBIR=TBIR/total,BNI=BNI/total)
  pred[is.na(pred)] <- 0
  
  # 1. make obs and pred spatial objects
  sp.obs <- obs#obs
  sp.pred <- pred#pred
  coordinates(sp.obs) <- ~Lon+Lat
  coordinates(sp.pred) <- ~Lon+Lat
  
  #2. make an empty data frame to hold the residuals
  resid <- data.frame(Lat=as.numeric(), 
                      Lon=as.numeric(), 
                      BNT=as.numeric(),
                      BBI=as.numeric(),
                      TBT=as.numeric(),
                      TBI=as.numeric(),
                      TBIR=as.numeric(),
                      BNI=as.numeric())
  
  # 3. vector of unique lpj-guess coordinates to loop though, make it a spatial object
  coords = unique(pred[c("Lat", "Lon")])
  coordinates(coords) <- ~Lon+Lat
  #4. take each coordinate in lpj guess 
  #nrow(as.data.frame(coords))) # there area 123 coodinates
  for (i in seq(from=1, to=123)){
    #i = 13
    coord = coords[i,]
    # 5. find the five closest witness tree points
    distances = pointDistance(coord, sp.obs, lonlat=TRUE, allpairs=FALSE)
    # 6. add distances to the predicted sp dataframe and plot to make sure it worked
    sp.obs$distances <- c(distances)
    #plot to check that distances have been correctly added:
    #ggplot() + geom_point(data=as.data.frame(sp.obs), aes(x=Lon, y=Lat, size=distances))
    
    # 7. get the minimum n distances and their associated coordinates and variables 
    #(n=neighbors as input for this function)
    obs_neighbors = as.data.frame(sp.obs) %>% arrange(distances)
    neighbors_in_neighborhood = obs_neighbors[1:neighbors,]
    # 8. for each witness tree of the 5 points (neighbors), 
    # take the year interval
    # filter lpj-guess for that interval (just the current coord we are working with)
    # calculate residuals for each pft, sum them and square them 
    # if SSR is greater than previously calculated ssr, replace 'SSR with that 'best_fit' with that row
    
    # something to hold the smallest SSR and something to hold residuals and coords for current lowest ssr
    SSR_smallest = 1000000000
    current_resid = as.numeric()
    
    for (j in seq(from=1, to=neighbors)){
      #j = 1
      start = neighbors_in_neighborhood[j,11]
      end = neighbors_in_neighborhood[j,12]
      #filter pred for current coordinate point
      pred_current = filter(as.data.frame(sp.pred), Lon==as.data.frame(coord)[,1] & Lat==as.data.frame(coord)[,2])
      #filter pred for start and end years
      pred_current = filter(pred_current, between(year, start, end)) 
      #get residuals
      BNT = (neighbors_in_neighborhood[j,]$BNT-mean(as.data.frame(pred_interval)$BNT))
      BBI = (neighbors_in_neighborhood[j,]$BBI-mean(as.data.frame(pred_interval)$BBI))
      TBT = (neighbors_in_neighborhood[j,]$TBT-mean(as.data.frame(pred_interval)$TBT))
      TBI = (neighbors_in_neighborhood[j,]$TBI-mean(as.data.frame(pred_interval)$TBI))
      TBIR = (neighbors_in_neighborhood[j,]$TBIR-mean(as.data.frame(pred_interval)$TBIR))
      BNI = (neighbors_in_neighborhood[j,]$BNI-mean(as.data.frame(pred_interval)$BNI))
      #find squared sum of residuals
      SSR = sum(BNT, BBI, TBT, TBI, TBIR, BNI)^2
      
      if (SSR < SSR_smallest) {
        SSR_smallest = SSR
        current_resid = cbind(Lat = as.data.frame(coord)[,2], Lon = as.data.frame(coord)[,1], BNT, BBI, TBT, TBI, TBIR, BNI, SSR) 
      }
    }
    # after looping through neighbors, as neighbor with lowest SSR to resid (lat, lon, residuals for each PFT and SSR for that grid point):
    resid <- rbind(resid, current_resid)
  }
  return(resid)
}