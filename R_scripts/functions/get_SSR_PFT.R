#function to find Squared Sum of REsiguals for each lpj-guess outptu. 

#iputs: residuals= output from either residuals_best_neighbor_in_neighborhood_FIA() or residuals_nearest_neighbor_FIA(). -both funcitons find the residuals for each PFT at each fia coordinate. 
# output is the SSR for that lpj-guess run 
get_SSR_PFT <- function(residuals=x){
  BNT = sum(residuals$BNT)^2
  BBI = sum(residuals$BBI)^2
  TBT = sum(residuals$TBT)^2
  TBI = sum(residuals$TBI)^2
  TBIR = sum(residuals$TBIR)^2
  BNI = sum(residuals$BNI)^2
  all_PFT = sum(BNT, BBI, TBT, TBI, TBIR, BNI)^2
  SSR <- data.frame(BNT=BNT, BBI=BBI, TBT=TBT, TBI=TBI, TBIR=TBIR, BNI=BNI, all_PFT=all_PFT)
  return(SSR)
}