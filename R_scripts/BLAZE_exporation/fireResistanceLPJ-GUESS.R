#fire resistance in LPJ-GUESS
library(patchwork) 
library(ggplot)

D <- 5 #tree diameter
R <- seq(0,1, by=0.01) #fire resistance shape parameter (0-1)
P <- 1-(1/(1+(D/R)^1.5)+0.05) #probability of survival

plot(R,P) #trees with a diameter of 5, varying resistance to fire


#to get an idea of values for R, hickler 2004 gives fire resistant trees (white oak, bur (scrub) oak, northern red oak, white pine and red pine) a value of 0.04 for R. All other non fire resistant trees: R=0.07



D <- seq(0,5, length=100) #tree diameter
RS <- 0.07 #fire resistance shape parameter (0-1)
RR <- 0.04
PR <- 1-(1/(1+(D/RR)^1.5)+0.05)
PS <- 1-(1/(1+(D/RS)^1.5)+0.05)
#probability of survival
plot(D,PR, type="l")
lines(D,PS, type="l")#trees with varying diameter, all resistant to fire -once you get to a diameter of less than 1, surival is 1. this seems a little off... I find it hard to believe that a one inch tree, no matter how well adapted it is to fire, would make it through a fire with 1 inch DBH...

#thonicke 2001 (and LPJ-GUESS mabual) says proportion of individuals that die = R-1, where R is fire resistance... much simpler...

#obviously I'm missing something. maybe I have the equation wrong. 
#maybe sapling survival is where resistance to fire is most important. 
#need to find out where hickler got these values for R from. 

library(sensitivity)

#sobols indces quantify importance of variables -which have large impact on output, which have only a little. do inputs interecact, how much?

#------------------------------

#Fire frequency in LPJ-DGVM
#Probability of fire occurrence increases with time (as fuel builds up) . weibull function (continuous probability function -time is continuous, so makes sense) hickler describes S shape for modelling fire -makes sense: has max prob of 1. Exponential increase in probability of fire as time since fire increases (x is ‘time to failure’). 

#Thonicke 2001. Litter moisture is the main driver for day to day fire prob. Fire season length determines fraction of area that burns. 
#Litter must reach minimum temp to ignite and enough litter build up 
#Moisture of extinction: threshold of moisture content in litter/fuel in order for fire to spread. 15-30% in temperate regions.
#Litter (fuel) content threshold: above 200 g/m2 and ignition can occur
#Relationship between length of fire season and area that burns

#---------------------------------------------------------------
#------First, find occurance:-----------------------------------
#---------------------------------------------------------------

#-------1: relationship between predicted (m) and observed/measured (mo) fuel moisture content
  mo <- seq(0, 1, by=0.01) #vector or possible observed fule moisture content
  mp <- 0.4994*mo+1.02 #vector of predicted fule moisture content
  plot(mo,mp)
  
#-------2: Probability of at least one fire occuring in a day in a gird cell

  me <- 0.3 #moisture of extinction (threshold of moisture content in litter/fuel in order for fire to spread. 15-30% in temperate regions) measured as a percent. at the time of the paper, species-specific parameters for flammability were'nt fully incorporated into the model. Thoneick sets woodys to 30%, herbaceous to 20%, but acknowledges that again, it's species specific. 

  e <- exp(1)
  pi <- pi
  
  p <- e^(-(pi)(mo/me)^2)
  p <- e^(-(pi)(mp/me)^2) #dont have mp yet... this would depend on litter build up and env. data
  #so when soil moisture (m) exceeds moisture extinction (me), fire will not ignite
  plot(p, mp)
  
#-------4: annual length of fire season, N. 
  
  N # add days with probability of at least 1 fire in a day (p) over whole year. 

    
  #---------------------------------------------------------------
  #-------Second find effect (spread and size of fire)------------
  #---------------------------------------------------------------

  
#-------6: annual sum of days with particular fire condition, N
  
  N <- 1:365 #vector of possible lengths of fire season, in days. so this is the sum of days that have probability of a least on fire occuring in that day
  s <- N/356 #fractional? length of fire season

#-------9: annual fraction of area burned (fraction of grid cell), A. So A is fucntion of length of the fire season. 
  
  A <- s*e^((s-1)/(0.45*(s-1)^3 + 2.85*(s-1)^2 + 2.96*(s-1) + 1.04)) #fractional area burnt

  plot(N,A)  
  
  #----------------------------------------------------------------
  #------------------Moisture of Extinciton------------------------
  #----------------------------------------------------------------
  
  
#Moisture of Extinction: 
  #Dimitrakopoulos, A. P., & Papaioannou, K. K. (2001). Flammability assessment of Mediterranean forest fuels. Fire Technology. https://doi.org/10.1023/A:1011641601076
  #**this paper is not connected to LPJ-GUESS -I am just using it to understand moisture of extinciton a little more and decide how to come up with values for the PFT 'litterme' 
  
  #y=alpha + beta * x
  #alpha = 
  #y = ignition time
  #x = moisture content (%)
  #beta = 
  
#rank (less, moderately, flammable, extremely) species by similar flammablity (meditreranean species)
  x <- c(0:100, by=1)
  
  #For example: 
  #less flammable species: juniperus oxycedrus (juniper) 
  b <- 0.532
  a <- 16.698
  
  yJuniper <- a+b*x
  
  #flammable speceis: quercus pubescens (valonia oak)
  b <- 0.277
  a <- 11.512

  yQuercus <- a+b*x
  
  
  plot(x,yJuniper, type="l")
  lines(x,yQuercus, type="l")
  
  
  
  #----------------
  #----------------
  #----------------
  #Thonicke 2010 uses this model to calculate fire rate of spread (ROS) 
  #wildland fire modeling
  
  #includes parameters for litter size and live/dead parameter. also includes slope and wind!
  
  #Area Burned----------
  
  Pb <- seq(from=0, to=1, by=0.1)#probability of fire per unit time at any point within the grid cell 
  A <- 1000#grid cell area (ha)
  Ab <- Pb*A#area burned in a grid cell in a day (Ha d-1)
  plot(Pb, Ab, xlab='probability of fire', ylab='area burned (Ha d-1)')
  


  
  Enf <- Enig*FDI #expected number of fires per unit area and time: E(nf)(ha-1 d-1) 
  af <- 0.2 #mean fire area (ha)
  Pb <- Enf*af 
  
  Enig <- nlid + nhig #expected number of ignition events (ie lightning or human caused) per unit area and time (ha-1d-1)
  nlid <- #number of lightning ignition events. thonicke 2010 daily fahs data to number of ignition events
  nhig <- #number of human ignition events
  FID #fire danger index (probability that an ignition even will start a fire)
  
  
  #so area burned is:
  Ab = Enig * FDI * af * A
  
  
  #Ignigtion events----------
  
  nlid #number of lightning ignition events. 
  #thonicke 2010 converts daily flash data to number of ignition events
  
  nhig = pD * kpD * aND/100 #number of human ignition events. (ha-1 d-1)
    #this is a funciton of populaiton density. ignition events decrease as population density decreases.
  
  #-- plot nhig--
  
  kpD = 30 * exp(-0.5 * sqrt(pD)) #exp() is eulers number
  
  pD <- seq(from=1, to=100, by=5)#populaiton density (individuals km-2)
  
  aND0 <- 0 #inclination of people to start fire (ignitions per individual per day) so value of 0.1 is one in ten individuals start a fire each day
              #imprtant for fire in NE project!!!
  aND0.1  <- 0.1 #one in ten
  
  aND0.5 <- 0.5 #1 in 2 individuals will start a fire each day
  
  nhig0 = pD * kpD * aND0/100
  nhig0.1 = pD * kpD * aND0.1/100
  nhig0.5 = pD * kpD * aND0.5/100
  
  nhig0 <- data.frame(cbind(nhig0, pD, aND=rep(0, times=20)))
  names(nhig0) <- c("nhig", "pD", "aND")
  nhig0.1 <- data.frame(cbind(nhig0.1, pD, aND=rep(0.1, times=20)))
  names(nhig0.1) <- c("nhig", "pD", "aND")
  nhig0.5 <- data.frame(cbind(nhig0.5, pD, aND=rep(0.5, times=20)))
  names(nhig0.5) <- c("nhig", "pD", "aND")
  data <- rbind(nhig0, nhig0.1, nhig0.5)
  data$aND <- as.factor(data$aND)
  
  library(ggplot2)
  library(dplyr)
  plot <- ggplot(data=data, mapping=aes(y=nhig, x=pD, group=aND, col=aND)) + xlab("population density (individuals km-2)") + ylab("number of human caused ignitions events (ha-1 d-1)")
  plot + geom_line(aes(colour = aND))
  
  

  
  #Fuel Moisture Content--------------
  
  #Nesteroc Index NI(d)(C^2)
  NId = sum(Tmax * (Tmax - Tdew)) #summation is over the period of consecutive days with precipitation less than 3mm
  
  
  Tmax #daily maximum temperature
  Tdew = Tmin - 4#dew point temperature
  
  Tmin #daily minimum temperature
  
  #Fire Danger----------------
  
  #probability that an ignition event actually turns into a spreading fire. depends on litter moisture, W0
  
  Pspread #probability of fire spread
  
  #Rate of Spread (ROS)------------------
  
  #Rothernal's equation 
  
  ROS = (IR * e * (1 + phW)) / (Pb * e * Qig) #forward rate of spread
  
  IR #reaction intensity -energy release rate per unit area of fire front (kJ m-2 min-1)
  e #(epsilon) propagating flux ratio -proportion of IR that heats adjacent particles to ignition
  phW #(phi) multiplier that accounts for effect of wind. increases effect of e 
  pb #fuel bulk density (kg m-3). assigned by PFT
  Qig #heat of pre ignition, or amount of heat required to ignite a given amount of fuel. (kg kg -1)
  
  Uforward #forward wind speed 0.4 for woody, 0.6 for herbaceous 

  
  #Fractional Combustion and fire intensity--------------
  
  Isurface #intensity of fire at flame front. function of rate of fire spread (ROS), fuel consumption(FCi), fuel classes, and heat content of fuel (h)
  
  
  #Fire Damage to Plants----------------
  
  SH = F * Isurface^0.667#scorch height
  
  Isurface #intensity of fire at flame front
  F #PFT parameter
  
  
  
  #Trace Gas Emissions----------------
  
  #------
  #------
  #-----SIMFIRE------
  #I htink SIMFIRE is the place to play with fire probability: either through the constants, or the Nesterov index. could make fire danger go up or down. Lets see how significanly this will impact fraction of gridcell burned (which is then converted to a fire probability for each day) 
  
  a <- 0.11 #constant, particular to the biome (in this case forest mosaic)
  #a <- 0 #biome=0 is no vegetaiton
  b <- 0.113 #constant
  #b <- seq(0,1, by=0.1)
  c <- 0.860 #constant
  #c <- seq(0,1, by=0.1)
  e <- -0.0168 #constant
  #e <- seq(-1,1, by=0.1)
  E <- exp(1) #eulers number
  f <- 0.6 #seq(0,1, by=0.1)#average annual maximum fPAR (fraction absorbed photosynthetically available radiation) for broad and needleleaved this ould be 06. this is calulated for each patch
  
  #N, nesterov index: from i-W, W is number of days since last rainfall greater than 3 mm
  #lets say W is 3, 3 days since rainfall is greater than 3 mm
  t <- c(20, 40, 18)#, 40)#, 45, 46)#, 32)#temperatures for those 3 days
  D <- c(10, 10, 10)#, 10)#, 10, 10)#, 10)#dew point temperature -water comes out of vapor at this temp if its 1% humitity
  t <- c(20, 40, 18, 20, 40, 18, 20, 40, 18, 20, 40, 18)#, 40)#, 45, 46)#, 32)#temperatures for those 3 days
  D <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  
  #t <- 10
  #D <- 10
  N <- sum((t-D)*t) #maximum nesterov index (fire danger index) between 0 and 4000
        #between 0 and 300: minimal
        #bewteen 301 and 1000: moderate
        #between 1001 and 4000: high
        #above 4000: extreme 
  N <- seq(0, 4000, by=1000)
  
  P <- seq(0, 10, by=1) #human population density, people -ha
  #P <- 0
  #annual burned area:
  BA <- a*f*N*E^(e*P) 
  BA
  
  data <- data.frame(cbind(P, BA))
  
  plot(data$P, data$BA)
  #add column of monthly burned area
  g <- 1000 #gird area, m2
  data <- mutate(data, fBA=BA/g) 
  
  ggplot() + geom_point(data=data, aes(x=P, y=fBA)) + xlab("people -ha") + ylab("annual burned area for a given gridcell") + ggtitle("SIMFIRE")#so, people are putting fires out. 
  
  plot(P,data$fBA, type="l", col="green", lwd=5, xlab="people -ha", ylab="fraction burned area")
  
  #goal would be to have different values for fire probability. 
  
  
  
  
  
  
  
  
  #----BLAZE----
  #Keetch-Byram-Drought-index -a drought index: ranges form 0-800. 0 is no moisture depletion, 800 absolutely dry. 
  
  DSLR <- 100 #number of days since last rainfall 
  E <- exp(1) #eulers number
  T <- 40 #maximum temperature? 
  Pa <- 2.5 #average annual precipitation, mm/d, 'light rain' is < 2.5 mm
  Pd <- 0 #precipitaiton of current day
  KBDI1 <- seq(0,800, by=100) #Keetch-Byram-Drought-index for previous day -800 means it was very dry the day before
    
  #Keetch-Byram-Drought-index -a drought index:
  KBDI <- if(DSLR>0) {
    ((800-KBDI1)*(0.968*E^0.0486*(Tmax*9/5+32)-8.3))/(1+10.88*E^(-0.0441*(Pa)/25.4)*254)
    } else {
      min(0, 5-Pd) #so if it has been no days since rain, then drought index is 0. not sure why this part inlcude precipitation fro that day. 
    }
  KBDI
    
    
    #Keetch-Byram-Drought-index -a drought index
    
  #McArthur drought factor:
  KBDI  #Keetch-Byram-Drought-index -a drought index
  DSLR <- 10#number of days since last rainfall 
  p <- #daily total of the last day with rainfall (mm/d) 
  D <- (0.0191*(KBDI+104)*(DSLR+1)^(3/2))/(3.52*(DSLR+1)^(3/2)+p-1)
    
  RH <- 40 #relative humidity (%) 40% would be dry
  T #<- 20 #temperature in celcius, defined above. 
  U <- 10 #wind speed at 10 meters. 10 meters persecond would be a strong breeze
  
  #McArthur Fire Danger Index:
  FFDI <- 2*exp(-0.45+0.987*log(D+0.001)-0.03456*RH+0.0338*T+0.0234*U)
  
  #Fire rate of spread
  FFDI 
  w <- #amount of fuel ready to burn
  
  Vspread <- 1/3*(FDI*w*10^-5) #m/second
  
  #Fire Line Intensity
  H <- 20 #heat yield of burning fuel, 20 Mj/kg
  #w <- #available guel, kg/m-2
  #Vspread <- #fires rate of spread
  

    
  #-------------------
  # PFT Fire survival Probability
  #-------------------
  
  #TEMPERATE BORADLEAVED 
  
get_survival = function(FLI=x, dbh=y, fire_survival_mod=x){
    
    #FLI=seq(0, 8000, by = 1000) 
    #dbh=10 
    #fire_survival_mod=1.5
    
    #1. get P_survival without using fire_survival_mod:
    
    #get probability of survival at FLI of 3000 kW/m 
    #this only relies on dbh
    P_3000 = 0.95-(1/(1+((dbh/0.7)^1.5)))
    #empty vector:
    P_survival = as.numeric()
    #caluculate survival probability based on actual FLI
    for (i in 1:length(FLI)) {
      fli = FLI[i]
      if (fli >= 7000) {
        p_survival = 0.001
      } else if (fli >= 3000 & fli < 7000) {
        p_survival = P_3000*(1-((fli-3000)/4000))
      } else {
        p_survival = exp((fli/3000) * log(P_3000))
      }
      
      P_survival = c(P_survival, p_survival)
    }
    
    #2. get P_survival with fire_survival_mod: added after finding P_3000:
    
    P_3000 = (0.95-(1/(1+((dbh/0.7)^1.5)))) * fire_survival_mod
    P_survival_mod_3000 = as.numeric()
    for (i in 1:length(FLI)) {
      fli = FLI[i]
      if (fli >= 7000) {
        p_survival = 0.001
      } else if (fli >= 3000 & fli < 7000) {
        p_survival = P_3000*(1-((fli-3000)/4000))
      } else {
        p_survival = exp((fli/3000) * log(P_3000))
      }
      
      P_survival_mod_3000 = c(P_survival_mod_3000, p_survival)
    }
    
    #3. get P_survival with fire_survival_mod added after finding final P_survival:
    
    P_3000 = 0.95-(1/(1+((dbh/0.7)^1.5)))
    P_survival_mod = as.numeric()
    for (i in 1:length(FLI)) {
      fli = FLI[i]
      if (fli >= 7000) {
        p_survival = 0.001
      } else if (fli >= 3000 & fli < 7000) {
        p_survival = P_3000*(1-((fli-3000)/4000))
      } else {
        p_survival = exp((fli/3000) * log(P_3000))
      }
      p_survival = p_survival * fire_survival_mod
      P_survival_mod = c(P_survival_mod, p_survival)
    }
    
    df = cbind(FLI, P_survival, P_survival_mod_3000, P_survival_mod, dbh=rep(dbh, length(FLI)), fire_survival_mod=rep(fire_survival_mod, length(FLI)))
    return(df)
}

 

df_1.5 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1.5)
df_1.3 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1.3)
df_1.1 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1.1)
df_1 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1)
df_1.7 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1.7)
df_1.9 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1.9)
df_2 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=2)
df_5 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=5)
df_10 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=10)
df_15 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=15)
df_20 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=20)
df_25 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=25)



#df1 = data.frame(rbind(df_1, df_1.1, df_1.3, df_1.5, df_1.7, df_1.9, df_2, df_5, df_10, df_15, df_20, df_25))
#df2 = data.frame(rbind(df_1, df_1.1, df_1.3, df_1.5, df_1.7, df_1.9, df_2, df_5, df_10, df_15, df_20, df_25))
#df3 = data.frame(rbind(df_1, df_1.1, df_1.3, df_1.5, df_1.7, df_1.9, df_2, df_5, df_10, df_15, df_20, df_25))
df4 = data.frame(rbind(df_1, df_1.1, df_1.3, df_1.5, df_1.7, df_1.9, df_2, df_5, df_10, df_15, df_20, df_25))

df = rbind(df1, df2, df3, df4)

head(df)
df_test <- df
#make survival probabilities greater than equal 1
df_test[df_test$P_survival_mod_3000 > 1, 3] <- 1
df_test[df_test$P_survival_mod > 1, 4] <- 1

a = ggplot() + geom_point(data=df, aes(y=P_survival, x=FLI)) + ggtitle("dbh=2")

b = ggplot() + geom_point(data=df, aes(y=P_survival_mod_3000, x=FLI, color = as.factor(fire_survival_mod)), show.legend = FALSE) + ggtitle("After P_survival(3000)")

c = ggplot() + geom_point(data=df, aes(y=P_survival_mod, x=FLI, color = as.factor(fire_survival_mod))) + ggtitle("After P_survival") + guides(color=guide_legend(title="fire_survival_mod"))

a + b + c



# look at dbh, fire_survival_mod and survival probability:

p1 <- ggplot() + geom_point(data=filter(df_test, dbh==2), aes(y=P_survival, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("P_survival, dbh=2") + theme(legend.position = "none") 

p2 <- ggplot() + geom_point(data=filter(df_test, dbh==2), aes(y=P_survival_mod_3000, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("FSM before P_survival(3000), dbh=2") + theme(legend.position = "none")

p3 <- ggplot() + geom_point(data=filter(df_test, dbh==2), aes(y=P_survival_mod, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("FSM after P_survival(3000), dbh=2") + theme(legend.position = "none")

p4 <- ggplot() + geom_point(data=filter(df_test, dbh==5), aes(y=P_survival, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("P_survival, dbh=5") + theme(legend.position = "none") 

p5 <- ggplot() + geom_point(data=filter(df_test, dbh==5), aes(y=P_survival_mod_3000, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("FSM before P_survival(3000), dbh=5") + theme(legend.position = "none")

p6 <- ggplot() + geom_point(data=filter(df_test, dbh==5), aes(y=P_survival_mod, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("FSM after P_survival(3000), dbh=10") + theme(legend.position = "none")

p7 <- ggplot() + geom_point(data=filter(df_test, dbh==10), aes(y=P_survival, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("P_survival, dbh=10") + theme(legend.position = "none") 

p8 <- ggplot() + geom_point(data=filter(df_test, dbh==10), aes(y=P_survival_mod_3000, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("FSM before P_survival(3000), dbh=10") + theme(legend.position = "none")

p9 <- ggplot() + geom_point(data=filter(df_test, dbh==10), aes(y=P_survival_mod, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("FSM after P_survival(3000), dbh=10") + theme(legend.position = "none")

p10 <- ggplot() + geom_point(data=filter(df_test, dbh==20), aes(y=P_survival, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("P_survival, dbh=20") + theme(legend.position = "none") 

p11<- ggplot() + geom_point(data=filter(df_test, dbh==20), aes(y=P_survival_mod_3000, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("FSM before P_survival(3000), dbh=20") + theme(legend.position = "none")

p12<- ggplot() + geom_point(data=filter(df_test, dbh==20), aes(y=P_survival_mod, x=FLI, color =as.factor(fire_survival_mod))) + ggtitle("FSM after P_survival(3000), dbh=20") + theme(legend.position = "none")

(p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12)

p1 = ggplot() + geom_point(data=df, mapping=aes(x=dbh, y=P_survival_mod, color=as.factor(fire_survival_mod)))

p2 = ggplot() + geom_point(data=df, mapping=aes(x=dbh, y=P_survival_mod_3000, color=as.factor(fire_survival_mod)))

p1+p2

ggplot() + geom_point(data=filter(df, fire_survival_mod==1), mapping=aes(x=dbh, y=P_survival, color=as.factor(fire_survival_mod)))

ggplot() + geom_point(data=df, mapping=aes(x=dbh, y=P_survival, color=as.factor(FLI)))

ggplot() + geom_point(data=df_test, aes(x=FLI, y=P_survival_mod_3000, col=as.factor(dbh)))

# how does fsm of 2 react to fire?
df_5 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=5, fire_survival_mod=2)
df_10 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=10, fire_survival_mod=2)
df_15 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=15, fire_survival_mod=2)
df_20 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=2)
df_25 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=25, fire_survival_mod=2)
df_30 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=30, fire_survival_mod=2)

df = as.data.frame(rbind(df_5, df_10, df_15, df_20, df_25, df_30))

p1 <- ggplot() + geom_point(data=df, aes(x=FLI, y=P_survival_mod_3000, col=as.factor(dbh))) + ggtitle("fire_survival_mod = 2") + ylab("P(survival)") + ylab("Fire Line Intensity")+ theme(legend.position = "none") 

# how does fsm of 0.4 react to fire?
df_5 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=5, fire_survival_mod=1.4)
df_10 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=10, fire_survival_mod=1.4)
df_15 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=15, fire_survival_mod=1.4)
df_20 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1.4)
df_25 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=25, fire_survival_mod=1.4)
df_30 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=30, fire_survival_mod=1.4)

df = as.data.frame(rbind(df_5, df_10, df_15, df_20, df_25, df_30))

p2 <- ggplot() + geom_point(data=df, aes(x=FLI, y=P_survival_mod_3000, col=as.factor(dbh))) + ggtitle("fire_survival_mod = 1.4") + ylab("P(survival)") + ylab("Fire Line Intensity")+ ylim(0, 1.5)+ theme(legend.position = "none") 


# how does fsm of 0.1 react to fire?
df_5 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=5, fire_survival_mod=1.1)
df_10 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=10, fire_survival_mod=1.1)
df_15 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=15, fire_survival_mod=1.1)
df_20 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1.1)
df_25 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=25, fire_survival_mod=1.1)
df_30 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=30, fire_survival_mod=1.1)

df = as.data.frame(rbind(df_5, df_10, df_15, df_20, df_25, df_30))

p3 <- ggplot() + geom_point(data=df, aes(x=FLI, y=P_survival_mod_3000, col=as.factor(dbh))) + ggtitle("fire_survival_mod = 1.1") + ylab("P(survival)") + ylab("Fire Line Intensity") + ylim(0, 1.5) + theme(legend.position = "none") 

# how does fsm of 0.1 react to fire?
df_5 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=5, fire_survival_mod=1)
df_10 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=10, fire_survival_mod=1)
df_15 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=15, fire_survival_mod=1)
df_20 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=20, fire_survival_mod=1)
df_25 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=25, fire_survival_mod=1)
df_30 = get_survival(FLI=seq(0, 8000, by = 1000) , dbh=30, fire_survival_mod=1)

df = as.data.frame(rbind(df_5, df_10, df_15, df_20, df_25, df_30))

p4 <- ggplot() + geom_point(data=df, aes(x=FLI, y=P_survival, col=as.factor(dbh))) + ggtitle("fire_survival_mod = 1 (TBI)") + ylab("P(survival)") + ylab("Fire Line Intensity") + ylim(0, 1.5)

p1 | p2 | p3 | p4

