#February 9 2021
#Script to look at fire survival for each PFT
#Survival is found after Fire Line Intensity. 
#PFT mortality is then used to find carbin fluxes

library(ggplot2)
library(dplyr)
library(reshape2)
library(patchwork)



fli=seq(0, 8000, by = 1000) #fire line intensity, kw/m
e = exp(1)#eulers number
mcwd = 1 #mass of coarse woody debris (Mg/ha)
dbh_list=seq(5, 30, 5)

df <- data.frame(p_survival_BN=as.numeric(), 
                 p_survival_TN=as.numeric(), 
                 P_survival_TB=as.numeric(),
                 FLI=as.numeric(),
                 DBH=as.numeric())

for (dbh in dbh_list){
  
  ##------ BOREAL NEADLELEAVED --------##
  #For trees at latitudes beyond 50 degrees N/S (|latitude|>50) according to Dalziel et al 2008:
  
  p_survival_BN = e^(-fli/500)
  
  ##------ TEMPERATE NEADLELEAVED --------##
  #All other needle-leafed trees have survival-probabilities following to Kobziar et al 2006. Where dbh is diameter-at-breast-height [cm]; mcwd the mass of coarse woody debris [Mg/ha].
  
  p_survival_TN = 1 - (1+e^-(1.0337+0.000151*fli-0.221*dbh+0.219*mcwd))^-1
  
  ##------ TEMPERATE BROADLEAVED --------##
  #Extra-tropical non-savannah broadleaved trees are considered to be temperate.
  #Their survival probability is calculated following Hickler et al. 2004 using the resilience value res to distinguish between seeders and resprouters -this is no longer used in BLAZE
  
  #get probability of survival at FLI of 3000 kW/m 
  #this only relies on dbh
  P_3000 = 0.95-(1/(1+((dbh/0.7)^1.5)))
  
  p_survival_TB = as.numeric()
  #caluculate survival probability based on actual FLI
  for (i in 1:length(fli)) {
    FLI = fli[i]
    if (FLI >= 7000) {
      p_survival = 0.001
    } else if (FLI >= 3000 & FLI < 7000) {
      p_survival = P_3000*(1-((FLI-3000)/4000))
    } else {
      p_survival = exp((FLI/3000) * log(P_3000))
    }
    
    p_survival_TB = c(p_survival_TB, p_survival)
  }

  DF <- cbind(p_survival_BN, p_survival_TN, p_survival_TB, fli, DBH=rep(dbh, length(p_survival_BN)))
  names(DF) <- c("p_survival_BN", "p_survival_TN", "p_survival_TB", "FLI", "DBH")
  df <- rbind(DF, df)
}

df <- melt(data = df, id.vars = c("fli", "DBH"), measure.vars = c("p_survival_BN","p_survival_TN", "p_survival_TB"))
df[is.na(df)] <- 0
names(df) <- c("FLI", "DBH", "PFT", "p_survival")


ggplot() + geom_point(data=df, aes(x=FLI, y=p_survival, color=PFT))

df_test = filter(df, FLI==3000 & PFT=="p_survival_TN")
df_test = rbind(df_test, filter(df, FLI==3000 & PFT=="p_survival_TB"))
ggplot() + geom_point(data=df_test, aes(x=DBH, y=p_survival, color=PFT)) + ggtitle("fire line intensity = 3000 kw/m")


# So how can I change the p_survival function for temperate broadleaf trees to make DBH respons more to FLI


#-----
#BLAZE 
#-----
dbh_list=seq(5, 30, 5)
fli=seq(0, 8000, by = 1000)

df <- data.frame(P_survival_TB=as.numeric(),
                 FLI=as.numeric(),
                 DBH=as.numeric())

for (i in 1:length(dbh_list)){
  dbh = dbh_list[i]
  P_3000 = 0.95-(1/(1+((dbh/0.7)^1.5)))
#caluculate survival probability based on actual FLI
  p_survival_TB = as.numeric()
  for (j in 1:length(fli)) {
    FLI = fli[j]
    if (FLI >= 7000) {
      p_survival = 0.001
    } else if (FLI >= 3000 & FLI < 7000) {
      p_survival = P_3000*(1-((FLI-3000)/4000))
    } else {
      p_survival = exp((FLI/3000) * log(P_3000))
    }
    
    p_survival_TB = c(p_survival_TB, p_survival)
  }
  x = as.data.frame(cbind(p_survival_TB, FLI=fli, DBH=rep(dbh, length(p_survival_TB))))
  df = rbind(df, x)
}
#-----

#-----
#BLAZE with seeders/sprouters
#-----
dbh_list=seq(5, 30, 5)
fli=seq(0, 8000, by = 1000)

seeder = 0.1
sprouter = 3

res = sprouter

df <- data.frame(P_survival_TB=as.numeric(),
                 FLI=as.numeric(),
                 DBH=as.numeric())

for (i in 1:length(dbh_list)){
  dbh = dbh_list[i]
  P_3000 = 0.95-(1/(1+((dbh/res)^1.5)))
  #caluculate survival probability based on actual FLI
  p_survival_TB = as.numeric()
  for (j in 1:length(fli)) {
    FLI = fli[j]
    if (FLI >= 7000) {
      p_survival = 0.001
    } else if (FLI >= 3000 & FLI < 7000) {
      p_survival = P_3000*(1-((FLI-3000)/4000))
    } else {
      p_survival = exp((FLI/3000) * log(P_3000))
    }
    
    p_survival_TB = c(p_survival_TB, p_survival)
  }
  x = as.data.frame(cbind(p_survival_TB, FLI=fli, DBH=rep(dbh, length(p_survival_TB))))
  df = rbind(df, x)
}

df_seeder = cbind(df, tactic = rep("seeder", nrow(df)))
df_sprouter = cbind(df, tactic = rep("sprouter", nrow(df)))
df = rbind(df_seeder, df_sprouter)

ggplot() + geom_line(data=filter(df, FLI==3000), aes(x=DBH, y=p_survival_TB, color=tactic))


p1 = ggplot() + geom_line(data=filter(df, DBH==5), aes(x=FLI, y=p_survival_TB, color=tactic)) + ggtitle("DBH=5")
p2 = ggplot() + geom_line(data=filter(df, DBH==15), aes(x=FLI, y=p_survival_TB, color=tactic)) + ggtitle("DBH=15")
p3 = ggplot() + geom_line(data=filter(df, DBH==30), aes(x=FLI, y=p_survival_TB, color=tactic)) + ggtitle("DBH=30")
p1 | p2 | p3

#-----

#-----
#BLAZE modify for dbh 
#-----
dbh_list=seq(5, 30, 5)
fli=seq(0, 8000, by = 1000)

df <- data.frame(P_survival_TB=as.numeric(),
                 FLI=as.numeric(),
                 DBH=as.numeric())

#parameter to change
dbh_modifier = 0.7
dbh_modifier = 2

for (i in 1:length(dbh_list)){
  dbh = dbh_list[i]
  P_3000 = 0.95-(1/(1+((dbh/dbh_modifier)^1.5)))
  #caluculate survival probability based on actual FLI
  p_survival_TB = as.numeric()
  for (j in 1:length(fli)) {
    FLI = fli[j]
    if (FLI >= 7000) {
      p_survival = 0.001
    } else if (FLI >= 3000 & FLI < 7000) {
      p_survival = P_3000*(1-((FLI-3000)/4000))
    } else {
      p_survival = exp((FLI/3000) * log(P_3000))
    }
    
    p_survival_TB = c(p_survival_TB, p_survival)
  }
  x = as.data.frame(cbind(p_survival_TB, FLI=fli, DBH=rep(dbh, length(p_survival_TB))))
  df = rbind(df, x)
}
#-----


#--------------
# litterme  - litter moisture flammability threshold
#           - relic of old fire model
#           - value between 0 and 1, lower value means more flammable
#           - The probability of a fire occurring within the modeled area is       calculated each day based on: 

# 1. Litter load, flammability and the available water content of the uppermost soil layer (W, in the range 0-1), as a surrogate for the litter moisture content
W = seq(0, 1, 0.1)
# 2. Litter flammability: characterised by the “moisture of extinction”, which is a PFT-specific parameter in the range 0-1, with lower values representing increasing flammability
me = seq(0, 1, 0.1)

fire <- data.frame(P_fire=as.numeric(),
                 me=as.numeric(),
                 W=as.numeric())

for (i in 1:length(W)){
  w = W[i]
  P_fire = exp(-3.14*(w/me)^2)
  df = data.frame(cbind(P_fire, me, W=rep(w, length(P_fire))))
  fire = rbind(df, fire)
}

ggplot() + geom_point(data=fire, aes(x=W, y=P_fire, color=as.factor(me))) + xlab("W (litter load, flammability and the available water content)") + ylab("P (fire probability)") + scale_fill_discrete(name = "me (litter flammability)")
                                                                                                                                                                                         









