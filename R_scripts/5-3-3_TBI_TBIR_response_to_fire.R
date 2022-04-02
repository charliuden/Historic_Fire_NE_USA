#Seeders and Sprouters

#script to look at TBI and TBIR sensitivity to changeing seeder_sprouter_resilience

#ASSUMPTION 1: 
#when fires are frequent, they are less hot
#ASSUMPTION 2: 
#With more frequent fires, TBIR density goes up, age goes down, and dbh goes down
#ASSUMPTION 3: 
#TBI will follow a similar but less extreme pattern

#focus only on TBIR and TBI
palette <- c("TBI"="#0A3DFA","TBIR"="#0BA186")
palette_age <- c("TBI_age"="#0A3DFA","TBIR_age"="#0BA186")
palette_dens <- c("TBI_dens"="#0A3DFA","TBIR_dens"="#0BA186")
palette_dbh <- c("TBI_dbh"="#0A3DFA","TBIR_dbd"="#0BA186")
#coordinates to look at:
Lat==45.25 & Lon==-67.75 #norhtern point
Lat==41.75 & Lon==-73.75 #southern point
# 1. actual intensity of fires 
#############################################################################

#-----
#Libraries
library(ggplot2) 
library(dplyr)
library(plyr)
library(reshape2)
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
#BLAZE with seeders/sprouters
#-----
dbh_list=seq(5, 30, 5)
fli=seq(0, 8000, by = 1000)

seeder = 5
sprouter = 0.3

res = seeder


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

p1 = ggplot() + geom_line(data=filter(df, DBH==5), aes(x=FLI, y=p_survival_TB, color=tactic)) + ggtitle("DBH=5")
p2 = ggplot() + geom_line(data=filter(df, DBH==15), aes(x=FLI, y=p_survival_TB, color=tactic)) + ggtitle("DBH=15")
p3 = ggplot() + geom_line(data=filter(df, DBH==30), aes(x=FLI, y=p_survival_TB, color=tactic)) + ggtitle("DBH=30")
p1 | p2 | p3
#----------

####----- Fire Line Intensity --- ####
clean_fli <- function(fli=x){
  names(fli) <- c("Lon","Lat","year", "max_potfli", "mean_potfli")
  fli <- fli[-c(1),] 
  fli <- as.data.frame(apply(fli, 2, as.numeric))
  return(fli)
}

####----- Annual Burned Area ---
clean_ba <- function(ba=x) {
  names(ba) <- c("Lon","Lat","year", "BurntFr")
  ba <- ba[-c(1),] 
  ba <- as.data.frame(apply(ba, 2, as.numeric))                 
  return(ba)
}

#----------
#function to merge dataframes by lat lon and year and get actual values of FLI
merge_ba_fli <- function(ba=x, fli=y){
  df <- merge(ba, fli, by=c("Lon","Lat","year"))
  #where fires occur (burfr=1), make a column showing the mean potential fire line intensity, otherwise make mean potential fire line intensity 0
  df <- mutate(df, fli_mean=case_when(BurntFr>0 ~ mean_potfli, 
                                      BurntFr==0 ~ 0))
  #same thing, but get max potential fire line intensity
  df <- mutate(df, fli_max=case_when(BurntFr>0 ~ max_potfli, 
                                     BurntFr==0 ~ 0))
  #make a column showing a fire that occurs is over 3000 kw/m 
  df <- mutate(df, heat_mean=case_when(fli_mean>3000 ~ 1, 
                                       fli_mean<3000 ~ 0))
  df <- mutate(df, heat_max=case_when(fli_max>3000 ~ 1, 
                                      fli_max<3000 ~ 0))
  df[is.na(df)] <- 0
  return(df)
}

# --- age -----
clean_age <- function(age=x) {
  names(age) <- c("Lon","Lat","year", "TBI_age", "TBIR_age", "C3G_age", "C4G_age")
  age <- age[-c(1),] 
  age[,1] <- as.numeric(age[,1])
  age[,2] <- as.numeric(age[,2])
  age[,3] <- as.numeric(age[,3])
  age[,4] <- as.numeric(age[,4])
  age[,5] <- as.numeric(age[,5])
  age[,6] <- as.numeric(age[,6])
  age[,7] <- as.numeric(age[,7])
  return(age)
}

# --- stem density -----
clean_density <- function(dens=x){
  names(dens) <- c("Lon","Lat","year", "TBI_dens", "TBIR_dens", "C3G_dens", "C4G_dens", "Totla_dens")
  dens <- dens[-c(1),] 
  dens[,1] <- as.numeric(dens[,1])
  dens[,2] <- as.numeric(dens[,2])
  dens[,3] <- as.numeric(dens[,3])
  dens[,4] <- as.numeric(dens[,4])
  dens[,5] <- as.numeric(dens[,5])
  dens[,6] <- as.numeric(dens[,6])
  dens[,7] <- as.numeric(dens[,7])
  dens[,8] <- as.numeric(dens[,8])
  return(dens)
}

# --- stem diameter -----
clean_diameter <- function(dbh=x){
  names(dbh) <- c("Lon","Lat","year", "TBI_dbh", "TBIR_dbh", "C3G_dbh", "C4G_dbh")
  dbh <- dbh[-c(1),] 
  dbh[,1] <- as.numeric(dbh[,1])
  dbh[,2] <- as.numeric(dbh[,2])
  dbh[,3] <- as.numeric(dbh[,3])
  dbh[,4] <- as.numeric(dbh[,4])
  dbh[,5] <- as.numeric(dbh[,5])
  dbh[,6] <- as.numeric(dbh[,6])
  dbh[,7] <- as.numeric(dbh[,7])
  return(dbh)
}
#------

#------
### Anual Fire Probability = 0
#------
fli_0 = clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/yearly_blaze_fli.out"))

ba_0 = clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/annual_burned_area.out"))

df_0 <- merge_ba_fli(fli=fli_0, ba=ba_0)

age_0 <- clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/age.out")))

dens_0 <- clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dens.out"))

dbh_0 <- clean_diameter(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dbh.out"))

# merge dfs together
df_0 <- merge(df_0, age_0, by=c("Lon","Lat","year"))
df_0 <- merge(df_0, dens_0, by=c("Lon","Lat","year"))
df_0 <- merge(df_0, dbh_0, by=c("Lon","Lat","year"))
head(df_0)
#------

### Anual Fire Probability = 0.02
#------
fli_0.02 = clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-02-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/yearly_blaze_fli.out"))

ba_0.02 = clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-02-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/annual_burned_area.out"))

df_0.02 <- merge_ba_fli(fli=fli_0.02, ba=ba_0.02)

age_0.02 <- clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-02-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/age.out")))

dens_0.02 <- clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-02-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dens.out"))

dbh_0.02 <- clean_diameter(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-02-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dbh.out"))

# --- merge dfs ---
df_0.02 <- merge(df_0.02, age_0.02, by=c("Lon","Lat","year"))
df_0.02 <- merge(df_0.02, dens_0.02, by=c("Lon","Lat","year"))
df_0.02 <- merge(df_0.02, dbh_0.02, by=c("Lon","Lat","year"))
#------

### Anual Fire Probability = 0.01
#------
fli_0.01 = clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-01-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/yearly_blaze_fli.out"))

ba_0.01 = clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-01-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/annual_burned_area.out"))

df_0.01 <- merge_ba_fli(fli=fli_0.01, ba=ba_0.01)

age_0.01 <- clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-01-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/age.out")))

dens_0.01 <- clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-01-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dens.out"))

dbh_0.01 <- clean_diameter(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-01-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dbh.out"))

# --- merge dfs ---
df_0.01 <- merge(df_0.01, age_0.01, by=c("Lon","Lat","year"))
df_0.01 <- merge(df_0.01, dens_0.01, by=c("Lon","Lat","year"))
df_0.01 <- merge(df_0.01, dbh_0.01, by=c("Lon","Lat","year"))
#------

### Anual Fire Probability = 0.005
#------
fli_0.005 = clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-005-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/yearly_blaze_fli.out")) 

ba_0.005 = clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-005-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/annual_burned_area.out"))

df_0.005 <- merge_ba_fli(fli=fli_0.005, ba=ba_0.005)

age_0.005 <- clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-005-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/age.out")))

dens_0.005 <- clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-005-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dens.out"))

dbh_0.005 <- clean_diameter(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-005-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dbh.out"))

# --- merege dfs ---
df_0.005 <- merge(df_0.005, age_0.005, by=c("Lon","Lat","year"))
df_0.005 <- merge(df_0.005, dens_0.005, by=c("Lon","Lat","year"))
df_0.005 <- merge(df_0.005, dbh_0.005, by=c("Lon","Lat","year"))
#------

### Anual Fire Probability = 0.05
#------
fli_0.05 = clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-05-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/yearly_blaze_fli.out"))

ba_0.05 = clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-05-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/annual_burned_area.out"))

df_0.05 <- merge_ba_fli(fli=fli_0.05, ba=ba_0.05)

age_0.05 <- clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-05-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/age.out")))

dens_0.05 <- clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-05-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dens.out"))

dbh_0.05 <- clean_diameter(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-05-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dbh.out"))

# --- merge dfs ---
df_0.05 <- merge(df_0.05, age_0.05, by=c("Lon","Lat","year"))
df_0.05 <- merge(df_0.05, dens_0.05, by=c("Lon","Lat","year"))
df_0.05 <- merge(df_0.05, dbh_0.05, by=c("Lon","Lat","year"))
#------

### Anual Fire Probability = 0.002
#------
fli_0.002 = clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-002-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/yearly_blaze_fli.out"))

ba_0.002 = clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-002-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/annual_burned_area.out"))

df_0.002 <- merge_ba_fli(fli=fli_0.002, ba=ba_0.002)

age_0.002 <- clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-002-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/age.out")))

dens_0.002 <- clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-002-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dens.out"))

dbh_0.002 <- clean_diameter(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/seeders_sprouter_sensitivity_runs/fire0-002-climate1200-patch1-TBI-TBIR-grc-005-fsm-2-seed-5-sprout-03/dbh.out"))

# --- merge dfs ---
df_0.002 <- merge(df_0.002, age_0.002, by=c("Lon","Lat","year"))
df_0.002 <- merge(df_0.002, dens_0.002, by=c("Lon","Lat","year"))
df_0.002 <- merge(df_0.002, dbh_0.002, by=c("Lon","Lat","year"))
#------

#merge all output together for easier handling
df = rbind(cbind(filter(df_0.05, fli_max>0), fire_prob=rep("0.05", nrow(filter(df_0.05, fli_max>0)))),
           cbind(filter(df_0.02, fli_max>0), fire_prob=rep("0.02", nrow(filter(df_0.02, fli_max>0)))),
           cbind(filter(df_0.01, fli_max>0), fire_prob=rep("0.01", nrow(filter(df_0.01, fli_max>0)))), 
           cbind(filter(df_0.005, fli_max>0), fire_prob=rep("0.005", nrow(filter(df_0.005, fli_max>0)))),
           cbind(filter(df_0.002, fli_max>0), fire_prob=rep("0.002", nrow(filter(df_0.002, fli_max>0)))),
           cbind(filter(df_0, fli_max>0), fire_prob=rep("0", nrow(filter(df_0, fli_max>0)))))


#view as a summary table and plot 
fire_prob = rbind(0.05, 0.02, 0.01, 0.005, 0.002, 0, 0.05, 0.02, 0.01, 0.005, 0.002, 0)

fli = rbind(mean(filter(df, fire_prob==0.05 & BurntFr==1)$fli_max), 
            mean(filter(df, fire_prob==0.02 & BurntFr==1)$fli_max), 
            mean(filter(df, fire_prob==0.01 & BurntFr==1)$fli_max), 
            mean(filter(df, fire_prob==0.005 & BurntFr==1)$fli_max), 
            mean(filter(df, fire_prob==0.002 & BurntFr==1)$fli_max), 
            0,
            mean(filter(df, fire_prob==0.05 & BurntFr==1)$fli_max), 
            mean(filter(df, fire_prob==0.02 & BurntFr==1)$fli_max), 
            mean(filter(df, fire_prob==0.01 & BurntFr==1)$fli_max), 
            mean(filter(df, fire_prob==0.005 & BurntFr==1)$fli_max), 
            mean(filter(df, fire_prob==0.002 & BurntFr==1)$fli_max), 
            0)

number_fires = rbind(nrow(filter(df, fire_prob==0.05 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0.02 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0.01 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0.005 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0.002 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0 & BurntFr==1)),
                     nrow(filter(df, fire_prob==0.05 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0.02 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0.01 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0.005 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0.002 & BurntFr==1)), 
                     nrow(filter(df, fire_prob==0 & BurntFr==1)))

fires_over_3000 = rbind(nrow(filter(df, fire_prob==0.05 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0.02 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0.01 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0.005 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0.002 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0 & heat_max==1)),
                        nrow(filter(df, fire_prob==0.05 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0.02 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0.01 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0.005 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0.002 & heat_max==1)), 
                        nrow(filter(df, fire_prob==0 & heat_max==1)))

age = rbind(mean(filter(df, fire_prob==0.05)$TBIR_age), mean(filter(df, fire_prob==0.02)$TBIR_age), mean(filter(df, fire_prob==0.01)$TBIR_age), mean(filter(df, fire_prob==0.005)$TBIR_age), mean(filter(df, fire_prob==0.002)$TBIR_age), mean(filter(df, fire_prob==0)$TBIR_age), mean(filter(df, fire_prob==0.05)$TBI_age), mean(filter(df, fire_prob==0.02)$TBI_age), mean(filter(df, fire_prob==0.01)$TBI_age), mean(filter(df, fire_prob==0.005)$TBI_age), mean(filter(df, fire_prob==0.002)$TBI_age), mean(filter(df, fire_prob==0)$TBI_age))

density = rbind(mean(filter(df, fire_prob==0.05)$TBIR_dens), mean(filter(df, fire_prob==0.02)$TBIR_dens), mean(filter(df, fire_prob==0.01)$TBIR_dens), mean(filter(df, fire_prob==0.005)$TBIR_dens), mean(filter(df, fire_prob==0.002)$TBIR_dens), mean(filter(df, fire_prob==0)$TBIR_dens), mean(filter(df, fire_prob==0.05)$TBI_dens), mean(filter(df, fire_prob==0.02)$TBI_dens), mean(filter(df, fire_prob==0.01)$TBI_dens), mean(filter(df, fire_prob==0.005)$TBI_dens), mean(filter(df, fire_prob==0.002)$TBI_dens), mean(filter(df, fire_prob==0)$TBI_dens))

diameter = rbind(mean(filter(df, fire_prob==0.05)$TBIR_dbh), mean(filter(df, fire_prob==0.02)$TBIR_dbh), mean(filter(df, fire_prob==0.01)$TBIR_dbh), mean(filter(df, fire_prob==0.005)$TBIR_dbh), mean(filter(df, fire_prob==0.002)$TBIR_dbh), mean(filter(df, fire_prob==0)$TBIR_dbh), mean(filter(df, fire_prob==0.05)$TBI_dbh), mean(filter(df, fire_prob==0.02)$TBI_dbh), mean(filter(df, fire_prob==0.01)$TBI_dbh), mean(filter(df, fire_prob==0.005)$TBI_dbh), mean(filter(df, fire_prob==0.002)$TBI_dbh), mean(filter(df, fire_prob==0)$TBI_dbh))

PFT = c(rep("TBIR", 6), rep("TBI", 6))

cols = c("fire_prob", "fli", "number_fires", "fires_over_3000", "density", "age", "diameter", "PFT")
table = as.data.frame(cbind(fire_prob, fli,  number_fires, fires_over_3000, density, age, diameter, PFT))
colnames(table) = cols
table[,1] <- as.numeric(table[,1])
table[,2] <- as.numeric(table[,2])
table[,3] <- as.numeric(table[,3])
table[,4] <- as.numeric(table[,4])
table[,5] <- as.numeric(table[,5])
table[,6] <- as.numeric(table[,6])
table[,7] <- as.numeric(table[,7])
table[,8] <- as.factor(table[,8])

p1 = ggplot() + geom_point(data=table, aes(x=fire_prob, y=number_fires)) + geom_line(data=table, aes(x=fire_prob, y=number_fires)) + xlab("")#+ ylab("total number of fires")
p2 = ggplot() + geom_point(data=table, aes(x=fire_prob, y=fires_over_3000))  + geom_line(data=table, aes(x=fire_prob, y=fires_over_3000)) + xlab("")#+ ylab("number of fires over 3000 kw/m")
p3 = ggplot() + geom_point(data=table, aes(x=fire_prob, y=fli)) + geom_line(data=table, aes(x=fire_prob, y=fli)) + xlab("")#+ ylab("mean fire line intensity")

p4 = ggplot() + geom_point(data=table, aes(x=fire_prob, y=density, color=PFT)) + geom_line(data=table, aes(x=fire_prob, y=density, col=PFT)) + xlab("") + scale_color_manual(values=palette)#+ ylab("mean density")

p5 = ggplot() + geom_point(data=table, aes(x=fire_prob, y=age, color=PFT)) + geom_line(data=table, aes(x=fire_prob, y=age, color=PFT)) + xlab("") + scale_color_manual(values=palette)#+ ylab("mean age")

p6 = ggplot() + geom_point(data=table, aes(x=fire_prob, y=diameter, color=PFT)) + geom_line(data=table, aes(x=fire_prob, y=diameter, color=PFT)) + xlab("Fire Probability") + scale_color_manual(values=palette) #+ ylab("mean diameter") 

p1 /p3 / p4 / p5 / p6


