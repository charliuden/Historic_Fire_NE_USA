# January 18th 2022

# ---- Exploring BLAZE and TBIR response to fire --- 

#   •Mortality as a function of fli and dbh
#   •What is the intensity of the actual fires occurring?
  #   •Because if fires are at a lower intensity (below 3000), then of course tbir is not doing as well as we would like. Below 3000 kw/m2, the fire survival probability of TBI is high. So it will do no worse than TBIR. 
#   •Try to look at density and dbh again

#focus only on TBIR and TBI
palette <- c("TBI"="#0A3DFA","TBIR"="#0BA186")
palette_age <- c("TBI_age"="#0A3DFA","TBIR_age"="#0BA186")
palette_dens <- c("TBI_dens"="#0A3DFA","TBIR_dens"="#0BA186")

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

####----- Fire Line Intensity --- ####

clean_fli <- function(fli=x){
  names(fli) <- c("Lon","Lat","year", "max_potfli", "mean_potfli")
  fli <- fli[-c(1),] 
  fli <- as.data.frame(apply(fli, 2, as.numeric))
  return(fli)
}
fli = clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/yearly_blaze_fli.out"))

####----- Annual Burned Area ---

clean_ba <- function(ba=x) {
  names(ba) <- c("Lon","Lat","year", "BurntFr")
  ba <- ba[-c(1),] 
  ba <- as.data.frame(apply(ba, 2, as.numeric))                 
  return(ba)
}
ba = clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/annual_burned_area.out"))

#----------
#function to merge dataframes by lat lon and year and get actual values of FLI
merge_ba_fli <- function(ba=x, fli=y){
  df <- merge(ba, fli, by=c("Lon","Lat","year"))
  #where fires occur (burfr=1), make a column showing the mean potential fire line intensity, otherwise make mean potential fire line intensity 0
  df <- mutate(df, fli_mean=case_when(BurntFr==0 ~ mean_potfli, 
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

df <- merge_ba_fli(fli=fli, ba=ba)


p1 <- ggplot() + geom_point(data=df, aes(x=year, y=fli_max, col=as.factor(heat_max))) + ylab("maximum fire line intensity (kW/m)") + scale_color_manual(values=c("gold", "red"), name = "kW/m", labels = c("<3000", ">3000"))


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
age <- clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/age.out")))

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
dens <- clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/dens.out"))

# --- put all datasets together ---
df <- merge(df, age, by=c("Lon","Lat","year"))
df <- merge(df, dens, by=c("Lon","Lat","year"))
#############################################################################
# 2. tree density and stand age
ggplot() + geom_point(data=df, aes(y=TBIR_dens, x=TBIR_age))

melt_age <- melt(data = df, id.vars = "year", measure.vars = c("TBI_age","TBIR_age"))
melt_age[is.na(melt_age)] <- 0
names(melt_age) <- c("year", "PFT", "age")
melt_dens <- melt(data = df, id.vars = "year", measure.vars = c("TBI_dens","TBIR_dens"))
melt_dens[is.na(melt_dens)] <- 0
names(melt_dens) <- c("year", "PFT", "dens")

p2 <- ggplot() + geom_point(data=melt_age, aes(x=year, y=age, color=PFT, alpha=0.1)) + scale_color_manual(values=palette_age) + guides(alpha=FALSE)
p3 <- ggplot() + geom_point(data=melt_dens, aes(x=year, y=dens, color=PFT, alpha=0.1)) + scale_color_manual(values=palette_dens) + ylab("density") + guides(alpha=FALSE)

p1 / p2 / p3


#############################################################################
# 3. look at single points

#Lat==45.25 & Lon==-67.75 #norhtern point
#Lat==41.75 & Lon==-73.75

p1 <- ggplot() + geom_point(data=filter(df, Lat==41.75 & Lon==-73.75), aes(x=year, y=fli_max, col=as.factor(heat_max))) + ylab("maximum fire line intensity (kW/m)") + scale_color_manual(values=c("gold", "red"), name = "kW/m", labels = c("<3000", ">3000")) + ggtitle("Lat==41.75 & Lon==-73.75") 


melt_age <- melt(data = filter(df,Lat==41.75 & Lon==-73.75), id.vars = "year", measure.vars = c("TBI_age","TBIR_age"))
melt_age[is.na(melt_age)] <- 0
names(melt_age) <- c("year", "PFT", "age")
melt_dens <- melt(data = filter(df, Lat==41.75 & Lon==-73.75), id.vars = "year", measure.vars = c("TBI_dens","TBIR_dens"))
melt_dens[is.na(melt_dens)] <- 0
names(melt_dens) <- c("year", "PFT", "dens")

p2 <- ggplot() + geom_point(data=melt_age, aes(x=year, y=age, color=PFT, alpha=0.1)) + scale_color_manual(values=palette_age)  + ggtitle("Lat==41.75 & Lon==-73.75") + guides(alpha=FALSE) + theme(axis.text = element_text(size = 15)) 
p3 <- ggplot() + geom_point(data=melt_dens, aes(x=year, y=dens, color=PFT, alpha=0.1)) + scale_color_manual(values=palette_dens) + ylab("density") + ggtitle("Lat==41.75 & Lon==-73.75") + guides(alpha=FALSE)

p1 / p2 / p3

# need to get a closer look at these 'hot fires'
df_hot = filter(df, BurntFr==1 & heat_max==1)
nrow(df_hot) 

# there were only 12 fires over the course of 200 years and 123 patches, even though the fire probability was set to 0.1 (fire every 10 years)


#############################################################################
# 4. mortality and fire line intensity

#January 20: look to see if stand age responds to fire

#fire0-1-singledist-patch1-TBI-TBIR-grc-0-fsm-01
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-0-fsm-01/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-0-fsm-01/annual_burned_area.out")))
                  
df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-0-fsm-01/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-0-fsm-01/dens.out")), by=c("Lon","Lat","year"))

df_0.1_0_0.1 = cbind(df, fireprob=rep(0.1, nrow(df)), grc=rep(0, nrow(df)), fsm=rep(0.1, nrow(df)))

#fire0-1-singledist-patch1-TBI-TBIR-grc-07-fsm-04
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-07-fsm-04/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-07-fsm-04/annual_burned_area.out")))

df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-07-fsm-04/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-07-fsm-04/dens.out")), by=c("Lon","Lat","year"))

df_0.1_0.7_0.4 = cbind(df, fireprob=rep(0.1, nrow(df)), grc=rep(0.7, nrow(df)), fsm=rep(0.4, nrow(df)))


#fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/annual_burned_area.out")))

df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/dens.out")), by=c("Lon","Lat","year"))

df_0.02_0.7_0.4 = cbind(df, fireprob=rep(0.02, nrow(df)), grc=rep(0.7, nrow(df)), fsm=rep(0.4, nrow(df)))


#fire0-005-singledist-patch1-TBI-TBIR-grc-0-fsm-01
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-005-singledist-patch1-TBI-TBIR-grc-0-fsm-01/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-005-singledist-patch1-TBI-TBIR-grc-0-fsm-01/annual_burned_area.out")))

df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-005-singledist-patch1-TBI-TBIR-grc-0-fsm-01/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-005-singledist-patch1-TBI-TBIR-grc-0-fsm-01/dens.out")), by=c("Lon","Lat","year"))

df_0.005_0_0.1 = cbind(df, fireprob=rep(0.005, nrow(df)), grc=rep(0, nrow(df)), fsm=rep(0.1, nrow(df)))

#fire0-005-singledist-patch1-TBI-TBIR-grc-005-fsm-2
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-005-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-005-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/annual_burned_area.out")))

df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-005-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-005-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/dens.out")), by=c("Lon","Lat","year"))

df_0.005_0.05_2 = cbind(df, fireprob=rep(0.005, nrow(df)), grc=rep(0.05, nrow(df)), fsm=rep(2, nrow(df)))

#fire0-02-singledist-patch1-TBI-TBIR-grc-005-fsm-2
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/annual_burned_area.out")))

df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/dens.out")), by=c("Lon","Lat","year"))

df_0.02_0.05_2 = cbind(df, fireprob=rep(0.02, nrow(df)), grc=rep(0.05, nrow(df)), fsm=rep(2, nrow(df)))

#fire0-1-singledist-patch1-TBI-TBIR-grc-005-fsm-2
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/annual_burned_area.out")))

df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/dens.out")), by=c("Lon","Lat","year"))

df_0.1_0.05_2 = cbind(df, fireprob=rep(0.1, nrow(df)), grc=rep(0.05, nrow(df)), fsm=rep(2, nrow(df)))


df <- rbind(df_0.1_0_0.1, df_0.1_0.7_0.4, df_0.02_0.7_0.4, df_0.005_0_0.1, df_0.005_0.05_2, df_0.02_0.05_2, df_0.1_0.05_2, df_0.02_0.05_2)

#find where fires exceed 3000 and take the following 50 years of data

  filter(df, heat_max==1)
  
  fires <- data.frame(Lon=as.numeric(), 
                      Lat=as.numeric(), 
                      year=as.numeric(),
                      BurntFr=as.numeric(),
                      max_potfli=as.numeric(),
                      mean_potfli=as.numeric(),
                      fli_mean=as.numeric(),
                      fli_max=as.numeric(),
                      heat_mean=as.numeric(),
                      heat_max=as.numeric(), 
                      TBI_age=as.numeric(), 
                      TBIR_age=as.numeric(),
                      C3G_age=as.numeric(),
                      C4G_age=as.numeric(),
                      TBI_dens=as.numeric(),
                      TBIR_dens=as.numeric(),
                      C3G_dens=as.numeric(),
                      C4G_dens=as.numeric(),
                      Totla_dens=as.numeric(), 
                      fireprob=as.numeric(),
                      grc=as.numeric(),
                      fsm=as.numeric()
                      ) 
#find hot fires and the preceding 50 years  
for (i in seq(1, nrow(df))) {
  if (df[i,c("heat_max")]==1) {
    rows=i+50
    fires <- rbind(fires, df[i:rows,])
  }
}
  
write.csv(fires, "/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-1-singledist-patch1-TBI-TBIR-grc-005-fsm-2/fires_age_density_50years.csv")

head(fires)

  
ggplot(fires, aes(x=year, y=TBIR_age, color=as.factor(grc))) + geom_point()

#you would expect density to decrease and age to increase after a hot fire. 
ggplot(fires, aes(x=year, y=TBIR_age, color=TBIR_dens, alpha=0.1)) + geom_point()

#perhaps, even though TBIR is more fire resistant, TBI is able to recover more quickly because its growth respiration cost is lower than TBIR

arrange(filter(df, heat_max==1 & grc==0.05), year)
ggplot(filter(fires, grc==0.05), aes(x=year, y=TBIR_age, alpha=0.1)) + geom_point()
#but why is TBIR sometimes able to recover from fire but sometimes not. NOTE all instances when tbir does not recoer, grc=0.7 BUT sometimes when grc=0.7, tbir is able to recover. 
#there is no association with lat, lon or fire probability. 
ggplot(filter(fires, grc==0.05 & fireprob==0.02), aes(x=year, y=TBIR_age, alpha=0.1)) + geom_point()


ggplot(filter(fires, grc==0.05 & fireprob==0.02), aes(x=year, y=TBI_age, alpha=0.1, color=fli_max)) + geom_point()


melt_age <- melt(data = filter(fires,  grc==0.05 & fireprob==0.02), id.vars = "year", measure.vars = c("TBI_age","TBIR_age"))
melt_age[is.na(melt_age)] <- 0
names(melt_age) <- c("year", "PFT", "age")
ggplot(melt_age, aes(x=year, y=age, alpha=0.1, color=PFT)) + geom_point()


#Fire Kills young ones, so mean age should increase because fire is killing the young trees.
#   1. For a given fire_survival_modifier, find the FLI required to kill TBIR. 
#   2. In the run output, find fires that meet that threshold
#   3. take the following 50 years after that fire and check that average age increases post fire

#FSM = 2, survival oroabability for tbir begins to fall below 1 at around 5000 kw/m - so look at fires above 5000 and take following 50 years of output
# NOTE: dbh does not change survival probability by much... whcih is a problem if we are interested in response in age... 

#fire0-02-singledist-patch1-TBI-TBIR-grc-005-fsm-2
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/annual_burned_area.out")))

df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-climate1200-singledist-patch1-TBI-TBIR-grc-005-fsm-2/dens.out")), by=c("Lon","Lat","year"))

df = cbind(df, fireprob=rep(0.02, nrow(df)), grc=rep(0.05, nrow(df)), fsm=rep(2, nrow(df)))

#--
fires <- data.frame(Lon=as.numeric(), 
                    Lat=as.numeric(), 
                    year=as.numeric(),
                    BurntFr=as.numeric(),
                    max_potfli=as.numeric(),
                    mean_potfli=as.numeric(),
                    fli_mean=as.numeric(),
                    fli_max=as.numeric(),
                    heat_mean=as.numeric(),
                    heat_max=as.numeric(), 
                    TBI_age=as.numeric(), 
                    TBIR_age=as.numeric(),
                    C3G_age=as.numeric(),
                    C4G_age=as.numeric(),
                    TBI_dens=as.numeric(),
                    TBIR_dens=as.numeric(),
                    C3G_dens=as.numeric(),
                    C4G_dens=as.numeric(),
                    Totla_dens=as.numeric(), 
                    fireprob=as.numeric(),
                    grc=as.numeric(),
                    fsm=as.numeric()
) 
# TBIR: find fires above 5000 kw/m and the preceding 50 years 
for (i in seq(1, nrow(df))) {
  if (df[i,c("fli_max")]>4500) {
    rows=i+50
    fires <- rbind(fires, df[i:rows,])
  }
}
fires_tbir = fires
p1 <- ggplot(data=fires_tbir) + geom_point(mapping=aes(x=year, y=TBIR_age), color="#0BA186") + ylim(0, 200)
p2 <- ggplot(data=filter(fires_tbir, fli_max>3000), aes(x=year, y=fli_max)) + geom_point() + ylim(0, 10000)
p1 / p2

# TBI: find fires above 3000 kw/m and the preceding 50 years 
for (i in seq(1, nrow(df))) {
  if (df[i,c("fli_max")]>3000) {
    rows=i+50
    fires <- rbind(fires, df[i:rows,])
  }
}
fires_tbi = fires
p3 <- ggplot(data=fires_tbi) + geom_point(mapping=aes(x=year, y=TBI_age), color="#0A3DFA")+ ylim(0, 200)
p4 <- ggplot(data=filter(fires_tbi, fli_max>3000), aes(x=year, y=fli_max)) + geom_point() + ylim(0, 10000)
(p1 / p2) | (p3 / p4)















#fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/
df = merge_ba_fli(fli=clean_fli(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/yearly_blaze_fli.out")), ba=clean_ba(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/annual_burned_area.out")))

df = merge(df, clean_age(as.data.frame(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/age.out"))), by=c("Lon","Lat","year"))

df = merge(df, clean_density(read.table("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/LPJ-GUESS_output/fire0-02-singledist-patch1-TBI-TBIR-grc-07-fsm-04/dens.out")), by=c("Lon","Lat","year"))

df = cbind(df, fireprob=rep(0.02, nrow(df)), grc=rep(0.7, nrow(df)), fsm=rep(0.4, nrow(df)))

#--
fires <- data.frame(Lon=as.numeric(), 
                    Lat=as.numeric(), 
                    year=as.numeric(),
                    BurntFr=as.numeric(),
                    max_potfli=as.numeric(),
                    mean_potfli=as.numeric(),
                    fli_mean=as.numeric(),
                    fli_max=as.numeric(),
                    heat_mean=as.numeric(),
                    heat_max=as.numeric(), 
                    TBI_age=as.numeric(), 
                    TBIR_age=as.numeric(),
                    C3G_age=as.numeric(),
                    C4G_age=as.numeric(),
                    TBI_dens=as.numeric(),
                    TBIR_dens=as.numeric(),
                    C3G_dens=as.numeric(),
                    C4G_dens=as.numeric(),
                    Totla_dens=as.numeric(), 
                    fireprob=as.numeric(),
                    grc=as.numeric(),
                    fsm=as.numeric()
) 
# TBIR: find fires above 5000 kw/m and the preceding 50 years 
for (i in seq(1, nrow(df))) {
  if (df[i,c("fli_max")]>4000) {
    rows=i+50
    fires <- rbind(fires, df[i:rows,])
  }
}
fires_tbir = fires
p1 <- ggplot(data=fires_tbir) + geom_point(mapping=aes(x=year, y=TBIR_age), color="#0BA186") + ylim(0, 200)
p2 <- ggplot(data=filter(fires_tbir, fli_max>3000), aes(x=year, y=fli_max)) + geom_point() + ylim(0, 10000)
p1 / p2

# TBI: find fires above 3000 kw/m and the preceding 50 years 
for (i in seq(1, nrow(df))) {
  if (df[i,c("fli_max")]>3000) {
    rows=i+50
    fires <- rbind(fires, df[i:rows,])
  }
}
fires_tbi = fires
p3 <- ggplot(data=fires_tbi) + geom_point(mapping=aes(x=year, y=TBI_age), color="#0A3DFA")+ ylim(0, 200)
p4 <- ggplot(data=filter(fires_tbi, fli_max>3000), aes(x=year, y=fli_max)) + geom_point() + ylim(0, 10000)
(p1 / p2) | (p3 / p4)

