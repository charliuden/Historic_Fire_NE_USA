# November 30 2021

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

# Step 2 of Analysis: After finding the Squared sum or residials for each LPJ-GUESS 200 year run, we have two questions:

  # 1. which run has the lowest SSR when compared with the FIA data 
  # 2. how much does PFT composition change in response to GRC, FSM and fire probability? 

# ----- PART 1------

#this csv file contains squared sum of residuals for each lpj-guess run (calculated from FIA data). 
SSR_fia <- read.csv("../data/fia_residuals_&_SSR.csv")[,2:11]
unique(SSR_fia[,c("fsm","grc")])
filter(SSR_fia, all_PFT==min(SSR_fia$all_PFT))
arrange(SSR_fia, all_PFT)

str(SSR_fia)
arrange(SSR_fia, all_PFT)

#Plot SSR to fine best fit
p4 <- ggplot() + geom_point(data=SSR_fia, aes(x=fsm, y=all_PFT, col=as.factor(fireprob))) +xlab("fire_survival_mod") + ylab("squared sum of residuals") + ggtitle("SSR from best neighbor in neighborhood") + scale_x_continuous(breaks=seq(from=1, to=2, by=0.1)) + guides(size="none") + guides(col=guide_legend(title="annual fire prob"))

p5 <- ggplot() + geom_point(data=SSR_fia, aes(x=grc, y=all_PFT, col=as.factor(fireprob))) +xlab("growth_resp_cost") + ylab("squared sum of residuals") + ggtitle("SSR from best neighbor in neighborhood") + scale_x_continuous(breaks=seq(from=-1, to=0, by=0.1)) + guides(size="none") + guides(col=guide_legend(title="annual fire prob"))

                                        
p4 
p5

p6 <- ggplot() + geom_point(data=SSR_fia, aes(x=grc, y=all_PFT, size=fireprob, alpha=0.1)) +xlab("growth_resp_cost") + ylab("squared sum of residuals") + ggtitle("Squared sum of residuals from best neighbor in neighborhood") + scale_x_continuous(breaks=seq(from=-1, to=0, by=0.1)) + guides(alpha="none") 
p6

#look at runs so far
#make empty data frame to hold best fire prob for each combo
best_ssr = data.frame(SSR=as.numeric(),
                      fireprob=as.numeric(),
                      grc=as.numeric(),
                      fsm=as.numeric())

#get unique combos of grc and fsm 
combos = unique(SSR_fia[,c("grc", "fsm")])
for (i in seq(from=1, to=nrow(combos))) {
  #i = 1
  combo = combos[i,]
  combo = filter(SSR_fia, grc==combo[1,1] & fsm==combo[1,2])
  combo = filter(combo, all_PFT==min(combo$all_PFT))
  best_combo = as.data.frame(cbind(SSR=combo$all_PFT, fireprob=combo$fireprob, grc=combo$grc, fsm=combo$fsm))
  best_ssr = rbind(best_ssr, best_combo)
}

best_ssr

ggplot() + geom_point(data = SSR_fia, aes(x=grc, y=fsm)) + scale_x_continuous(breaks=seq(from=-1, to=0, by=0.1)) + scale_y_continuous(breaks=seq(from=1, to=2, by=0.1))

#plot of x=grc, y=fsm, size=fire probability at that grc/fsm combo with the lowest SSR, and color = SSR
ggplot() + geom_point(data = best_ssr, aes(x=grc, y=fsm, color=log(SSR), size=fireprob)) + scale_x_continuous(breaks=seq(from=-1, to=0, by=0.1)) + scale_y_continuous(breaks=seq(from=1, to=2, by=0.1)) + scale_color_gradient(high="red", low="green")

# ----- PART 2 ----------

# Will need to run the following function on each lpj-guess output and plot to see how much each PFT responds to changing fire, GRC and FAM.
# But this function has to run the function on babbage, where the data is
mean_lai_pft = function(data=x){
  #fire prob must be character string, ie: "fore_0"
  pft_means = cbind(mean(data$BNT), mean(data$BBI), mean(data$TBT), mean(data$TBI), mean(data$TBIR), mean(data$BNI))
  mean_total = sum(sum(data$BNT), sum(data$BBI), sum(data$TBT), sum(data$TBI), sum(data$TBIR), sum(data$BNI)) /(nrow(data[,4:9])*ncol(data[,4:9]))
  #PFT = c("BNT", "BBI", "TBT", "TBI", "TBIR", "BNI")
  fire = data[1,13]#index in table
  fsm = data[1,15]#index table
  grc = data[1,16]#index table
  data = cbind(pft_means, mean_total, fire, fsm, grc)
  names(data) = c("BNT", "BBI", "TBT", "TBI", "TBIR", "BNI", "total", "fire", "fsm", "grc")
  return(data)
}

#output:
lai_mean <- read.csv("/Users/charlotteuden/Desktop/trees/Historic_Fire_NE_USA/data/mean_lai_for_fsm_grc_sensitivity_nov17.csv")[,2:11]
#replace fsm=0 with fsm = 1
index <- lai_mean$fsm == 0
lai_mean$fsm[index] <- 1
head(lai_mean)

#------
#add FIA and witness tree data for comparison data for comparison 
#------
mean_ob
mean_obs_pft <- function(data=x, fire=y, fsm=x, grc=w, dataset=v){
  #fire = NA
  #dataset = "Witness_Trees"
  #fsm = NA
  #grc = NA
  #data = wit
  pft_means = cbind(mean(data$BNT), mean(data$BBI), mean(data$TBT), mean(data$TBI), mean(data$TBIR), mean(data$BNI))
  mean_total = sum(sum(data$BNT), sum(data$BBI), sum(data$TBT), sum(data$TBI), sum(data$TBIR), sum(data$BNI)) /(nrow(data)*6)
  data = as.data.frame(cbind(pft_means, mean_total, fire, fsm, grc, dataset))
  colnames(data) = c("BNT", "BBI", "TBT", "TBI", "TBIR", "BNI", "total_mean", "fire_prob", "fsm", "grc", "dataset")
  data[,c("BNT")] = as.numeric(data[,c("BNT")])
  data[,c("BBI")] = as.numeric(data[,c("BBI")])
  data[,c("TBT")] = as.numeric(data[,c("TBT")])
  data[,c("TBI")] = as.numeric(data[,c("TBI")])
  data[,c("TBIR")] = as.numeric(data[,c("TBIR")])
  data[,c("BNI")] = as.numeric(data[,c("BNI")])
  
  #standardise
  data = mutate(data, total = BNT+BBI+TBT+TBI+TBIR+BNI)
  data = mutate(data, BNT=BNT/total, BBI=BBI/total, TBT=TBT/total, TBI=TBI/total, TBIR=TBIR/total, BNI=BNI/total)
  return(data)
}
# read in witness tree data
wit <- read.csv("../data/witness_trees/witness_treees_PFTs.csv")[,-1]
wit_mean = mean_obs_pft(data=wit, fire=NA, fsm=NA, grc=NA, dataset="Witness_Trees")
#read in FIA tree data
fia <- read.csv("../data/FIA/FIA_PFTs.csv")[,-1]
fia_mean = mean_obs_pft(data=fia, fire=NA, fsm=NA, grc=NA, dataset = "FIA")
mean_obs = rbind(fia_mean, wit_mean)

mean_obs_melt <- melt(data = mean_obs, id.vars = c("fire_prob","fsm", "grc", "dataset"), measure.vars = c("BNT","BNI","TBT","BBI","TBI","TBIR"))
mean_obs_melt[is.na(mean_obs_melt)] <- 0
names(mean_obs_melt) <- c("fire_prob", "fsm", "grc", "dataset", "PFT", "mean_proportion")
mean_obs_melt[,6] <- as.numeric(mean_obs_melt[,6])

p1 = ggplot() + geom_point(data=mean_obs_melt, aes(x=dataset, y=mean_proportion, col=PFT, size = 1)) + ylab("mean proportion") + xlab("observed data") + scale_color_manual(values=palette) + ylim(0,1)
p1

#boxplot of fia and wit
head(wit)
head(fia)
f = cbind(fia[,4:9], dataset= rep("FIA", nrow(fia)))
w = cbind(wit[,5:10], dataset= rep("Witness_Trees", nrow(wit)))
fw = rbind(f, w)
fw <- melt(data = fw, id.vars = c("dataset"), measure.vars = c("BNT","BNI","TBT","BBI","TBI","TBIR"))
names(fw) <- c("dataset", "PFT", "prop_total")

ggplot() + geom_boxplot(data=fw, aes(x=PFT, y=prop_total, fill= dataset, color=PFT)) + scale_color_manual(values=palette) + scale_fill_manual(values=c("lightgrey", "darkgray")) + ylab("proportion of total")
#------
# Plot LPJ-GUESS output means

lai_mean = mutate(lai_mean, total = BNT+BBI+TBT+TBI+TBIR+BNI)
lai_mean = mutate(lai_mean, BNT=BNT/total, BBI=BBI/total, TBT=TBT/total, TBI=TBI/total, TBIR=TBIR/total, BNI=BNI/total)

df_melt <- melt(data = lai_mean, id.vars = c("fire_prob","fsm", "grc"), measure.vars = c("BNT","BNI","TBT","BBI","TBI","TBIR"))
df_melt[is.na(df_melt)] <- 0
names(df_melt) <- c("fire_prob", "fsm", "grc", "PFT", "mean_lai")
df_melt[,5] <- as.numeric(df_melt[,5])

palette <- c("BNT" = "#FE630A","TBT"="#FAC10A", "BNI" = "lightblue", 'BBI'='#97BB02', "TBI"="#0A3DFA","TBIR"="#0BA186")

p2 = ggplot() + geom_jitter(data=df_melt, aes(x=fsm, y=mean_lai, col=PFT, size=fire_prob, alpha=0.001)) + ylab("mean proportion (leaf area index)") + xlab("fire_survival_mod") + scale_color_manual(values=palette) + scale_x_continuous(breaks = round(seq(min(df_melt$fsm), max(df_melt$fsm), by = 0.1),1)) + ylim(0,1) + guides(alpha="none") #+ geom_vline(xintercept = 0.95, color="lightgray") + geom_vline(xintercept = 1.05)+ geom_vline(xintercept = 1.15)+ geom_vline(xintercept = 1.25)+ geom_vline(xintercept = 1.35)+ geom_vline(xintercept = 1.45)

p3 = ggplot() + geom_jitter(data=df_melt, aes(x=grc, y=mean_lai, col=PFT, size=fire_prob, alpha=0.001)) + ylab("mean proportion (leaf area index)") + xlab("growth_resp_cost") + scale_color_manual(values=palette) + scale_x_continuous(breaks = round(seq(min(df_melt$grc), max(df_melt$grc), by = 0.1),1)) + ylim(0,1) + guides(alpha="none")

#plot observerved and predicted portion of PFTs:
p1 | p2 | p3

#------
# plot fire on x axis, lai on y, and color by PFT

df_melt <- df_melt %>%
  mutate(fire_interval = case_when(
    fire_prob==0.000 ~ 1000,
    fire_prob==0.100 ~ 10,
    fire_prob==0.010 ~ 100,
    fire_prob==0.020 ~ 50,
    fire_prob==0.050 ~ 20,
    fire_prob==0.005 ~ 200,
    fire_prob==0.001 ~ 1000,
    fire_prob==0.002 ~ 500,
    fire_prob==1.000 ~ 1
  ))

ggplot() + geom_point(data=mean_obs_melt, aes(x=dataset, y=mean_proportion, col=PFT, size = 1)) + ylab("mean proportion") + xlab("observed data") + scale_color_manual(values=palette) + ylim(0,0.75)
ggplot() + geom_point(data=df_melt, aes(x=fire_interval, y=mean_lai, col=PFT, size=grc, alpha=0.01)) + scale_color_manual(values=palette)+ ylim(0,0.75)+ ylab("mean proportion (leaf area index)") + xlab("fire interval (years)")
ggplot() + geom_point(data=df_melt, aes(x=fire_interval, y=mean_lai, col=PFT, size=fsm, alpha=0.01)) + scale_color_manual(values=palette)+ ylim(0,0.75)+ ylab("mean proportion (leaf area index)") + xlab("fire interval (years)")


tbir = filter(df_melt, PFT=='TBIR')
ggplot() + geom_point(data=tbir, aes(x=fire_interval, y=mean_lai))



df = filter(lai_mean, fire_prob==0.02, fsm==1.4, grc==-0.7)
df = filter(lai_mean, fire_prob==0.02)
df = mutate(df, total = BNT+BBI+TBT+TBI+TBIR+BNI)
df = mutate(df, BNT=BNT/total, BBI=BBI/total, TBT=TBT/total, TBI=TBI/total, TBIR=TBIR/total, BNI=BNI/total)


melt <- melt(data = df, id.vars = c("fire_prob","fsm", "grc"), measure.vars = c("BNT","BNI","TBT","BBI","TBI","TBIR"))
melt[is.na(melt)] <- 0
names(melt) <- c("fire_prob", "fsm", "grc", "PFT", "mean_proportion")

#best run
ggplot() + geom_jitter(data=melt, aes(x=fsm, y=mean_proportion, col=PFT, size = 1)) + scale_color_manual(values=palette) + xlab("") + ylab("") + theme(
  axis.text.x = element_blank())+ ylim(0,0.75) + scale_x_discrete(breaks = NULL) + guides(size="none")
#best fire value, all grc and fsm
a = ggplot() + geom_point(data=melt, aes(x=fsm, y=mean_proportion, col=PFT, size=0.1)) + scale_color_manual(values=palette) + xlab("") + ylab("") + ylim(0,1)+ guides(size="none")

b = ggplot() + geom_point(data=melt, aes(x=grc, y=mean_proportion, col=PFT)) + scale_color_manual(values=palette) + xlab("") + ylab("") + ylim(0,1)+ guides(size="none")

a+b


ggplot() + geom_point(data=melt, aes(x=fsm, y=mean_proportion, col=PFT, size=grc)) + scale_color_manual(values=palette) + ylim(0,0.75)
+ scale_x_continuous(breaks = round(seq(min(melt$fsm), max(fsm), by = 0.1)))



+ xlab("") + ylab("") + ylim(0,0.75)+ guides(size="none")


ggplot() + geom_point(data=filter(melt, fsm==1.4), aes(x=grc, y=mean_proportion, col=PFT)) + scale_color_manual(values=palette) + ylim(0,0.75)

ggplot() + geom_point(data=filter(melt, grc==-0.7), aes(x=fsm, y=mean_proportion, col=PFT)) + scale_color_manual(values=palette) + ylim(0,0.75)













