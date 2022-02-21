# Charlotte Uden
# December 2 2021

#Script to look at runs with 1 patch per gridcell (rather than the usual 150 patches/gridcell)

# Question: 
    # 1. is BurntFR output taken from the average burned fraciton across all 150 patches for a given cell ?
    # to answer this, lets look at a single patch! (change npatches in the instruction file from 150 to 1

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
library(mapdata)# for base map
library(maps)# for base map
library(cowplot) # for get_legend
#-----

source("../R_scripts/functions/cleanup_BA.R")

#import lpj-guess runs with a single patch per gridcell
fire0 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=0, distinterval=1, grc=-0.7, fsm=1.4) 
fire1 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire1-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=1, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.1 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-1-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.1, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.01 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-01-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.01, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.02 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-02-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.02, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.05 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-05-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.05, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.001 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-001-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.001, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.002 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-002-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.002, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.005 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-005-singledist-patch1-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.005, distinterval=1, grc=-0.7, fsm=1.4) 

df_1 = rbind(fire0, fire0.001, fire0.002, fire0.005, fire0.01, fire0.02, fire0.05, fire0.1, fire1)
df_1 = filter(df_1, year>1810)

#import lpj-guess runs with 150 patches per gridcell
fire0 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0, distinterval=1, grc=-0.7, fsm=1.4) 
fire1 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire1-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=1, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.1 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-1-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.1, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.01 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-01-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.01, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.02 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-02-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.02, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.05 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-05-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.05, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.001 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-001-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.001, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.002 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-002-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.002, distinterval=1, grc=-0.7, fsm=1.4) 
fire0.005 = cleanup_BA(data=read.table("../data/LPJ-GUESS_output/fire0-005-singledist-grc-07-fsm-04/annual_burned_area.out"),fireprob=0.005, distinterval=1, grc=-0.7, fsm=1.4) 

df_150 = rbind(fire0, fire0.001, fire0.002, fire0.005, fire0.01, fire0.02, fire0.05, fire0.1, fire1)
df_150 = filter(df_150, year>1810)

melt <- with(df_1, aggregate(BurntFr, by = list(year, fireprob), 
                           FUN = "max"))
melt[is.na(melt)] <- 0
names(melt) <- c("year", "fireprob", "BurntFr")

ggplot() + geom_point(data=filter(melt, year>1810), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1))

ggplot() + geom_point(data=filter(melt, year>1810), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1))


# filter dataframe to get gridcells we are looking at to be highligheted and plot them on a map
highlight_df <- df_1 %>% 
  filter((Lat==42.75 & Lon==-73.75) | (Lat==46.25 & Lon==-69.25))
source("../R_scripts/functions/base_map.R")
map = base_map()
map <- ggplot() + geom_polygon(data = new_england, aes(x=long, y = lat, group = group, alpha=0.5))  +
  coord_fixed(1.3)

p1 = map + geom_point(data = highlight_df, aes(x=Lon, y=Lat)) + theme(legend.position = "none") + scale_color_manual(values = "blue")

legend_plot = ggplot() + geom_point(data=filter(df_1, Lat==42.75, Lon==-73.75), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1)) + ggtitle("n patches = 1 | southern point") + guides(alpha = FALSE)
legend <- cowplot::get_legend(legend_plot)

p2 = ggplot() + geom_point(data=filter(df_1, Lat==42.75, Lon==-73.75), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1)) + ggtitle("n patches = 1 | southern point")  + theme(legend.position = "none")

p3 = ggplot() + geom_point(data=filter(df_1, Lat==46.25, Lon==-69.25), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1)) + ggtitle("n patches = 1 | northern point") + theme(legend.position = "none")

p4 = ggplot() + geom_point(data=filter(df_150, Lat==42.75, Lon==-73.75), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1)) + ggtitle("n patches = 150 | southern point") + theme(legend.position = "none")

p5 = ggplot() + geom_point(data=filter(df_150, Lat==46.25, Lon==-69.25), aes(x=year, y=BurntFr, col=as.factor(fireprob), alpha = 0.1)) + theme(legend.position = "none")+ ggtitle("n patches = 150 | northern point")


p5 + p3 + p1 + p4 + p2 + plot_layout(ncol = 3, widths = c(2, 2, 1, 2, 2)) 
(((p5 / p4) | (p3 / p2) + plot_layout(guides = 'keep')) | p1) + plot_layout(guides = 'collect')+ legend

((p5 / p4) | (p3 / p2)) | p1 | legend + plot_layout(widths = c(2, 2, 2, 2, 0.5, 0.5)) 

((p5 / p4) | (p3 / p2)) / (p1 | legend)

((p5 / p4) | (p3 / p2) | legend) / p1 + plot_layout(widths = c(2, 2, 2, 2, 1, 3)) 

(p5 | p3) / (p4 | p2) / (legend | p1)
p5  + p3 + p4 + p2 + legend + p1 + plot_layout(ncol = 2, widths = c(2, 2, 2, 2, 1, 3)) 

# so it looks like Burn Fr is the number of burned patches for a given year, divided by the total number of patches 
# if BurnFr = 0.6, and there are 150 patches total then the number of patches that burned is 150 * 0.6 = 90 patches. 

