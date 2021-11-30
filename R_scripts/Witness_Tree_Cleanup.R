#Script to clean up the raw witness tree data from Charlie Cogbill
#tidied up November 23rd 2021

# Data Source:
#Cogbill, C. V., Burk, J., & Motzkin, G. (2002). The forests of presettlement New England, USA: Spatial and compositional patterns based on town proprietor surveys. Journal of Biogeography. https://doi.org/10.1046/j.1365-2699.2002.00757.x

# 1. find prominent genera (make up more htan 0.05% of total genus composition)
# 2. sort dominant genera into PFT's
# 3. add date intervals for each state
# 4. export as csv for future use

#------
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
library(cowplot)
#------
#------------------------
#read in witness tree data
#------------------------
wit <- read.csv("../data/witness_trees/583NewEngland%.csv") #raw from charlie cogbill
#use Trees column to get total tree count for each data point
wit <- mutate(wit, Beech=Beech*Trees, Maples=Maples*Trees, Birches=Birches*Trees,Ashs=Ashs*Trees, Hemlock=Hemlock*Trees, Basswood=Basswood*Trees, Elms=Elms*Trees, Pines=Pines*Trees, Hickories=Hickories*Trees, Spruces=Spruces*Trees, Fir=Fir*Trees, Cedar=Cedar*Trees, Oaks=Oaks*Trees, Chestnut=Chestnut*Trees, Ironwoods=Ironwoods*Trees, Poplars=Poplars*Trees, Tamarack=Tamarack*Trees, Cherries=Cherries*Trees, Chamae.is=Chamae.is*Trees, Nyssa=Nyssa*Trees, Juglans=Juglans*Trees, Buttonwood=Buttonwood*Trees, Liriodendron=Liriodendron*Trees, Magnolia=Magnolia*Trees)

sum.wit <- as.numeric(rbind(sum(wit$Beech),sum(wit$Maples),sum(wit$Birches),sum(wit$Ashs),sum(wit$Hemlock),sum(wit$Basswood),sum(wit$Elms),sum(wit$Pines),sum(wit$Hickories),sum(wit$Spruces),sum(wit$Fir),sum(wit$Cedar),sum(wit$Oaks),sum(wit$Chestnut),sum(wit$Ironwoods),sum(wit$Poplars),sum(wit$Tamarack),sum(wit$Cherries),sum(wit$Chamae.is),sum(wit$Nyssa),sum(wit$Juglans),sum(wit$Buttonwood),sum(wit$Liriodendron),sum(wit$Magnolia)))
trees <- c("Beech", "Maples", "Birches", "Ashs", "Hemlock", "Basswood", "Elms", "Pines", "Hickories", "Spruces", "Fir", "Cedar", "Oaks", "Chestnut", "Ironwoods", "Poplars", "Tamarack", "Cherries", "Chamae.is", "Nyssa", "Juglans", "Buttonwood", "Liriodendron", "Magnolia")
sum.wit <- data.frame(cbind(sum.wit, trees))
sum.wit[,1] <- as.numeric(sum.wit[,1])
names(sum.wit) <- c("sum.count", "genus")
sum.wit = arrange(sum.wit, sum.count)

ggplot(data=sum.wit, aes(x=reorder(genus, -sum.count), y=sum.count)) + geom_bar(stat="identity") + ylab("sum of tree count") + theme(axis.title.x = element_blank()) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#pick trees that make up more than 0.5% of total count
sum.wit.05 = mutate(sum.wit, prop = sum.count/sum(sum.wit$sum.count))
sum.wit.05 = filter(sum.wit.05, prop > 0.005)
ggplot(data=sum.wit.05, aes(x=reorder(genus, -prop), y=prop)) + geom_bar(stat="identity") + ylab("proportion of total tree count") + theme(axis.title.x = element_blank()) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#trees to include: "Cedar","Poplars","Ironwoods,Basswood","Elms","Fir","Ashs","Chestnut","Hickories","Spruces","Birches","Pines","Maples","Beech","Oaks", "Cherries" (cherries are included in fia data sorting, so will be included here. Hemlock is not included becasue it is too difficult to model using a such broad definitions of plant funcitonal types. 

#wird species:

#MAGNOLIA
ggplot() + geom_point(data=wit, aes(x=Long., y=Lat., col=Magnolia))#southern species
#HEMLOCK
ggplot() + geom_point(data=wit, aes(x=Long., y=Lat., col=Hemlock))#also would have entire pft to itself...
#JUGLANS
ggplot() + geom_point(data=wit, aes(x=Long., y=Lat., col=Juglans))#might want to consider adding this...
#NYSSA
ggplot() + geom_point(data=wit, aes(x=Long., y=Lat., col=Nyssa))
#TAMERAK
ggplot() + geom_point(data=wit, aes(x=Long., y=Lat., col=Tamarack)) #would have a whole pft to itself...


#to add to FIA data: ironwood/hophornbeam, chestnuts
#birches may also be a problem - paper and yellow are not separated in this dataset but are in very different PFTs

#sort into pfts
wit <- mutate(wit, BNT=Fir+Spruces,
              BNI=Cedar+Pines,
              TBT=Ironwoods+Basswood+Elms+Ashs+Chestnut+Birches+Maples+Beech,
              BBI=Poplars,
              TBI=Hickories+Cherries,
              TBIR=Oaks)
wit <- wit[,c("Ver..2010.09","Lat.","Long.","Trees","BNT","BNI","TBT","BBI","TBI","TBIR")]
names(wit) <- c("state","Lat","Lon","Total","BNT","BNI","TBT","BBI","TBI","TBIR")
#add start and end dates
wit <- wit %>%
  mutate(year_start = case_when(
    state=="NY" ~ 1760,
    state=="VT" ~ 1763,
    state=="MA" ~ 1623,
    state=="ME" ~ 1662,
    state=="CT" ~ 1642,
    state=="RI" ~ 1642,
    state=="NH" ~ 1673
  ))
wit <- wit %>%
  mutate(year_end = case_when(
    state=="NY" ~ 1811,
    state=="VT" ~ 1820,
    state=="MA" ~ 1835,
    state=="ME" ~ 1835,
    state=="CT" ~ 1818,
    state=="RI" ~ 1818,
    state=="NH" ~ 1850
  ))
head(wit)

#standardise
wit = mutate(wit, BNT=BNT/Total, BNI=BNI/Total, TBT=TBT/Total, BBI=BBI/Total, TBI=TBI/Total, TBIR=TBIR/Total)

head(wit)

write.csv(wit, "../data/witness_trees/witness_treees_PFTs.csv")


