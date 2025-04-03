#--------------------------------------------------------
#Tri-variate map - watersheds
#-------------------------------------------------------

#-----------------------------------------------------------------------
#Estimate indicators 
#-------------------------------------------------------------------------
#biodiversity vs flow alteration vs viable protection
library(sf); library(tidyverse); library(spatialEco); library(naniar)

#summaries of the watersheds
huc12 <- st_read("C:/Users/Lise/Documents/GIT/NPRA_NFA_v2/outputs/NPRALayer_huc12_revised_2_28_25.gpkg")
dim(huc12)
sf_use_s2(FALSE)
area <- st_area(huc12) #these are multipolygons
Area <- tapply(area,huc12$HUC_12,sum)
Area <- Area/1000000 #in km2
length(Area)#95144
mean(Area)#96.89271
sd(Area)#69.95261

huc12$HUC6 <- sapply(strsplit(as.character(huc12$HUC_12),"*"),function(x) paste(x[1:6],collapse="",sep=""))
Area_RB <- tapply(area,huc12$HUC6,sum)
Area_RB <- Area_RB/1000000 #in km2
length(Area_RB)#373
mean(Area_RB)#24715.17
sd(Area_RB)#18034.09

#keep only columns of interest
#huc12 <- huc12[,c(1:10,18:19,93:114)]

#keep only CONUS
huc12 <- huc12[!huc12$State%in% c("Alaska","Hawaii"),]
#huc12 <- st_drop_geometry(huc12)

#huc12 %>% replace_with_na_all(condition = ~.x == -999) takes too much time

#-------------------------------------------------------
#estimate biodiv and human pressure indicators using percent rank at national scale
#-----------------------------------------------------
#These two ranking functions implement two slightly different ways to compute a percentile. For each x_i in x:
#cume_dist(x) counts the total number of values less than or equal to x_i, and divides it by the number of observations.
#percent_rank(x) counts the total number of values less than x_i, and divides it by the number of observations minus 1.

summary(huc12$TERich) #
huc12$TERich[huc12$TERich==-999]<- NA
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(BioStateTot = percent_rank(TERich))#richness of imperilled species

summary(huc12$HAI)
huc12$HAI[huc12$HAI==-999]<- NA
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(HydroStateTot = percent_rank(HAI))#hydrological alteration

summary(huc12$gHM)
huc12$gHM[huc12$gHM==-999]<- NA
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(GHMStateTot = percent_rank(gHM))#human footprint along rivers

summary(huc12$IndexLateralC)
huc12$IndexLateralC[huc12$IndexLateralC==-999]<- NA
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(LateralCStateTot = percent_rank(IndexLateralC))#index of lateral connectivity

huc12$LateralCStateTot <- ifelse(is.na(huc12$LateralCStateTot)==T,0,huc12$LateralCStateTot) #add zero otherwise cannot compute aggregated index if no floodplain
summary(huc12$LateralCStateTot)


#fuzzy sum
huc12$AltIndRaw <- apply(st_drop_geometry(huc12)[,c("HydroStateTot","GHMStateTot","LateralCStateTot")],1,fuzzySum)

hist(huc12$BioStateTot)
hist(huc12$HydroStateTot)
hist(huc12$GHMStateTot)
hist(huc12$LateralCStateTot)

#retransform in percentile (to take the top 25%)
huc12 <- huc12 %>%
  mutate(AltIndTot = percent_rank(AltIndRaw))#
hist(huc12$AltIndTot)

#human intactedness is 1 - alteration
huc12$HumanIntactStateTot <- 1 - huc12$AltIndTot
summary(huc12$HumanIntactStateTot)
hist(huc12$HumanIntactStateTot)

##-------------------------------------------------------
#estimate biodiv and human pressure indicators using percent rank of values per state
#-----------------------------------------------------

#percentile scaled per state
huc12 <- huc12 %>%
   group_by(State) %>%
    mutate(BiodState = percent_rank(TERich))#number of imperilled species
         
huc12 <- huc12 %>%
  group_by(State) %>%
  mutate(AltIndPerc = percent_rank(AltIndRaw))

#human intactedness
huc12$HumanIntactState <- 1 - huc12$AltIndPerc
  
#checks
boxplot(huc12$AltIndPerc ~ huc12$State)
boxplot(huc12$BiodState ~ huc12$State)
#plot(huc12$BioIndPerc ~ huc12$AltInd)

#-------------------------------------------------------------
#indicator for drinking water
#-------------------------------------------------------------
huc12$SW_POPServed[huc12$SW_POPServed==-999]<- 0
huc12$DomWellsPopServed[huc12$DomWellsPopServed==-999]<- 0

plot(huc12$SW_POPServed,huc12$DomWellsPopServed)
abline(0,1,col="red")


#add attributes to re-estimate pop served for domestic wells
hucb <- st_read("C:/Users/Lise/Documents/GIT/NPRA_NFA_v2/outputs/RIP_huc12_context.gpkg")
plot(hucb$SW_POP_Per,hucb$DomWells_POP_Per)
#hucb$SW_POP_Per[hucb$SW_POP_Per > 100] <- 100 #cannot use the percentage as is often > 100%
plot(hucb$SW_POP_Per,hucb$DomWells_POP_Per)
abline(0,1,col="red")

#ccompare population of reference
plot(hucb$SW_POPRef,hucb$DomWells_POPRef)
abline(0,1,col="red")

#use the % population served by domestic wells (instead of counts) to re-estimate the number of people given the population of reference
huc12$DomWells_POP_Per <- hucb$DomWells_POP_Per[match(huc12$HUC_12,hucb$HU12)]
huc12$SW_POPRef <- hucb$SW_POPRef[match(huc12$HUC_12,hucb$HU12)]
huc12$DomWellsPopServedL <- huc12$SW_POPRef * huc12$DomWells_POP_Per/100 #use the percentage of pop dependent on wells to estimate the number of people based on census provided per huc12
huc12$DomWellsPopServedL[is.na(huc12$DomWellsPopServedL)==T]<-0

plot(huc12$SW_POPServed,huc12$DomWellsPopServedL)
abline(0,1,col="red")

#huc12$PopDrinkWater <- huc12$SW_POPServed + huc12$DomWellsPopServedL #add both
huc12$PopDrinkWater <- huc12$IMP_R

#transform in percentile (to take the top 25%)
huc12 <- huc12 %>%
  ungroup() %>% 
    mutate(PopDrinkTot = percent_rank(PopDrinkWater))#
hist(huc12$PopDrinkTot)
plot(huc12$PopDrinkWater,huc12$PopDrinkTot,col=factor(huc12$State))

#percentile scaled per state
huc12 <- huc12 %>%
  group_by(State) %>%
  mutate(PopDrinkState = percent_rank(PopDrinkWater))#drinking water dependence

#-------------------------------------------------------------
#indicator for upstream protection
#-------------------------------------------------------------
#watersheds that do not meet the 30% protection threshold
huc12$Viable[huc12$Viable==-999]<- NA
huc12$NoViableLocal <- ifelse(huc12$Viable < 30,1,0)

#watersheds that meet the 30% upstream protection
huc12$ViableUpstream[huc12$ViableUpstream==-999]<- NA
huc12$ViableUpstreamY <- ifelse(huc12$ViableUpstream > 30,1,0)

#watersheds that show higher upstream than local protection
huc12$UvL <- ifelse(huc12$ViableUpstream > huc12$Viable,1,0)
huc12$UvL[is.na(huc12$UvL)] <- 0 #add 0 for isolated basins

#top protected upstream watersheds nationally
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(UpPercTot = percent_rank(ViableUpstream))
  #mutate(UpPercTot=percent_rank(replace(ViableUpstream,ViableUpstream==0,NA)))
#huc12$UpPercTot[huc12$ViableUpstream==0] <- 0 
#huc12$UpPercTot[is.na(huc12$UpPercTot)] <- 1 #add 1 for isolated/source basins (max of protection to reflect priority)
#plot(percent_rank(huc12$ViableUpstream),huc12$UpPercTot,col=factor(huc12$State))
plot(huc12$ViableUpstream,huc12$UpPercTot,col=factor(huc12$State))

#top protected upstream watersheds within states
huc12 <- huc12 %>%
  group_by(State) %>%
  mutate(UpPercState = percent_rank(ViableUpstream))
#huc12$UpPercState[is.na(huc12$UpPercState)] <- 1 #add 1 for isolated/source basins (max of protection to reflect priority)
#plot(percent_rank(huc12$ViableUpstream),huc12$UpPercState,col=factor(huc12$State))
plot(huc12$ViableUpstream,huc12$UpPercState,col=factor(huc12$State))

#lowest protected local watersheds nationally
huc12$NonViable <- 100 - huc12$Viable
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(PercTot = percent_rank(NonViable))
#plot(percent_rank(huc12$NonViable),huc12$PercTot,col=factor(huc12$State))
plot(huc12$NonViable,huc12$PercTot,col=factor(huc12$State))

#top protected local watersheds within states
huc12 <- huc12 %>%
  group_by(State) %>%
  mutate(PercState = percent_rank(NonViable))
plot(percent_rank(huc12$NonViable),huc12$PercState,col=factor(huc12$State))

#indicator of protection (combined)

#option 1: using the sum and assigning arbitrary weights for source waters
#should use the row values as th percentiles are not between 0 and 1 due to the distribution of the variables
huc12$ViableUpstreamP <- huc12$ViableUpstream
#assigning arbitrary weights to source
#huc12$ViableUpstreamP[is.na(huc12$ViableUpstream)]<-75+rnorm(length(huc12$ViableUpstreamP[is.na(huc12$ViableUpstream)]),0.0000001,0.00001) #add 75% for sources so they are not excluded from the analysis
#disregarding sources
huc12$ViableUpstreamP[is.na(huc12$ViableUpstream)]<-0
huc12$ProtectIndRaw <- apply(st_drop_geometry(huc12)[,c("NonViable","ViableUpstreamP")],1,sum) 
hist(huc12$ProtectIndRaw)
#transformer in percentile (to take the top 25%)
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(ProtectIndTot = percent_rank(ProtectIndRaw))#index of protection potential
hist(huc12$ProtectIndTot)
abline(v=0.75)

huc12 <- huc12 %>%
  group_by(State) %>%
  mutate(ProtectIndPerc = percent_rank(ProtectIndRaw))

#checks
plot(huc12$ViableUpstreamP,huc12$NonViable,col=ifelse(huc12$ProtectIndTot >= 0.75,"red","black")) #problematic
plot(huc12$ProtectIndRaw,huc12$PercTot+huc12$UpPercTot)
plot(huc12$ProtectIndRaw~huc12$PercTot)
plot(huc12$ProtectIndRaw~huc12$UpPercTot)

#add source watersheds
ref <- st_read('C:/Users/Lise/Documents/AR/Spatial/NHDPlusNationalData/NationalWBDSnapshot.gdb','WBDSnapshot_National')
#The hydrologic unit type that most closely identifies the watershed.          
# S - "Standard" hydrologic unit - Any land HU with drainage flowing to a single outlet point, excluding non-contributing areas. This includes areas or small triangular wedges between adjacent HU's that remain after classic hydrologic units are delineated.  Some examples include "true", "classic", "composite", and "remnant" hydrologic units.       
# C - "Closed Basin" hydrologic unit - A drainage area that is 100% non-contributing. This means all surface flow is internal, no overland flow leaves the hydrologic unit through the outlet point.       
# F - "Frontal" hydrologic unit - Areas along the coastline of lakes, oceans, bays, etc. that have more than one outlet.  These HU's are predominantly land with some water areas at or near the outlet(s).       
# M - “Multiple Outlet” hydrologic unit An area that has more than one natural outlet; for example, an outlet located on a stream with multiple channels. This does not include frontal or water hydrologic units, hydrologic units with artificial interbasin transfers, drainage outlets through karst or ground-water flow, or outlets that cross a stream with an island. This code should be used only in rare instances.        
# W - "Water" hydrologic unit - Hydrologic units that are predominantly water with adjacent land areas, ex. lake, estuaries, and harbors. 
sources <- ref$HUC_12[which(ref$HU_12_TYPE == "S")] # & ref$NCONTRB_A==0
sources <- sources[! sources %in% c(ref$HU_12_DS)]
huc12$Source <- ifelse(huc12$HUC_12 %in% sources,"Y","N")

#---------------------------------------------------------------
#identifying the top percentile 25%
#--------------------------------------------------------------
#25% lowest alteration
huc12$HuIntacTop25Tot <- ifelse(huc12$HumanIntactStateTot >= 0.75,1,0)
huc12$HuIntacTop25 <- ifelse(huc12$HumanIntactState  >= 0.75,1,0)

#25% highest biodiv
huc12$BioIndTop25 <- ifelse(huc12$BiodState >= 0.75,1,0)
huc12$BioIndTop25Tot <- ifelse(huc12$BioStateTot >= 0.75,1,0)

#25% highest upstream protection
huc12$TopUpstream <- ifelse(huc12$UpPercState >= 0.75,1,0)
huc12$TopUpstreamTot <- ifelse(huc12$UpPercTot >= 0.75,1,0)

#25% highest upstream protection & lowest local protection
#option 1:using combined index
huc12$ProtTop25 <- ifelse(huc12$ProtectIndPerc >= 0.75,1,0)#state
huc12$ProtTop25Tot <- ifelse(huc12$ProtectIndTot >= 0.75,1,0)
table(huc12$ProtTop25Tot,huc12$Source)*100/sum(table(huc12$ProtTop25Tot,huc12$Source)) #the majority of selected watersheds are source

#option 2: more than 30% upstream and less than 30% local OR less than 30% local and source water
huc12$ProtTop25 <- ifelse(huc12$Viable < 30 & huc12$ViableUpstreamP >=30 | huc12$Viable < 30 & huc12$Source == "Y",1,0)#state
huc12$ProtTop25Tot <- ifelse(huc12$Viable < 30 & huc12$ViableUpstreamP >=30 | huc12$Viable < 30 & huc12$Source == "Y",1,0)#national
table(huc12$ProtTop25Tot,huc12$Source)*100/sum(table(huc12$ProtTop25Tot,huc12$Source)) #the majority of selected watersheds are source


#25% highest drinking water
huc12$DrinkTop25 <- ifelse(huc12$PopDrinkState >= 0.75,1,0)
huc12$DrinkTop25Tot <- ifelse(huc12$PopDrinkTot >= 0.75,1,0)

#--------------------------------------------------------------
#estimate priorities
#--------------------------------------------------------------
#estimate priority watersheds
#state-level
huc12$priorityBiod <- 0
# huc12$priority[huc12$Source=="Y" & huc12$HuIntacTop25==1 & huc12$NoViableLocal == 1  & huc12$BioIndTop25 == 1] <- 1
# huc12$priority[huc12$TopUpstream==1 & huc12$HuIntacTop25==1 & huc12$BioIndTop25 == 1] <- 1
huc12$priorityBiod[huc12$HuIntacTop25==1 & huc12$BioIndTop25 == 1] <- 1
table(huc12$priorityBiod)

huc12$priorityDrink <- 0
huc12$priorityDrink[huc12$HuIntacTop25==1 & huc12$DrinkTop25 == 1] <- 1
table(huc12$priorityDrink)

huc12$priority <- 0
huc12$priority[huc12$DrinkTop25==1 & huc12$HuIntacTop25==1 & huc12$BioIndTop25 == 1] <- 1
table(huc12$priority)

#national level
huc12$priorityBiodTot <- 0
huc12$priorityBiodTot[huc12$HuIntacTop25Tot==1 & huc12$BioIndTop25Tot == 1] <- 1 #& huc12$NoViableLocal == 1  
#huc12$priorityTot[huc12$TopUpstreamTot==1 & huc12$HuIntacTop25Tot==1 & huc12$BioIndTop25Tot == 1] <- 1 #
table(huc12$priorityBiodTot)

huc12$priorityDrinkTot <- 0
huc12$priorityDrinkTot[huc12$HuIntacTop25Tot==1 & huc12$DrinkTop25Tot == 1] <- 1 
table(huc12$priorityDrinkTot)

huc12$priorityTot <- 0
huc12$priorityTot[huc12$HuIntacTop25Tot==1 & huc12$BioIndTop25Tot == 1 & huc12$DrinkTop25Tot == 1] <- 1  
table(huc12$priorityTot)

huc12$priorityTotW <- 0
huc12$priorityTotW[huc12$ProtTop25Tot==1 & huc12$HuIntacTop25Tot==1 & huc12$BioIndTop25Tot == 1 & huc12$DrinkTop25Tot == 1] <- 1  
table(huc12$priorityTotW)

table(huc12$HuIntacTop25Tot,useNA="ifany")
table(huc12$HuIntacTop25,useNA="ifany")
table(huc12$BioIndTop25,useNA="ifany")
table(huc12$BioIndTop25Tot,useNA="ifany")
table(huc12$TopUpstream,useNA="ifany")
table(huc12$TopUpstreamTot,useNA="ifany")
table(huc12$ProtTop25,useNA="ifany")
table(huc12$ProtTop25Tot,useNA="ifany")

table(huc12$priority)#377
table(huc12$priority)[2]*100/sum(table(huc12$priority))

table(huc12$priorityTot)#169
table(huc12$priorityTot)[2]*100/sum(table(huc12$priorityTot))#0.2041161

table(huc12$priorityTot,huc12$NoViableLocal) 
table(huc12$priority,huc12$NoViableLocal) 

#----------------------------------------------------------------
#save file
#---------------------------------------------------------------
#fill in NAs
huc12 <- huc12[,!names(huc12) %in% c("Join_Count","Target_FID","total_mi","rightmost.closed","Satisfactory")]

huc12$LateralCStateTot[is.na(huc12$IndexLateralC)] <- NA #it was just to aggregate values

#remove huc12 with no NPRA
huc12 <- huc12[!is.na(huc12$Viable)==T,]

st_write(huc12, "outputs/RIP_huc12_prioritization.gpkg",append=FALSE)  # save as geopackage to save names

# #set NA to -999 to export as shapefile
# myList <- setNames(lapply(vector("list", ncol(huc12)), function(x) x <- -999), names(huc12))
# myList[which(sapply(lapply(huc12, class),'[',1)=="character")] <- "-999"
# huc12 <- huc12 %>% replace_na(myList)
# 
# st_write(huc12, "outputs/RIP_huc12_prioritization.shp",driver = "ESRI Shapefile",append=FALSE)  # 

#--------------------------------------------------------------
#Estimate statistics & summarize protection
#-------------------------------------------------------------
library(ggplot2);library(introdataviz); library(sf)
#huc12_p <- st_read("outputs/RIP_huc12_prioritization.shp")
huc12_p <- st_read("outputs/RIP_huc12_prioritization.gpkg")

length(which(is.na(huc12_p$BioIndTop25Tot) == T))
length(which(is.na(huc12_p$HuIntacTop25Tot) == T))
length(which(is.na(huc12_p$DrinkTop25Tot) == T))

#remove missing values (mostly lake watersheds)
huc12_p <- huc12_p[!is.na(huc12_p$BioIndTop25Tot) == T,]
huc12_p <- huc12_p[!is.na(huc12_p$HuIntacTop25Tot) == T,]
huc12_p <- huc12_p[!is.na(huc12_p$DrinkTop25Tot) == T,]

dim(huc12_p)#80374
N <- dim(huc12_p)[1]

#------------------------------------------
#how many priority watersheds
#------------------------------------------
#state-level
sum(huc12_p$priority)
table(huc12_p$priority,huc12_p$State)

#national level
sum(huc12_p$priorityTot)#443
sum(huc12_p$priorityTot)*100/nrow(huc12_p)#0.5511733
table(huc12_p$priorityTot,huc12_p$State) 

#overlap among indicators (after selecting protection potential)
table(huc12_p$priorityTotW)#165
sum(huc12_p$priorityTotW)*100/nrow(huc12_p)#0.2052903
table(huc12_p$priorityTotW,huc12_p$State) 

#how many are already protected? [0 is protection]
table(huc12_p$priorityTot,huc12_p$NoViableLocal)
#     0     1
#0 11446  68485
#1   185   258
185*100/(185+258)#41.76072

#how many are identified as top protection needs
table(huc12_p$priorityTot,huc12_p$ProtTop25Tot)
#    0     1
#0 47261  32670
#1    278   165
165*100/(278+165) #37.24605

#count of watershed independently for each indicator
table(huc12_p$DrinkTop25Tot) #top drinking
#  0     1 
#60132   20242   
table(huc12_p$DrinkTop25Tot,huc12_p$NoViableLocal)#[0 is protection]
#     0     1
#0  8901  51231
#1  2730  17512
2730*100/(2730+17512)#13.48681

table(huc12_p$DrinkTop25Tot,huc12_p$ProtTop25Tot)#
#    0     1
#0 35665 24467
#1 11874  8368
8368*100/(8368+11874)#41.33979

table(huc12_p$BioIndTop25Tot) #top biodiv
#  0     1 
#68943  11431  
table(huc12_p$BioIndTop25Tot,huc12_p$NoViableLocal)#[0 is protection]
#     0     1
#0  8826 60117
#1  2805  8626
2805*100/(2805+8626)#24.53854
table(huc12_p$BioIndTop25Tot,huc12_p$ProtTop25Tot)#
#    0     1
#0 39964 28979
#1  7575  3856
3856*100/(7575+3856)#33.73283

table(huc12_p$HuIntacTop25Tot)#human intecadness
#   0     1 
#60258  20116  
table(huc12_p$HuIntacTop25Tot,huc12_p$NoViableLocal)#[0 is protection]
#     0     1
#0  6164  54094
#1  5467  14649
5458*100/(5458+14651)#27.14208

table(huc12_p$HuIntacTop25Tot,huc12_p$ProtTop25Tot)#
#    0     1
#0 36664 23594
#1 10875  9241
9241*100/(10875+9241)#45.93856

table(huc12_p$Source)
#N     Y 
#44653 35721 

table(huc12_p$ProtTop25Tot)#top protection needs
#  0     1 
#47539  32835 
#------------------------------------
#are protected watersheds protecting values better (using the 30% local protection?
#------------------------------------
#t-test
t.test(huc12_p$BioStateTot,huc12_p$NoViableLocal) #p-value < 2.2e-16
tapply(huc12_p$BioStateTot,huc12_p$NoViableLocal,mean)
#       0         1 
#0.360860  0.855289  

t.test(huc12_p$HumanIntactStateTot,huc12_p$NoViableLocal) #p-value < 2.2e-16
tapply(huc12_p$HumanIntactStateTot,huc12_p$NoViableLocal,mean)
#     0         1 
#0.5004483  0.8552890

t.test(huc12_p$PopDrinkTot,huc12_p$NoViableLocal) #p-value < 2.2e-16
tapply(huc12_p$PopDrinkTot,huc12_p$NoViableLocal,mean)
#      0         1 
#0.492259  0.855289 

#FIGURE
#using 30% protection
dat_mod <- data.frame(Y = c(huc12_p$PopDrinkTot,huc12_p$BioStateTot,huc12_p$HumanIntactStateTot), 
                      X = c(rep("Drinking value",nrow(huc12_p)),rep("Biodiversity value",nrow(huc12_p)),rep("Human intactness",nrow(huc12_p))),
                      group = c(huc12_p$NoViableLocal,huc12_p$NoViableLocal,huc12_p$NoViableLocal))
dat_mod$group[dat_mod$group==1] <- "≤ 30%"
dat_mod$group[dat_mod$group==0] <- "> 30%"
dat_mod$group <- factor(dat_mod$group)

dat_mod$X <- factor(dat_mod$X,levels=c("Drinking value","Biodiversity value","Human intactness"))
p <- ggplot(dat_mod, aes(x = X, y = Y, fill = group)) +
  introdataviz::geom_split_violin(alpha = .9, trim = T,width=0.7,scale="width") +
  scale_fill_manual(values = c(grey(0.8),"white"), name = "Local protection",guide = guide_legend(reverse = TRUE)) +
#  scale_fill_brewer(palette = "Dark2", name = "Local protection") +
  scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line.x = element_line(colour = "black"),axis.title=element_blank(),axis.ticks.y=element_blank(),axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

ggsave("outputs/PrioritizationCharacteristics.pdf", plot=p,width=5, height=2,dpi = 300)

#Alternative: using the top 25% to protect watersheds
dat_mod <- data.frame(Y = c(huc12_p$PopDrinkTot,huc12_p$BioStateTot,huc12_p$HumanIntactStateTot), 
                      X = c(rep("Drinking value",nrow(huc12_p)),rep("Biodiversity value",nrow(huc12_p)),rep("Human intactness",nrow(huc12_p))),
                      group = c(huc12_p$ProtTop25Tot,huc12_p$ProtTop25Tot,huc12_p$ProtTop25Tot))
dat_mod$group[dat_mod$group==1] <- "Protection needs"
dat_mod$group[dat_mod$group==0] <- "Better protection"
dat_mod$group <- factor(dat_mod$group)

dat_mod$X <- factor(dat_mod$X,levels=c("Drinking value","Biodiversity value","Human intactness"))
p <- ggplot(dat_mod, aes(x = X, y = Y, fill = group)) +
  introdataviz::geom_split_violin(alpha = .9, trim = T,width=0.7,scale="width") +
  scale_fill_manual(values = c(grey(0.8),"white"), name = "Local protection",guide = guide_legend(reverse = TRUE)) +
  #  scale_fill_brewer(palette = "Dark2", name = "Local protection") +
  scale_y_continuous(limits = c(0, 1),expand = c(0, 0)) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),axis.title=element_blank(),axis.ticks.y=element_blank(),axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

ggsave("outputs/PrioritizationCharacteristicsprtocNeeds.pdf", plot=p,width=5, height=2,dpi = 300)

#-----------------------------------------------------
#Venn diagram
#-----------------------------------------------------
require(VennDiagram)

#A = Human intactness
#B = Biodiversity value
#C = Protection need

draw.triple.venn(area1 = table(huc12_p$HuIntacTop25Tot)[2], area2 = table(huc12_p$BioIndTop25Tot)[2], area3 = table(huc12_p$DrinkTop25Tot)[2],
n12 = table(huc12_p$HuIntacTop25Tot,huc12_p$BioIndTop25Tot)[2,2],
n23 = table(huc12_p$BioIndTop25Tot,huc12_p$DrinkTop25Tot)[2,2],
n13 = table(huc12_p$HuIntacTop25Tot,huc12_p$DrinkTop25Tot)[2,2],
n123 = table(huc12_p$priorityTotW)[2],euler.d=T,scale=T, sep.dist = 0.1, rotation.degree = 30,fill=c("#A80084","#00A9E6","#E69800"))

jpeg("outputs/FigVenn.jpeg", units="in", width=3, height=3, res=300, pointsize = 16)

draw.triple.venn(area1 = round(table(huc12_p$HuIntacTop25Tot)[2]*100/sum(table(huc12_p$HuIntacTop25Tot)[2]),1), area2 = round(table(huc12_p$BioIndTop25Tot)[2]*100/sum(table(huc12_p$BioIndTop25Tot)[2]),1), area3 = round(table(huc12_p$DrinkTop25Tot)[2]*100/sum(table(huc12_p$DrinkTop25Tot)[2]),1),
                 n12 = round(table(huc12_p$HuIntacTop25Tot,huc12_p$BioIndTop25Tot)[2,2]*100/sum(table(huc12_p$HuIntacTop25Tot,huc12_p$BioIndTop25Tot)[2,2],table(huc12_p$HuIntacTop25Tot,huc12_p$BioIndTop25Tot)[1,2],table(huc12_p$HuIntacTop25Tot,huc12_p$BioIndTop25Tot)[2,1]),1),
                 n23 = round(table(huc12_p$BioIndTop25Tot,huc12_p$DrinkTop25Tot)[2,2]*100/sum(table(huc12_p$BioIndTop25Tot,huc12_p$DrinkTop25Tot)[2,2],table(huc12_p$BioIndTop25Tot,huc12_p$DrinkTop25Tot)[1,2],table(huc12_p$BioIndTop25Tot,huc12_p$DrinkTop25Tot)[2,1]),1),
                 n13 = round(table(huc12_p$HuIntacTop25Tot,huc12_p$DrinkTop25Tot)[2,2]*100/sum(table(huc12_p$HuIntacTop25Tot,huc12_p$DrinkTop25Tot)[2,2],table(huc12_p$HuIntacTop25Tot,huc12_p$DrinkTop25Tot)[1,2],table(huc12_p$HuIntacTop25Tot,huc12_p$DrinkTop25Tot)[2,1]),1),
                 n123 = round(table(huc12_p$priorityTot)[2]*100/sum(table(huc12_p$priorityTot)),1),euler.d=T,scale=T, sep.dist = 0.1, rotation.degree = 30,fill=c("#A80084","#00A9E6","#E69800"))
dev.off()


jpeg("outputs/FigVennDrinking.jpeg", units="in", width=3, height=3, res=300, pointsize = 16)

draw.triple.venn(area1 = round(table(huc12_p$HuIntacTop25Tot)[2]*100/sum(table(huc12_p$HuIntacTop25Tot)),2), area2 = round(table(huc12_p$DrinkTop25Tot)[2]*100/sum(table(huc12_p$HuIntacTop25Tot)),2), area3 = round(table(huc12_p$ProtTop25Tot)[2]*100/sum(table(huc12_p$HuIntacTop25Tot)),2),
                 n12 = round(table(huc12_p$HuIntacTop25Tot,huc12_p$DrinkTop25Tot)[2,2]*100/sum(table(huc12_p$HuIntacTop25Tot)),2),
                 n23 = round(table(huc12_p$DrinkTop25Tot,huc12_p$ProtTop25Tot)[2,2]*100/sum(table(huc12_p$HuIntacTop25Tot)),2),
                 n13 = round(table(huc12_p$HuIntacTop25Tot,huc12_p$ProtTop25Tot)[2,2]*100/sum(table(huc12_p$HuIntacTop25Tot)),2),
                 n123 = round(table(huc12_p$priorityDrink)[2]*100/sum(table(huc12_p$priorityDrink)),2),euler.d=T,scale=T, sep.dist = 0.1, rotation.degree = 30,fill=c("#A80084","#00A9E6","#E69800"))
dev.off()

#------------------------------------------------------
#Proportional quadrants
#-----------------------------------------------------
library(ggplot2);library(introdataviz); library(sf)
huc12_p <- st_read("outputs/RIP_huc12_prioritization.gpkg")

four_quadrant_facet <- function(x_list, color_sets, col_text="grey") {    
  df_list <- lapply(seq_along(x_list), function(i) {
    x <- x_list[[i]]
    sqx <- sqrt(x) 
    data.frame(
      x = c(sqx[1], -sqx[2], -sqx[3], sqx[4]) / 2, 
      y = c(sqx[1], sqx[2], -sqx[3], -sqx[4]) / 2, 
      size = sqx, 
      label = x,
      quad = factor(1:4),  # Quadrant identifier
      dataset = factor(i)  # Dataset identifier (for faceting)
    )
  })
  
  df_combined <- do.call(rbind, df_list)
  mm <- max(df_combined$size) * 1.1
  
  # Assign different color sets based on dataset
  all_colors <- unlist(color_sets)
  names(all_colors) <- as.character(interaction(rep(seq_along(x_list), each=4), 1:4, sep="_"))
  
  ggplot(df_combined, aes(x=x, y=y, width=size, height=size, fill=quad, group=quad)) +
    geom_tile() +
    geom_text(aes(label=label), col=col_text, size=5) +
    geom_hline(yintercept=0, size=0.8) +
    geom_vline(xintercept=0, size=0.8) +
    facet_wrap(~dataset, ncol=2) +  # Separate plots for each dataset
    scale_fill_manual(values=unlist(color_sets)) +  # Assign different color sets per dataset
    coord_fixed() +
    xlim(c(-mm, mm)) + ylim(c(-mm, mm)) +
    theme_void() +
    theme(legend.position = "none",plot.title = element_blank(),strip.text = element_blank())
}

local <- ifelse(huc12_p$Viable <30,"alow","high")
upstream <- ifelse(huc12_p$ViableUpstream <30,"alow","high")

lu <- table(upstream,local)
N2 <- sum(lu) #42075
lu <- round(lu*100/sum(lu),1)

x1 <- c(lu[2,2],lu[2,1],lu[1,1],lu[1,2])

sl <- table(local[is.na(upstream)==T])
N1 <- sum(sl) #40721
sl <- round(sl*100/sum(sl),1)

x2 <- c(sl[2],sl[1],sl[3],sl[4])

# Define separate color sets for each dataset (4 colors per set)
color_set1 <- c("#593FB3", "#62E7F5","#F2F2F2", "#FF7373")
color_set2 <- c("#0070FF","#FFFFBE","transparent", "transparent")

# Call the function with different datasets and color sets
#four_quadrant_facet(list(x1, x2), list(color_set1, color_set2))
p1 <- four_quadrant_facet(list(x1), list(color_set1))
ggsave("outputs/ProportionsLocalvsUpstream.pdf", plot=p1,width=11, height=4,dpi = 300)

p2 <- four_quadrant_facet(list(x2), list(color_set2))
ggsave("outputs/ProportionsLocalSource.pdf", plot=p2,width=11, height=2,dpi = 300)

#-------------------------------------------------
#Scatterplot
#------------------------------------------------
colo <- with(huc12_p, ifelse(Viable >= 30 & ViableUpstream >= 30, "#593FB3",
                             ifelse(Viable < 30 & ViableUpstream >= 30, "#62E7F5",
                                    ifelse(Viable < 30 & ViableUpstream < 30, "#F2F2F2", "#FF7373"))))

jpeg("outputs/Scatter_UpstreamVsLocal.jpeg", units="in", width=10, height=10, res=300, pointsize = 16)

plot(huc12_p$Viable,huc12_p$ViableUpstream,axes=F,xlim=c(0,100),ylim=c(0,115),xlab="Local watershed protection (% river length)",ylab="Upstream watershed protection (% river length)",cex.lab=1.3,
     bg=colo,pch=21)
points(huc12_p$Viable[huc12_p$priorityTot==1],huc12_p$ViableUpstream[huc12_p$priorityTot==1],col="red3",lwd=3)

axis(1)
axis(2,las=2)

par(xpd=NA)
colo2 <- ifelse(huc12_p$Viable[is.na(huc12_p$ViableUpstream)==T] >= 30, "#0070FF","#FFFFBE")
coord2 <- jitter(rep(120,length(huc12_p$Viable)),amount=16)
points(huc12_p$Viable[is.na(huc12_p$ViableUpstream)==T],coord2[is.na(huc12_p$ViableUpstream)==T],bg=colo2,pch=21,cex=0.5,lwd=0.1)
points(huc12_p$Viable[is.na(huc12_p$ViableUpstream)==T & huc12_p$priorityTot==1],coord2[is.na(huc12_p$ViableUpstream)==T & huc12_p$priorityTot==1],col="red3",lwd=2,cex=0.5)

text(-11,120,"Source 
     watersheds")

dev.off()

#------------------------------------------------------------
standL <- ifelse(huc12_p$Viable >= 30, "high","low")
standU <- ifelse(huc12_p$ViableUpstream >= 30, "high","#low")
standTot <- ifelse(standL == "high" & standU == "high","H","N")
standTot[is.na(huc12_p$ViableUpstream)==T] <- NA
standS <- ifelse(standL == "high" & is.na(huc12_p$ViableUpstream)==T,"H","N")

#drinking water
a1 <- table(huc12_p$DrinkTop25Tot,standTot)
a2 <- table(huc12_p$DrinkTop25Tot,standS)

aa <- table(huc12_p$DrinkTop25Tot,standL)
aa[2,1]*100/sum(aa[2,])

#adequalty protected watersheds
(a1[2,1] + a2[2,1])*100/sum(a1[2,],a2[2,])


#biodiversity
a1 <- table(huc12_p$BioIndTop25Tot,standTot)
a2 <- table(huc12_p$BioIndTop25Tot,standS)

aa <- table(huc12_p$BioIndTop25Tot,standL)
aa[2,1]*100/sum(aa[2,])

#adequalty protected watersheds
(a1[2,1] + a2[2,1])*100/sum(a1[2,],a2[2,])

#human intacdness
a1 <- table(huc12_p$HuIntacTop25Tot,standTot)
a2 <- table(huc12_p$HuIntacTop25Tot,standS)

aa <- table(huc12_p$HuIntacTop25Tot,standL)
aa[2,1]*100/sum(aa[2,])

#adequalty protected watersheds
(a1[2,1] + a2[2,1])*100/sum(a1[2,],a2[2,])


#Total
a1 <- table(huc12_p$priorityTot,standTot)
a2 <- table(huc12_p$priorityTot,standS)

aa <- table(huc12_p$priorityTot,standL)
aa[2,1]*100/sum(aa[2,])

(a1[2,1] + a2[2,1])*100/sum(a1[2,],a2[2,])


#protection potential

