#--------------------------------------------------------
#Identifying gaps in protection & opportunities - watersheds
#-------------------------------------------------------

#Lise Comte, April 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

library(sf); library(tidyverse); library(spatialEco); library(naniar)

huc12 <- st_read("data/NPRALayer_huc12_revised_2_28_25.gpkg")

#summaries of the watersheds
dim(huc12)
sf_use_s2(FALSE)
area <- st_area(huc12) #these are multipolygons
Area <- tapply(area,huc12$HUC_12,sum)
Area <- Area/1000000 #in km2
length(Area)
mean(Area)
sd(Area)

#summaries of the river basins
huc12$HUC6 <- sapply(strsplit(as.character(huc12$HUC_12),"*"),function(x) paste(x[1:6],collapse="",sep=""))
Area_RB <- tapply(area,huc12$HUC6,sum)
Area_RB <- Area_RB/1000000 #in km2
length(Area_RB)
mean(Area_RB)
sd(Area_RB)

#keep only CONUS
huc12 <- huc12[!huc12$State%in% c("Alaska","Hawaii"),]

#-----------------------------------------------------------------------
#Estimate indicators of conservation value
#biodiversity vs habitat intactness vs drinking water dependence

#richness of imperiled species
summary(huc12$TERich) #
huc12$TERich[huc12$TERich==-999]<- NA
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(BioStateTot = percent_rank(TERich))

hist(huc12$BioStateTot)

#flow alteration
summary(huc12$HAI)
huc12$HAI[huc12$HAI==-999]<- NA
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(HydroStateTot = percent_rank(HAI))

#human developement along rivers
summary(huc12$gHM)
huc12$gHM[huc12$gHM==-999]<- NA
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(GHMStateTot = percent_rank(gHM))

#index of lateral discconnection
summary(huc12$IndexLateralC)
huc12$IndexLateralC[huc12$IndexLateralC==-999]<- NA
huc12 <- huc12 %>%
  ungroup() %>% 
  mutate(LateralCStateTot = percent_rank(IndexLateralC))

#replace NA by zero (otherwise cannot compute aggregated index if no floodplain)
huc12$LateralCStateTot <- ifelse(is.na(huc12$LateralCStateTot)==T,0,huc12$LateralCStateTot) 
summary(huc12$LateralCStateTot)

#fuzzy sum habitat intactness
huc12$AltIndRaw <- apply(st_drop_geometry(huc12)[,c("HydroStateTot","GHMStateTot","LateralCStateTot")],1,fuzzySum)

#visualization individual indicators
hist(huc12$HydroStateTot)
hist(huc12$GHMStateTot)
hist(huc12$LateralCStateTot)

#retransform to percentile 
huc12 <- huc12 %>%
  mutate(AltIndTot = percent_rank(AltIndRaw))#
hist(huc12$AltIndTot)

#human intactedness is 1 - alteration
huc12$HumanIntactStateTot <- 1 - huc12$AltIndTot
summary(huc12$HumanIntactStateTot)
hist(huc12$HumanIntactStateTot)

#drinking water importance
huc12$PopDrinkWater <- huc12$IMP_R

#transform to percentile (to take the top 25%)
huc12 <- huc12 %>%
  ungroup() %>% 
    mutate(PopDrinkTot = percent_rank(PopDrinkWater))#

hist(huc12$PopDrinkTot)


#identify source watersheds
#data available for download at https://www.epa.gov/waterdata/nhdplus-national-data
ref <- st_read('data/NHDPlusNationalData/NationalWBDSnapshot.gdb','WBDSnapshot_National')
#The hydrologic unit type that most closely identifies the watershed.          
# S - "Standard" hydrologic unit - Any land HU with drainage flowing to a single outlet point, excluding non-contributing areas. This includes areas or small triangular wedges between adjacent HU's that remain after classic hydrologic units are delineated.  Some examples include "true", "classic", "composite", and "remnant" hydrologic units.       
# C - "Closed Basin" hydrologic unit - A drainage area that is 100% non-contributing. This means all surface flow is internal, no overland flow leaves the hydrologic unit through the outlet point.       
# F - "Frontal" hydrologic unit - Areas along the coastline of lakes, oceans, bays, etc. that have more than one outlet.  These HU's are predominantly land with some water areas at or near the outlet(s).       
# M - “Multiple Outlet” hydrologic unit An area that has more than one natural outlet; for example, an outlet located on a stream with multiple channels. This does not include frontal or water hydrologic units, hydrologic units with artificial interbasin transfers, drainage outlets through karst or ground-water flow, or outlets that cross a stream with an island. This code should be used only in rare instances.        
# W - "Water" hydrologic unit - Hydrologic units that are predominantly water with adjacent land areas, ex. lake, estuaries, and harbors. 
sources <- ref$HUC_12[which(ref$HU_12_TYPE == "S")] 
sources <- sources[! sources %in% c(ref$HU_12_DS)]
huc12$Source <- ifelse(huc12$HUC_12 %in% sources,"Y","N")

#---------------------------------------------------------------
#identifying the top 25%

#25% lowest alteration
huc12$HuIntacTop25Tot <- ifelse(huc12$HumanIntactStateTot >= 0.75,1,0)
huc12$HuIntacTop25 <- ifelse(huc12$HumanIntactState  >= 0.75,1,0)

#25% highest biodiv
huc12$BioIndTop25 <- ifelse(huc12$BiodState >= 0.75,1,0)
huc12$BioIndTop25Tot <- ifelse(huc12$BioStateTot >= 0.75,1,0)

#25% highest upstream protection
huc12$TopUpstream <- ifelse(huc12$UpPercState >= 0.75,1,0)
huc12$TopUpstreamTot <- ifelse(huc12$UpPercTot >= 0.75,1,0)

#25% highest drinking water
huc12$DrinkTop25 <- ifelse(huc12$PopDrinkState >= 0.75,1,0)
huc12$DrinkTop25Tot <- ifelse(huc12$PopDrinkTot >= 0.75,1,0)

#estimate priority watersheds
huc12$priorityTot <- 0
huc12$priorityTot[huc12$HuIntacTop25Tot==1 & huc12$BioIndTop25Tot == 1 & huc12$DrinkTop25Tot == 1] <- 1  
table(huc12$priorityTot)

table(huc12$priorityTot)
table(huc12$priorityTot)[2]*100/sum(table(huc12$priorityTot))

#fill in NAs
huc12 <- huc12[,!names(huc12) %in% c("Join_Count","Target_FID","total_mi","rightmost.closed","Satisfactory")]
huc12$LateralCStateTot[is.na(huc12$IndexLateralC)] <- NA #it was just to aggregate values

#save file
st_write(huc12, "outputs/RIP_huc12_prioritization.gpkg",append=FALSE)  

#--------------------------------------------------------------
#Estimate statistics & summarize protection

library(ggplot2);library(introdataviz); library(sf)

huc12_p <- st_read("outputs/RIP_huc12_prioritization.gpkg")

#remove missing values (lake watersheds)
huc12_p <- huc12_p[!is.na(huc12_p$BioIndTop25Tot) == T,]
huc12_p <- huc12_p[!is.na(huc12_p$HuIntacTop25Tot) == T,]
huc12_p <- huc12_p[!is.na(huc12_p$DrinkTop25Tot) == T,]

dim(huc12_p)
N <- dim(huc12_p)[1]

#------------------------------------------
#How many priority watersheds?
sum(huc12_p$priorityTot)#
sum(huc12_p$priorityTot)*100/nrow(huc12_p)
table(huc12_p$priorityTot,huc12_p$State) 

#-----------------------------------------------------
#Venn diagram

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

#------------------------------------------------------
#Proportional quadrants

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
#Summary statistics
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

#adequatly protected watersheds
(a1[2,1] + a2[2,1])*100/sum(a1[2,],a2[2,])


#biodiversity
a1 <- table(huc12_p$BioIndTop25Tot,standTot)
a2 <- table(huc12_p$BioIndTop25Tot,standS)

aa <- table(huc12_p$BioIndTop25Tot,standL)
aa[2,1]*100/sum(aa[2,])

#adequatly protected watersheds
(a1[2,1] + a2[2,1])*100/sum(a1[2,],a2[2,])

#human intactdness
a1 <- table(huc12_p$HuIntacTop25Tot,standTot)
a2 <- table(huc12_p$HuIntacTop25Tot,standS)

aa <- table(huc12_p$HuIntacTop25Tot,standL)
aa[2,1]*100/sum(aa[2,])

#adequatly protected watersheds
(a1[2,1] + a2[2,1])*100/sum(a1[2,],a2[2,])


#Total
a1 <- table(huc12_p$priorityTot,standTot)
a2 <- table(huc12_p$priorityTot,standS)

aa <- table(huc12_p$priorityTot,standL)
aa[2,1]*100/sum(aa[2,])

(a1[2,1] + a2[2,1])*100/sum(a1[2,],a2[2,])

