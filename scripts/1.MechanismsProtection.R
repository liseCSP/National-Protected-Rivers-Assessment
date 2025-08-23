#---------------------------------------------
#Summary mechanisms & Protected Rivers Index
#---------------------------------------------
#Lise Comte, April 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

#Data can be found at: https://figshare.com/s/62d2f6da57b9526a9aee

#----------------- 
#Summary protection by mechanisms
library(dplyr); library(plotrix)

net_protect_seg_fin <- read.csv("outputs/Table_protection_segments_RIPAllCombined.csv") 

#summarizing protection for different individual mechanisms of protection
onrw_mi <- sum(as.vector(net_protect_seg_fin$ONRW_Len_m),na.rm=T)
nws_mi <- sum(as.vector(net_protect_seg_fin$NWS_Len_m),na.rm=T)
sws_mi <- sum(as.vector(net_protect_seg_fin$SWS_Len_m),na.rm=T)
eli_mi <- sum(as.vector(net_protect_seg_fin$NWS_Eli_Len_m),na.rm=T)
otrw_mi <- sum(as.vector(net_protect_seg_fin$OTRW_Len_m),na.rm=T)
onrw_mi <- onrw_mi+ otrw_mi

scenicO_mi <- sum(as.vector(net_protect_seg_fin$FPA_Len_m),na.rm=T)
rfpa_mi <- sum(as.vector(net_protect_seg_fin$RFPA_Len_m),na.rm=T)
wpa_mi <- sum(as.vector(net_protect_seg_fin$WPA_Len_m),na.rm=T)
fam_mi <- sum(as.vector(net_protect_seg_fin$FAM_Len_m),na.rm=T)

iucn1_mi <- sum(as.vector(net_protect_seg_fin$IUCNI_Len_m),na.rm=T)
iucn2_mi <- sum(as.vector(net_protect_seg_fin$IUCNII_Len_m),na.rm=T)
iucn3_mi <- sum(as.vector(net_protect_seg_fin$IUCNIII_Len_m),na.rm=T)
iucn4_mi <- sum(as.vector(net_protect_seg_fin$IUCNIV_Len_m),na.rm=T)
iucn5_mi <- sum(as.vector(net_protect_seg_fin$IUCNV_Len_m),na.rm=T)
iucn6_mi <- sum(as.vector(net_protect_seg_fin$IUCNVI_Len_m),na.rm=T)
iucn_mi <- iucn3_mi+iucn4_mi+iucn5_mi+iucn6_mi
iucnStrict_mi <- iucn1_mi+iucn2_mi

iucn <- data.frame(IUCN_Len_m = c(net_protect_seg_fin$IUCNI_Len_m,net_protect_seg_fin$IUCNII_Len_m,net_protect_seg_fin$IUCNIII_Len_m,net_protect_seg_fin$IUCNIV_Len_m,
                                  net_protect_seg_fin$IUCNV_Len_m,net_protect_seg_fin$IUCNVI_Len_m),
                   IUCN_Des_Tp = c(net_protect_seg_fin$IUCNI_Des_Tp,net_protect_seg_fin$IUCNII_Des_Tp,net_protect_seg_fin$IUCNIII_Des_Tp,net_protect_seg_fin$IUCNIV_Des_Tp,
                                   net_protect_seg_fin$IUCNV_Des_Tp,net_protect_seg_fin$IUCNVI_Des_Tp))
IS_mi <- tapply(iucn$IUCN_Len_m[iucn$IUCN_Des_Tp %in% c('Research Natural Area','State Wilderness','Wilderness Area','Wilderness Study Area',"National Park")],iucn$IUCN_Des_Tp[iucn$IUCN_Des_Tp %in% c('Research Natural Area','State Wilderness','Wilderness Area','Wilderness Study Area',"National Park")],sum)
IP_mi <- tapply(iucn$IUCN_Len_m[!iucn$IUCN_Des_Tp %in% c('Research Natural Area','State Wilderness','Wilderness Area','Wilderness Study Area',"National Park")],iucn$IUCN_Des_Tp[!iucn$IUCN_Des_Tp %in% c('Research Natural Area','State Wilderness','Wilderness Area','Wilderness Study Area',"National Park")],sum)
names(IP_mi)[IP_mi < (100000/0.621371)] <- "Other" #other if less than 100 miles

critic_mi <- sum(as.vector(net_protect_seg_fin$CritHabESA_Len_m),na.rm=T)

buff_mi <- sum(as.vector(net_protect_seg_fin$BufferRiparian_Len_m),na.rm=T)

gap3strict_mi <- sum(as.vector(net_protect_seg_fin$Gap3_Len_m[net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")]))
gap3_mi <- sum(as.vector(net_protect_seg_fin$Gap3_Len_m[!net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")]))
MUS_mi <- tapply(net_protect_seg_fin$Gap3_Len_m[net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")],net_protect_seg_fin$Gap3_Des_Tp[net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")],sum)
MU_mi <- tapply(net_protect_seg_fin$Gap3_Len_m[!net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")],net_protect_seg_fin$Gap3_Des_Tp[!net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")],sum)

#add other conservation areas and UA
OTH_mi <- tapply(net_protect_seg_fin$IUCNOTH_Len_m,net_protect_seg_fin$IUCNOTH_Des_Tp,sum,na.rm=T)
MU_mi <- c(MU_mi,OTH_mi)
MU_mi <- tapply(MU_mi, names(MU_mi), sum)

names(MU_mi)[MU_mi < (100000/0.621371)] <- "Other"#other if less than 100miles

KW_mi <- sum(as.vector(net_protect_seg_fin$KW_Len_m),na.rm=T)

protect <- data.frame(Mechanism = c("ONRW/OTRW","NationalW&S","StateW&S","EligibleStudyW&S","FreshwaterPA","RiparianPA","NWFPKeyWatersheds","Buffer","WatershedPA","FishingManagementPA","CriticalHabitat",names(IS_mi),names(IP_mi),names(MUS_mi),names(MU_mi)),
                      RiverKM = c(onrw_mi,nws_mi,sws_mi,eli_mi,scenicO_mi,rfpa_mi,KW_mi,buff_mi,wpa_mi,fam_mi,critic_mi,IS_mi,IP_mi,MUS_mi,MU_mi),
                      Label = c("ONRW/OTRW","National Wild & Scenic Rivers","State Wild & Scenic Rivers","Eligible Wild & Scenic Rivers",
                                "Scenic Riverways, National Rivers & Recreation Areas","Riparian, Floodplain, Wetland Protection Areas",
                                "Northwest Forest Plan - Key Watersheds","Riparian Buffers","Watershed Protection Areas",
                                "Fishing Management Areas","Critical Habitat (ESA)",names(IS_mi),names(IP_mi),names(MUS_mi),names(MU_mi)),
                      Category = c("River conservation system","River conservation system","River conservation system","River conservation system",
                                   "River conservation system","Riparian & floodplain protection",
                                   "Riparian & floodplain protection","Riparian & floodplain protection","Riparian & floodplain protection",
                                   "Riparian & floodplain protection","Policies that focus on endangered species",rep("Incidental protected areas (strict)",length(IS_mi)),
                                   rep("Incidental protected areas",length(IP_mi)),
                                   rep("Multiple use landscapes/riverscapes (special management)",length(MUS_mi)),
                                   rep("Multiple use landscapes/riverscapes",length(MU_mi))))

#collapse all the categories with less than 100k
protect <- aggregate(RiverKM~ Label+Category,protect,sum,na.rm=T)

#add protection in %
protect$Percent <- round(protect$RiverKM*100/sum(net_protect_seg_fin$Total_Length_m,na.rm=T),2)

#transform to km
protect$RiverKM <- protect$RiverKM/1000

#transform to miles
protect$RiverMI <- protect$RiverKM * 0.621371

#set up colors
protect$cols <- rep("#fcae91",nrow(protect))
protect$cols[protect$Category %in% c("River conservation system")]<-"#628dbd"
protect$cols[protect$Category %in% c("Riparian & floodplain protection")]<-"chartreuse4"
protect$cols[protect$Category %in% c("Policies that focus on endangered species")]<-"#c06c84"
protect$cols[protect$Category %in% c("Incidental protected areas (strict)")]<-"#fa8072"
protect$cols[protect$Category %in% c("Multiple use landscapes/riverscapes (special management)")]<-grey(0.3)
protect$cols[protect$Category %in% c("Multiple use landscapes/riverscapes")]<-"grey"

# #reorder
protect <- protect %>% arrange(factor(Category,levels=c("River conservation system","Riparian & floodplain protection","Policies that focus on endangered species",
                                                        "Incidental protected areas (strict)","Incidental protected areas","Multiple use landscapes/riverscapes (special management)",
                                                        "Multiple use landscapes/riverscapes")), -Percent)

write.csv(protect,"outputs/Summary_protection_segments_IndependentCategory.csv")

#------------------------------------------------------------------------------------
#ONLY CONUS
#------------------------------------------------------------------------------------

#summarizing protection for different mechanisms
net_protect_seg_fin <- net_protect_seg_fin[!net_protect_seg_fin$State %in% c("Alaska","Hawaii"),]

#summarizing protection for different mechanisms
onrw_mi <- sum(as.vector(net_protect_seg_fin$ONRW_Len_m),na.rm=T)
nws_mi <- sum(as.vector(net_protect_seg_fin$NWS_Len_m),na.rm=T)
sws_mi <- sum(as.vector(net_protect_seg_fin$SWS_Len_m),na.rm=T)
eli_mi <- sum(as.vector(net_protect_seg_fin$NWS_Eli_Len_m),na.rm=T)
otrw_mi <- sum(as.vector(net_protect_seg_fin$OTRW_Len_m),na.rm=T)
onrw_mi <- onrw_mi+ otrw_mi

scenicO_mi <- sum(as.vector(net_protect_seg_fin$FPA_Len_m),na.rm=T)
rfpa_mi <- sum(as.vector(net_protect_seg_fin$RFPA_Len_m),na.rm=T)
wpa_mi <- sum(as.vector(net_protect_seg_fin$WPA_Len_m),na.rm=T)
fam_mi <- sum(as.vector(net_protect_seg_fin$FAM_Len_m),na.rm=T)

iucn1_mi <- sum(as.vector(net_protect_seg_fin$IUCNI_Len_m),na.rm=T)
iucn2_mi <- sum(as.vector(net_protect_seg_fin$IUCNII_Len_m),na.rm=T)
iucn3_mi <- sum(as.vector(net_protect_seg_fin$IUCNIII_Len_m),na.rm=T)
iucn4_mi <- sum(as.vector(net_protect_seg_fin$IUCNIV_Len_m),na.rm=T)
iucn5_mi <- sum(as.vector(net_protect_seg_fin$IUCNV_Len_m),na.rm=T)
iucn6_mi <- sum(as.vector(net_protect_seg_fin$IUCNVI_Len_m),na.rm=T)
iucn7_mi <- sum(as.vector(net_protect_seg_fin$IUCNOTH_Len_m),na.rm=T)
iucn_mi <- iucn3_mi+iucn4_mi+iucn5_mi+iucn6_mi
iucnStrict_mi <- iucn1_mi+iucn2_mi

iucn <- data.frame(IUCN_Len_m = c(net_protect_seg_fin$IUCNI_Len_m,net_protect_seg_fin$IUCNII_Len_m,net_protect_seg_fin$IUCNIII_Len_m,net_protect_seg_fin$IUCNIV_Len_m,
                                  net_protect_seg_fin$IUCNV_Len_m,net_protect_seg_fin$IUCNVI_Len_m),
                   IUCN_Des_Tp = c(net_protect_seg_fin$IUCNI_Des_Tp,net_protect_seg_fin$IUCNII_Des_Tp,net_protect_seg_fin$IUCNIII_Des_Tp,net_protect_seg_fin$IUCNIV_Des_Tp,
                                   net_protect_seg_fin$IUCNV_Des_Tp,net_protect_seg_fin$IUCNVI_Des_Tp))
IS_mi <- tapply(iucn$IUCN_Len_m[iucn$IUCN_Des_Tp %in% c('Research Natural Area','State Wilderness','Wilderness Area','Wilderness Study Area',"National Park")],iucn$IUCN_Des_Tp[iucn$IUCN_Des_Tp %in% c('Research Natural Area','State Wilderness','Wilderness Area','Wilderness Study Area',"National Park")],sum)
IP_mi <- tapply(iucn$IUCN_Len_m[!iucn$IUCN_Des_Tp %in% c('Research Natural Area','State Wilderness','Wilderness Area','Wilderness Study Area',"National Park")],iucn$IUCN_Des_Tp[!iucn$IUCN_Des_Tp %in% c('Research Natural Area','State Wilderness','Wilderness Area','Wilderness Study Area',"National Park")],sum)
names(IP_mi)[IP_mi < (100000/0.621371)] <- "Other" #other if less than 100 miles

critic_mi <- sum(as.vector(net_protect_seg_fin$CritHabESA_Len_m),na.rm=T)

buff_mi <- sum(as.vector(net_protect_seg_fin$BufferRiparian_Len_m),na.rm=T)

gap3strict_mi <- sum(as.vector(net_protect_seg_fin$Gap3_Len_m[net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")]))
gap3_mi <- sum(as.vector(net_protect_seg_fin$Gap3_Len_m[!net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")]))
MUS_mi <- tapply(net_protect_seg_fin$Gap3_Len_m[net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")],net_protect_seg_fin$Gap3_Des_Tp[net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")],sum)
MU_mi <- tapply(net_protect_seg_fin$Gap3_Len_m[!net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")],net_protect_seg_fin$Gap3_Des_Tp[!net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")],sum)

#add other conservation areas and UA
OTH_mi <- tapply(net_protect_seg_fin$IUCNOTH_Len_m,net_protect_seg_fin$IUCNOTH_Des_Tp,sum,na.rm=T)
MU_mi <- c(MU_mi,OTH_mi)
MU_mi <- tapply(MU_mi, names(MU_mi), sum)

names(MU_mi)[MU_mi < (100000/0.621371)] <- "Other" #other if less than 100 miles


KW_mi <- sum(as.vector(net_protect_seg_fin$KW_Len_m),na.rm=T)

protect <- data.frame(Mechanism = c("ONRW/OTRW","NationalW&S","StateW&S","EligibleStudyW&S","FreshwaterPA","RiparianPA","NWFPKeyWatersheds","Buffer","WatershedPA","FishingManagementPA","CriticalHabitat",names(IS_mi),names(IP_mi),names(MUS_mi),names(MU_mi)),
                      RiverKM = c(onrw_mi,nws_mi,sws_mi,eli_mi,scenicO_mi,rfpa_mi,KW_mi,buff_mi,wpa_mi,fam_mi,critic_mi,IS_mi,IP_mi,MUS_mi,MU_mi),
                      Label = c("ONRW/OTRW","National Wild & Scenic Rivers","State Wild & Scenic Rivers","Eligible Wild & Scenic Rivers",
                                "Scenic Riverways, National Rivers & Recreation Areas","Riparian, Floodplain, Wetland Protection Areas",
                                "Northwest Forest Plan - Key Watersheds","Riparian Buffers","Watershed Protection Areas",
                                "Fishing Management Areas","Critical Habitat (ESA)",names(IS_mi),names(IP_mi),names(MUS_mi),names(MU_mi)),
                      Category = c("River conservation system","River conservation system","River conservation system","River conservation system",
                                   "River conservation system","Riparian & floodplain protection",
                                   "Riparian & floodplain protection","Riparian & floodplain protection","Riparian & floodplain protection",
                                   "Riparian & floodplain protection","Policies that focus on endangered species",rep("Incidental protected areas (strict)",length(IS_mi)),
                                   rep("Incidental protected areas",length(IP_mi)),
                                   rep("Multiple use landscapes/riverscapes (special management)",length(MUS_mi)),
                                   rep("Multiple use landscapes/riverscapes",length(MU_mi))))

#collapse all the categories 
protect <- aggregate(RiverKM~ Label+Category,protect,sum,na.rm=T)

#add protection in %
protect$Percent <- round(protect$RiverKM*100/sum(net_protect_seg_fin$Total_Length_m,na.rm=T),2)

#transform to km
protect$RiverKM <- protect$RiverKM/1000

#transform to miles
protect$RiverMI <- protect$RiverKM * 0.621371


#set up colors
protect$cols <- rep("#fcae91",nrow(protect))
protect$cols[protect$Category %in% c("River conservation system")]<-"#628dbd"
protect$cols[protect$Category %in% c("Riparian & floodplain protection")]<-"chartreuse4"
protect$cols[protect$Category %in% c("Policies that focus on endangered species")]<-"#c06c84"
protect$cols[protect$Category %in% c("Incidental protected areas (strict)")]<-"#fa8072"
protect$cols[protect$Category %in% c("Multiple use landscapes/riverscapes (special management)")]<-grey(0.3)
protect$cols[protect$Category %in% c("Multiple use landscapes/riverscapes")]<-"grey"

# #reorder
protect <- protect %>% arrange(factor(Category,levels=c("River conservation system","Riparian & floodplain protection","Policies that focus on endangered species",
                                                        "Incidental protected areas (strict)","Incidental protected areas","Multiple use landscapes/riverscapes (special management)",
                                                        "Multiple use landscapes/riverscapes")), -Percent)


write.csv(protect,"outputs/Summary_protection_segments_IndependentCategory_CONUS.csv")


#----------------
#Formatting data
protect <- read.csv("data/Summary_protection_segments_IndependentCategory.csv")
protect$Percent <- round(protect$Percent,1)
protectC <- read.csv("data/Summary_protection_segments_IndependentCategory_CONUS.csv")
protectC$Percent <- round(protectC$Percent,1)

#clean legends
protectC$Category <- gsub("River conservation system","River conservation",protectC$Category)
protectC$Category <- gsub("Riparian & floodplain protection","Riparian & floodplain conservation",protectC$Category)
protectC$Category <- gsub("Incidental protected areas$","Incidental protected areas (other)",protectC$Category)
protectC$Category <- gsub("Multiple use landscapes/riverscapes$","Multiple use (other)",protectC$Category)
protectC$Category <- gsub("landscapes/riverscapes","",protectC$Category)
protectC$Category <- gsub("  "," ",protectC$Category)

protect$Category <- gsub("River conservation system","River conservation",protect$Category)
protect$Category <- gsub("Riparian & floodplain protection","Riparian & floodplain conservation",protect$Category)
protect$Category <- gsub("Incidental protected areas$","Incidental protected areas (other)",protect$Category)
protect$Category <- gsub("Multiple use landscapes/riverscapes$","Multiple use (other)",protect$Category)
protect$Category <- gsub("landscapes/riverscapes","",protect$Category)
protect$Category <- gsub("  "," ",protect$Category)

protect$Category <- gsub("Incidental","Terrestrial",protect$Category)
protectC$Category <- gsub("Incidental","Terrestrial",protectC$Category)


#Save Table 1
pp <- aggregate(.~ Category,sum,data=protect[,c("Category", "RiverKM","Percent","RiverMI")])
ppC <- aggregate(.~ Category,sum,data=protectC[,c("Category", "RiverKM","Percent","RiverMI")])
names(ppC) <- paste0("CONUS.",names(ppC))

pp <- cbind(pp,ppC[,c("CONUS.RiverKM","CONUS.Percent","CONUS.RiverMI")])
write.csv(pp,"outputs/Table1_summary_mechanisms.csv")

#-------------------------------------------------
#Summary protection by PRI classes
net_protect_seg_fin <- read.csv("outputs/Table_protection_segments_RIPAllCombined.csv")

L_P <- tapply(net_protect_seg_fin$OverallProtection_Len_m,
              factor(net_protect_seg_fin$RIP_Class,levels=c("Unprotected","Class 4","Class 3","Class 2","Class 1")),sum,na.rm=T)

#add unprotected manually (for segments that are only partially protected)
L_P[1] <- sum(net_protect_seg_fin$Total_Length_m-net_protect_seg_fin$OverallProtection_Len_m,na.rm=T)

L_P <- L_P/1000
L_P_mi <- L_P  * 0.621371
L_P_per <- L_P*100/sum(net_protect_seg_fin$Total_Length_m/1000,na.rm=T)


#for CONUS
L_P_conter <- tapply(net_protect_seg_fin$OverallProtection_Len_m[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],
                     factor(net_protect_seg_fin$RIP_Class[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],levels=c("Unprotected","Class 4","Class 3","Class 2","Class 1")),sum,na.rm=T)
#add unprotected manually
L_P_conter[1] <- sum(net_protect_seg_fin$Total_Length_m[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")]-net_protect_seg_fin$OverallProtection_Len_m[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],na.rm=T)

L_P_conter <- L_P_conter /1000
L_P_mi_conter <- L_P_conter  * 0.621371
L_P_per_conter <- L_P_conter*100/sum(net_protect_seg_fin$Total_Length_m[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")]/1000,na.rm=T)

RIPRivers <- data.frame(ProtectedMilesCONUS=L_P_mi_conter,ProtectedMilesUS=L_P_mi,
                        ProtectedKmCONUS=L_P_conter,ProtectedKmUS=L_P,
                        ProtectedPerCONUS=L_P_per_conter,ProtectedPerUS=L_P_per)
write.csv(RIPRivers,"outputs/Results_RiversRIP.csv")

#-------------------------------------------------
#Barplot PRI results
library(reshape2); library(dplyr); library(RColorBrewer); library(scales); library(sf); library(MESS)
library(ggplot2); library(waffle)

RIPRivers <- read.csv("data/Results_RiversRIP.csv")

jpeg("outputs/barplotPRI.jpeg", units="in", width=6, height=9, res=300, pointsize = 15)
par(mar=c(6,7,1,1))
b <- barplot(RIPRivers$ProtectedKmUS,col="white",ylab="",las=2,cex.lab=1.3,ylim=c(0,5000000),border=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),axes=F)
barplot(RIPRivers$ProtectedKmCONUS,col=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),border=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),add=T,axes=F,names="")
axis(2,pos=0,las=2,at=seq(0,5000000,1000000),scales::comma(seq(0,5000000,1000000)))
mtext(side=2,"River Length (km)",cex=1.4,line=5.4)
par(xpd=NA)
text((b+0.25),rep(-100000,length(b)),c("No protection","Inadequate","Limited","Effective","Comprehensive"),srt=45,pos=2)
per <- round(RIPRivers$ProtectedPerUS,1)
text(b,RIPRivers$ProtectedKmUS+60000/0.6214,paste0(format(per,nsmall=1),"%"),col=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),cex=1)
per_conter <- round(RIPRivers$ProtectedPerCONUS,1)
text(b,(RIPRivers$ProtectedKmCONUS-45000/0.6214)[1:4],paste0(format(per_conter,nsmall=1),""),col="white",cex=1)
rect(4,2500000/0.6214,4.6,2800000/0.6214,border=grey(0.4))
text(4.7,2700000/0.6214,"U.S.",adj=0,col=grey(0.4))
rect(4,2500000/0.6214,4.6,2600000/0.6214,col=grey(0.4),border=grey(0.5))
text(4.7,2550000/0.6214,"CONUS",adj=0,col=grey(0.4))
lines(c(b[3]-0.5,b[5]+0.5),c(750000/0.6214,750000/0.6214),lwd=2)
text(b[4],850000/0.6214,"Viable protection",font=3,cex=1.2)
dev.off()


#-------------------------------------------
#Network of mechanisms
library(sf); library(reshape2)
net <- st_read("outputs/NPRALayer_segment.gpkg")
net <- st_drop_geometry(net)

net$PRI_Class <- ifelse(net$PRI ==0,"Unprotected",ifelse(net$PRI <= 1.25, "Class 4",
                                                         ifelse(net$PRI <= 2.5 & net$PRI > 1.25, "Class 3",
                                                                ifelse(net$PRI <= 3.75  & net$PRI > 2.5, "Class 2",
                                                                       ifelse(net$PRI > 3.75, "Class 1",NA)))))
pick <- c(grep("RIV_|RIP_|CRI_|INC1_|INC2_|MULT1_|MULT2_",names(net)))
mat <- net[,pick]
mat_len <- mat * net$TotalLen

#rename
colnames(mat)[colnames(mat) =="RIP_State.riparian.buffers"] <- "RIP_State.Riparian.Buffers"
colnames(mat_len)[colnames(mat_len) =="RIP_State.riparian.buffers"] <- "RIP_State.Riparian.Buffers"

#select only protected segments
mat <- mat[-which(apply(mat,1,sum)==0),]
dim(mat)

#frequencies
DAAT_abs1 <- apply(mat,2,sum)/nrow(mat)
write.table(DAAT_abs1,"outputs/FrequenciesMechanisms.csv")

#prevalence (for nodes)
DAAT_abs <- apply(mat_len,2,sum)/sum(net$TotalLen)
write.table(DAAT_abs,"outputs/PrevalenceMechanisms.csv")

#using co-occurence
mat <- cbind(0,0,0,0,mat)#dummy variables at the beginning
cor_v <- ecospat.co_occurrences(data=mat)#remove the first 4 columns

write.table(cor_v,"outputs/CooccurrenceMechanisms.csv")


##########################Figure
library(igraph); library(corrplot); library(ecospat); library(pheatmap) ; library(ggraph); library(cowplot)

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}


cor_v <- read.table("outputs/CooccurrenceMechanisms.csv")
DAAT_abs1 <- read.table("outputs/FrequenciesMechanisms.csv")
DAAT_abs <- read.table("outputs/PrevalenceMechanisms.csv")

#quick analysis of mean co-occurrence
sort(apply(cor_v,1,mean))

DAAT_abs <- setNames(DAAT_abs$x,rownames(DAAT_abs))
DAAT_abs1 <- setNames(DAAT_abs1$x,rownames(DAAT_abs1))

#remove mechanisms that are not well represented (less than 1%)
DAAT_abs <- DAAT_abs[-which(DAAT_abs < 0.01)]
DAAT_abs1 <- DAAT_abs1[match(names(DAAT_abs),names(DAAT_abs1)),drop=F]
cor_v <- cor_v[match(names(DAAT_abs),rownames(cor_v)),match(names(DAAT_abs),colnames(cor_v))]

#visualization
g = graph_from_adjacency_matrix(as.matrix(cor_v),weighted=T, mode="undirected", diag=F)

#set node colors
V(g)$membership <- sapply(strsplit(V(g)$name,"_",T),'[',1)

V(g)$colorB = "black"
V(g) [ membership == "RIV" ]$color <- "#628dbd"  
V(g) [ membership == "RIP" ]$color <- "chartreuse4"
V(g) [ membership == "CRI" ]$color <- "#c06c84"
V(g) [ membership == "INC1" ]$color <- "#fa8072"
V(g) [ membership == "INC2" ]$color <- "#fcae91"
V(g) [ membership == "MULT1" ]$color <- grey(0.3) 
V(g) [ membership == "MULT2" ]$color <- "grey" 

#V(g)$color <- ccl_w$cols[match(V(g)$name,ccl_w$Label)]

#set nodes names
name = sapply(strsplit(V(g)$name,"_",T),'[',2)
name = gsub("."," ",name,fixed=T)
name[name=="ONRW OTRW"] <- "ONRW/OTRW"
name = gsub("NWFP","Northwest Forest Plan",name)

# Set node size
V(g)$size <- log(DAAT_abs)

jpeg("outputs/NetworkMechanisms_FigureS3.jpeg", units="in", width=8.5, height=8, res=300, pointsize = 19.5)

g %>%
  ggraph(layout = "circle") +
  geom_edge_arc(colour="steelblue",
                lineend = "round",
                strength = .1,
                aes(edge_width = E(g)$weight,edge_alpha = E(g)$weight)) +
  geom_node_point(size=sqrt(DAAT_abs)*5, 
                  colour=alpha(V(g)$colorB,1),
                  fill=alpha(V(g)$color,1), shape = 21) +
  geom_node_text(label = name, 
                 repel = TRUE, 
                 point.padding = unit(1, "lines"), 
                 size=sqrt(sqrt(DAAT_abs1)*5)+1, 
                 colour=darken(alpha(V(g)$color,1)),max.overlaps=Inf) +
  scale_edge_width(range = c(0, 2)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

dev.off()


#------------------------------
#Number of mechanisms
library(sf); library(reshape2)

net <- st_read("data/NPRALayer_segment.gpkg")
net <- st_drop_geometry(net)
pick <- c(grep("RIV_|RIP_|CRI_|INC1_|INC2_|MULT1_|MULT2_",names(net)))
mat <- net[,pick]

#transform matrix to binary protected versus not
mat <- ifelse(mat>0,1,0)

#number of mechanisms
net$NumberMechanism <- apply(mat,1,sum)

#Mean number of mechansisms (across all segments)
mean(net$NumberMechanism,na.rm=T)

#Mean number of mechanisms (across protected segments)
mean(net$NumberMechanism[net$NumberMechanism>0])

#Percentage of protected length with more than one mechanism
sum(net$FinalProt_LenT[net$NumberMechanism>1])*100/sum(net$FinalProt_LenT)
tapply(net$FinalProt_LenT[net$NumberMechanism>0],net$NumberMechanism[net$NumberMechanism>0],sum)*100/sum(net$FinalProt_LenT[net$NumberMechanism>0])

#Mean number of mechanisms per PRI Categories
tapply(net$NumberMechanism[net$NumberMechanism>0],net$PRI_Class[net$NumberMechanism>0],mean)
net$NumberMechanism <- ifelse(net$NumberMechanism >= 5,"≥5",net$NumberMechanism)
net$NumberMechanism <- factor(net$NumberMechanism,levels=c("1","2","3","4","≥5"))
net$PRI_Class <- ifelse(net$PRI ==0,"Unprotected",ifelse(net$PRI <= 1.25, "Class 4",
                                                         ifelse(net$PRI <= 2.5 & net$PRI > 1.25, "Class 3",
                                                                ifelse(net$PRI <= 3.75  & net$PRI > 2.5, "Class 2",
                                                                       ifelse(net$PRI > 3.75, "Class 1",NA)))))

vari = c("Class 4","Class 3","Class 2","Class 1")
F_stat_state <- aggregate(FinalProt_LenT ~ NumberMechanism,sum,data=net)
F_stat_state$protect <- F_stat_state$FinalProt_LenT*100/sum(net$TotalLen)

write.csv(F_stat_state,"outputs/NumberMecha_s.csv",row.names=F)

#Figure
F_stat_state <- read.csv("outputs/NumberMecha_s.csv")

cols_bar <- rev(c("#CCCCCC","#C500FF","#00C5FF","#0070FF"))

jpeg("outputs/histNbMecha.jpeg", units="in", width=9, height=6, res=300, pointsize = 15)

par(mar=c(4,6,2,1));par(xpd=NA)
b <- barplot(F_stat_state$FinalProt_LenT,axes=F,col=rep(cols_bar,each=5))
axis(2,pos=0,las=2,at=seq(0,800000,200000),scales::comma(seq(0,800000,200000)),cex.axis=1)
mtext(side=2,"River Length (Miles)",cex=1.3,line=5)
text(b,rep(-20000,4),F_stat_state$NumberMechanism)
mtext(side=1,"Number of mechanisms",cex=1.3,line=2)
text(b,F_stat_state$FinalProt_LenT+10000,paste0(format(round(F_stat_state$protect,1),nsmall=1),"%"),col=rep(cols_bar,each=5),cex=0.7)
legend(bty="n","topleft",col=(cols_bar),pch=15,c("Comprehensive","Effective","Limited","Inadequate"),pt.cex=2.2,cex=1,inset=c(0.1,0))
dev.off()
