#---------------------------------------------
#Summary mechanisms & Protected Rivers Index
#---------------------------------------------
#Lise Comte, April 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

#Data can be found at: https://figshare.com/s/62d2f6da57b9526a9aee

#-------------------------------------------------
#Summary protection by PRI classes
net_protect_seg_fin <- read.csv("data/Table_protection_segments_RIPAllCombined.csv")

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
L_P_mi_conter <- L_P_conter * 0.621371
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

#------------------------------------------------- 
#Summary protection by mechanisms
library(sf); library(data.table); library(MESS); library(dplyr)


#segment-level data
net_protect_seg_fin <- read.csv("data/Table_protection_segments_RIPAllCombined.csv")

#pad HUC12 with zeros
net_protect_seg_fin$HUC_12 <- as.character(net_protect_seg_fin$HUC_12)
net_protect_seg_fin$HUC_12 <- ifelse(sapply(strsplit(net_protect_seg_fin$HUC_12,"*"),length)==11,paste0("0",net_protect_seg_fin$HUC_12),net_protect_seg_fin$HUC_12)

#identify columns
L <- grep("Len_m",names(net_protect_seg_fin))
huc <- which(names(net_protect_seg_fin)=='HUC_12')


#-----Remove overlap within categories 

#River conservation: ONRW = OTRW > NWS > SWS > Eligible
#------------------------------------------------------------------
ONRW_Len_m <- net_protect_seg_fin$ONRW_Len_m
ONRW_Len_m[net_protect_seg_fin$ONRW_Len_per < 5] <- 0

OTRW_Len_m <- net_protect_seg_fin$OTRW_Len_m - net_protect_seg_fin$ONRW_Len_m
OTRW_Len_m[OTRW_Len_m < 0] <- 0
OTRW_Len_m[net_protect_seg_fin$OTRW_Len_per < 5] <- 0

NWSF_Len_m <- net_protect_seg_fin$NWS_Len_m - ONRW_Len_m - OTRW_Len_m 
NWSF_Len_m[NWSF_Len_m < 0] <- 0
NWSF_Len_m[net_protect_seg_fin$NWSF_Len_per < 5] <- 0

SWSF_Len_m <- net_protect_seg_fin$SWS_Len_m - ONRW_Len_m - OTRW_Len_m - NWSF_Len_m 
SWSF_Len_m[SWSF_Len_m < 0] <- 0
SWSF_Len_m[net_protect_seg_fin$SWSF_Len_per < 5] <- 0

NWSF_Eli_Len_m <- net_protect_seg_fin$NWS_Eli_Len_m - ONRW_Len_m - OTRW_Len_m - NWSF_Len_m - SWSF_Len_m
NWSF_Eli_Len_m[NWSF_Eli_Len_m < 0] <- 0
NWSF_Eli_Len_m[net_protect_seg_fin$NWS_Eli_Len_per < 5] <- 0

FPA_Len_m <- net_protect_seg_fin$FPA_Len_m - ONRW_Len_m - OTRW_Len_m - NWSF_Len_m - SWSF_Len_m - NWSF_Eli_Len_m 
FPA_Len_m[FPA_Len_m < 0] <- 0
FPA_Len_m[net_protect_seg_fin$FPA_Len_per < 5] <- 0

RiverSystem_Len <-  ONRW_Len_m + OTRW_Len_m + NWSF_Len_m + SWSF_Len_m + NWSF_Eli_Len_m + FPA_Len_m
RiverSystem_Len <-  RiverSystem_Len


#Riparian conservation: RFPA > KW > WPA > FAM > RipBuff
#------------------------------------------------------------------
RFPA_Len_m <- net_protect_seg_fin$RFPA_Len_m
RFPA_Len_m[net_protect_seg_fin$RFPA_Len_m < 5] <- 0

KW_Len_m <- net_protect_seg_fin$KW_Len_m - RFPA_Len_m 
KW_Len_m[KW_Len_m<0] <- 0
KW_Len_m[net_protect_seg_fin$KW_Len_per < 5] <- 0

WPA_Len_m <- net_protect_seg_fin$WPA_Len_m - RFPA_Len_m - KW_Len_m 
WPA_Len_m[WPA_Len_m<0] <- 0
WPA_Len_m[net_protect_seg_fin$WPA_Len_per < 5] <- 0

FAM_Len_m <- net_protect_seg_fin$FAM_Len_m - RFPA_Len_m - KW_Len_m - WPA_Len_m 
FAM_Len_m[FAM_Len_m<0] <- 0
FAM_Len_m[net_protect_seg_fin$FAM_Len_per < 5] <- 0

BufferRiparian_Len_m <- net_protect_seg_fin$BufferRiparian_Len_m - RFPA_Len_m - KW_Len_m - WPA_Len_m - FAM_Len_m
BufferRiparian_Len_m[BufferRiparian_Len_m<0] <- 0
BufferRiparian_Len_m[net_protect_seg_fin$BufferRiparian_Len_per < 5] <- 0

RiparianSystem_Len <- RFPA_Len_m + KW_Len_m + WPA_Len_m + FAM_Len_m + BufferRiparian_Len_m
RiparianSystem_Len <-  RiparianSystem_Len

#Split riparian protected area per designation type
dat_rip <- data.frame(COMID_LC = c(net_protect_seg_fin$COMID_LC,net_protect_seg_fin$COMID_LC,net_protect_seg_fin$COMID_LC,net_protect_seg_fin$COMID_LC,net_protect_seg_fin$COMID_LC),
                      HUC_12 = c(net_protect_seg_fin$HUC_12,net_protect_seg_fin$HUC_12,net_protect_seg_fin$HUC_12,net_protect_seg_fin$HUC_12,net_protect_seg_fin$HUC_12),
                      Des_Tp = c(net_protect_seg_fin$RFPA_Des_Tp,net_protect_seg_fin$WPA_Des_Tp,net_protect_seg_fin$FAM_Des_Tp,ifelse(net_protect_seg_fin$BufferRiparian_Des_Tp=="NWFP_RiparianReserves","NWFP Riparian Reserves","State riparian buffers"),rep("NWFP Key Watersheds",nrow(net_protect_seg_fin))),
                      Len = c(net_protect_seg_fin$RFPA_Len_m,net_protect_seg_fin$WPA_Len_m,net_protect_seg_fin$FAM_Len_m,net_protect_seg_fin$BufferRiparian_Len_m,net_protect_seg_fin$KW_Len_m))
dat_rip <- na.omit(dat_rip)
RIP <- reshape2::dcast(dat_rip, COMID_LC ~ Des_Tp, value.var = "Len",max)#there should be no overlaping designation but in case use max instead of sum (in case a designation type has been assigned to more than one category)
RIP <- RIP[match(net_protect_seg_fin$COMID_LC,RIP$COMID_LC),]
RIP$COMID_LC <- net_protect_seg_fin$COMID_LC #for NA
RIP <- replace(RIP, RIP == "-Inf", 0)
RIP <- replace(RIP, is.na(RIP)==T, 0)
rm(list='dat_rip')

#merge all mechanisms with less than 100 miles in 'Other'
agg <- names(RIP)[which(apply(RIP,2,sum) < 100000/ 0.621371)]
RIP$Other <- apply(RIP[,names(RIP) %in% c(agg,'Other')],1,sum)
RIP <- RIP[,!names(RIP) %in% agg]
names(RIP)[-1] <- paste0("RIP_",names(RIP)[-1])


#Terrestrial protected area (strict)
#------------------------------------------------------------------
dat_ics <- data.frame(COMID_LC = c(net_protect_seg_fin$COMID_LC,net_protect_seg_fin$COMID_LC),HUC_12 = c(net_protect_seg_fin$HUC_12,net_protect_seg_fin$HUC_12),Des_Tp = c(net_protect_seg_fin$IUCNI_Des_Tp,net_protect_seg_fin$IUCNII_Des_Tp),Len = c(net_protect_seg_fin$IUCNI_Len_m,net_protect_seg_fin$IUCNII_Len_m))
dat_ics <- na.omit(dat_ics)
IS <- reshape2::dcast(dat_ics, COMID_LC ~ Des_Tp, value.var = "Len",max) #Use max instead of sum (in case a designation type has been assigned to more than one category)
IS <- IS[match(net_protect_seg_fin$COMID_LC,IS$COMID_LC),]
IS$COMID_LC <- net_protect_seg_fin$COMID_LC
IS <- replace(IS, IS == "-Inf", 0)
IS <- replace(IS, is.na(IS)==T, 0)
ISS <- IS[,!names(IS) %in% "Conservation Easement"]
rm(list='dat_ics')
names(ISS)[-1] <- paste0("INC1_",names(ISS)[-1])

IUCNI_Len_m <- net_protect_seg_fin$IUCNI_Len_m
IUCNI_Len_m[net_protect_seg_fin$IUCNI_Len_per < 5] <- 0

IUCNII_Len_m <- net_protect_seg_fin$IUCNII_Len_m - IUCNI_Len_m 
IUCNII_Len_m[IUCNII_Len_m<0] <- 0
IUCNII_Len_m[net_protect_seg_fin$IUCNII_Len_per < 5] <- 0

IUCNI_Len_m[which(net_protect_seg_fin$IUCNI_Des_Tp == "Conservation Easement")] <- 0 #remove conservation easements (will go to 'other' category)
IUCNII_Len_m[which(net_protect_seg_fin$IUCNII_Len_m == "Conservation Easement")] <- 0

IncidentalStrict_Len <- IUCNI_Len_m + IUCNII_Len_m 
IncidentalStrict_Len <-  IncidentalStrict_Len

#Terrestrial protected area (others)
#------------------------------------------------------------------
ISCC_Len_m <- IS[,names(IS) %in% c("Conservation Easement")] #add conservation easements

IUCNIII_Len_m <- net_protect_seg_fin$IUCNIII_Len_m - ISCC_Len_m 
IUCNIII_Len_m[IUCNIII_Len_m<0] <- 0
IUCNIII_Len_m[net_protect_seg_fin$IUCNIII_Len_per < 5] <- 0

IUCNIV_Len_m <- net_protect_seg_fin$IUCNIV_Len_m - ISCC_Len_m - IUCNIII_Len_m
IUCNIV_Len_m[IUCNIV_Len_m<0] <- 0
IUCNIV_Len_m[net_protect_seg_fin$IUCNIV_Len_per < 5] <- 0

IUCNV_Len_m <- net_protect_seg_fin$IUCNV_Len_m - ISCC_Len_m - IUCNIII_Len_m - IUCNIV_Len_m 
IUCNV_Len_m[IUCNV_Len_m<0] <- 0
IUCNV_Len_m[net_protect_seg_fin$IUCNV_Len_per < 5] <- 0

IUCNVI_Len_m <- net_protect_seg_fin$IUCNVI_Len_m - ISCC_Len_m - IUCNIII_Len_m - IUCNIV_Len_m - IUCNV_Len_m 
IUCNVI_Len_m[IUCNVI_Len_m<0] <- 0
IUCNVI_Len_m[net_protect_seg_fin$IUCNVI_Len_per < 5] <- 0

Incidental_Len <- ISCC_Len_m + IUCNIII_Len_m + IUCNIV_Len_m + IUCNV_Len_m  + IUCNVI_Len_m
Incidental_Len <-  Incidental_Len

dat_ics <- data.frame(COMID_LC = rep(net_protect_seg_fin$COMID_LC,5),
                      HUC_12 = rep(net_protect_seg_fin$HUC_12,5),
                      Des_Tp = c(net_protect_seg_fin$IUCNIII_Des_Tp,net_protect_seg_fin$IUCNIV_Des_Tp,net_protect_seg_fin$IUCNV_Des_Tp,net_protect_seg_fin$IUCNVI_Des_Tp,rep("Conservation Easement",length(ISCC_Len_m))),
                      Len = c(net_protect_seg_fin$IUCNIII_Len_m,net_protect_seg_fin$IUCNIV_Len_m,net_protect_seg_fin$IUCNV_Len_m,net_protect_seg_fin$IUCNVI_Len_m,ISCC_Len_m))

dat_ics <- na.omit(dat_ics)
IS <- reshape2::dcast(dat_ics, COMID_LC ~ Des_Tp, value.var = "Len",max) #use max to avoid duplicates among layers (same designation type for different mechanisms)
IS <- IS[match(net_protect_seg_fin$COMID_LC,IS$COMID_LC),]
IS$COMID_LC <- net_protect_seg_fin$COMID_LC
IS <- replace(IS, IS == "-Inf", 0)
IS <- replace(IS, is.na(IS)==T, 0)
rm(list='dat_ics')

#merge all mechanisms with less than 100 miles in 'Other'
agg <- names(IS)[which(apply(IS,2,sum) < 100000/0.621371)]
IS$Other <- apply(IS[,names(IS) %in% c(agg,'Other')],1,sum)
IS <- IS[,!names(IS) %in% agg]
names(IS)[-1] <- paste0("INC2_",names(IS)[-1])

dat_mult <- data.frame(COMID_LC = rep(net_protect_seg_fin$COMID_LC,2),
                       HUC_12 = rep(net_protect_seg_fin$HUC_12,2),
                       Des_Tp = c(net_protect_seg_fin$Gap3_Des_Tp,net_protect_seg_fin$IUCNOTH_Des_Tp),
                       Len = c(net_protect_seg_fin$Gap3_Len_m,net_protect_seg_fin$IUCNOTH_Len_m))
dat_mult <- na.omit(dat_mult)
MultipleUse <- reshape2::dcast(dat_mult, COMID_LC ~ Des_Tp, value.var = "Len",max) #use max to avoid duplicates among layers (same designation type for different mechanisms)
MultipleUse <- MultipleUse[match(net_protect_seg_fin$COMID_LC,MultipleUse$COMID_LC),]
MultipleUse$COMID_LC <- net_protect_seg_fin$COMID_LC
MultipleUse <- replace(MultipleUse, MultipleUse == "-Inf", 0)
MultipleUse <- replace(MultipleUse, is.na(MultipleUse)==T, 0)
rm(list='dat_mult')


#Multiple use (special management)
#------------------------------------------------------------------
MultipleUseSpecial_Len <- MultipleUse$'Area Of Critical Environmental Concern' + MultipleUse$'Inventoried Roadless Area' #[no overlap among them]
MultipleUseSpecial_Len <-  MultipleUseSpecial_Len
names(MultipleUse)[!names(MultipleUse) %in% c("COMID_LC","Area Of Critical Environmental Concern","Inventoried Roadless Area")] <- paste(names(MultipleUse)[!names(MultipleUse) %in% c("COMID_LC","Area Of Critical Environmental Concern","Inventoried Roadless Area")],"[Gap 3]")
MultipleUse_special <- MultipleUse[,names(MultipleUse) %in% c("COMID_LC","Area Of Critical Environmental Concern","Inventoried Roadless Area")]
MultipleUse <- MultipleUse[,!names(MultipleUse) %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")]

#merge all mechanisms with less than 100 miles in 'Other'
agg <- names(MultipleUse)[which(apply(MultipleUse,2,sum) < 100000/0.621371)]
MultipleUse$'Other [Gap 3]' <- apply(MultipleUse[,names(MultipleUse) %in% c(agg,'Other [Gap 3]')],1,sum)
MultipleUse <- MultipleUse[,!names(MultipleUse) %in% agg]

#Multiple use (others)
#------------------------------------------------------------------
m1 <- net_protect_seg_fin$Gap3_Len_m
m1[net_protect_seg_fin$Gap3_Des_Tp %in% c("Area Of Critical Environmental Concern","Inventoried Roadless Area")] <- 0
m2 <- net_protect_seg_fin$IUCNOTH_Len_m
m1 <- m1 - m2 #removing overlap
m1[m1 < 0] <- 0

MultipleUse_Len <- m1 + m2
MultipleUse_Len <-  MultipleUse_Len

names(MultipleUse) <- gsub("[Gap 3]","",names(MultipleUse),fixed=T)
names(MultipleUse) <- gsub(" $","",names(MultipleUse))
names(MultipleUse)[-1] <- paste0("MULT2_","",names(MultipleUse)[-1])

names(MultipleUse_special)[-1] <- paste0("MULT1_","",names(MultipleUse_special)[-1])


#Critical habitat
#------------------------------------------------------------------
CriticalHabitat_Len <- net_protect_seg_fin$CritHabESA_Len_m
CriticalHabitat_Len <-  CriticalHabitat_Len


#Format table
#------------------------------------------------------------------

#Total protection (after removing all overlap)
FinalProt_LenT <- net_protect_seg_fin$OverallProtection_Len_m

#Total length
TotalLen <- net_protect_seg_fin$Total_Length_m

#Prepare PRI classes
Class.1 <- ifelse(net_protect_seg_fin$RIP_Class == "Class 1",FinalProt_LenT,0) 
Class.2 <- ifelse(net_protect_seg_fin$RIP_Class == "Class 2",FinalProt_LenT,0) 
Class.3 <- ifelse(net_protect_seg_fin$RIP_Class == "Class 3",FinalProt_LenT,0) 
Class.4 <- ifelse(net_protect_seg_fin$RIP_Class == "Class 4",FinalProt_LenT,0) 
Unprotected <- TotalLen - Class.1 - Class.2 - Class.3 - Class.4 #accounts for both entirely unprotected segments and partially unprotected ones

#Add viable
Viable <- Class.1 + Class.2 + Class.3

#Add non-viable
NonViable <- Class.4 + Unprotected

#Create data frame from with individual and combined mechanisms
dataOver <- data.frame(COMID_LC = net_protect_seg_fin$COMID_LC, HUC_12 = net_protect_seg_fin$HUC_12,State = net_protect_seg_fin$State,PRI = net_protect_seg_fin$RIP,PRI_Class = net_protect_seg_fin$RIP_Class,Class.1,Class.2,Class.3,Class.4,Unprotected,Viable,
                       NonViable, RiverSystem=RiverSystem_Len,RiparianSystem=RiparianSystem_Len,IncidentalStrict=IncidentalStrict_Len,Incidental=Incidental_Len,
                       MultipleUseSpecial=MultipleUseSpecial_Len,MultipleUse=MultipleUse_Len,CriticalHabitat=CriticalHabitat_Len,FinalProt_LenT,TotalLen,
                       RIV_ONRW.OTRW = (ONRW_Len_m + OTRW_Len_m),RIV_National.Wild.Scenic=net_protect_seg_fin$NWS_Len_m,
                       RIV_State.Wild.Scenic=net_protect_seg_fin$SWS_Len_m,
                       RIV_Eligible.Study.Wild.Scenic=net_protect_seg_fin$NWS_Eli_Len_m,RIV_ScenicRiverways.NationalRivers=net_protect_seg_fin$FPA_Len_m,
                       RIP[match(net_protect_seg_fin$COMID_LC,RIP$COMID_LC),-1],
                       CRI_CriticalHabitat=CriticalHabitat_Len,
                       ISS[match(net_protect_seg_fin$COMID_LC,ISS$COMID_LC),-1],IS[,-1],
                       MultipleUse_special[match(net_protect_seg_fin$COMID_LC,MultipleUse_special$COMID_LC),-1],
                       MultipleUse[match(net_protect_seg_fin$COMID_LC,MultipleUse$COMID_LC),-1])
#a bit of cleaning
names(dataOver) <- gsub("..",".",names(dataOver),fixed=T)


#Get a summary per category (in km and %)
#-------------------------------
RiverSystem <- sum(dataOver$RiverSystem)/1000
RiparianSystem <- sum(dataOver$RiparianSystem)/1000
IncidentalStrict <- sum(dataOver$IncidentalStrict)/1000
Incidental <- sum(dataOver$Incidental)/1000
MultipleUseSpecial <- sum(dataOver$MultipleUseSpecial)/1000
MultipleUse <- sum(dataOver$MultipleUse)/1000
CriticalHabitat <- sum(dataOver$CriticalHabitat)/1000
TotalLength <- sum(dataOver$TotalLen)/1000

RiverSystemP <- sum(dataOver$RiverSystem)*100/sum(dataOver$TotalLen)
RiparianSystemP <- sum(dataOver$RiparianSystem)*100/sum(dataOver$TotalLen)
IncidentalStrictP <- sum(dataOver$IncidentalStrict)*100/sum(dataOver$TotalLen)
IncidentalP <- sum(dataOver$Incidental)*100/sum(dataOver$TotalLen)
MultipleUseSpecialP <- sum(dataOver$MultipleUseSpecial)*100/sum(dataOver$TotalLen)
MultipleUseP <- sum(dataOver$MultipleUse)*100/sum(dataOver$TotalLen)
CriticalHabitatP <- sum(dataOver$CriticalHabitat)*100/sum(dataOver$TotalLen)
TotalLengthP <- sum(dataOver$TotalLen)*100/sum(dataOver$TotalLen)

Tot <- c(RiverSystem,RiparianSystem,IncidentalStrict,Incidental,MultipleUseSpecial,MultipleUse,CriticalHabitat,TotalLength)
TotP <- c(RiverSystemP,RiparianSystemP,IncidentalStrictP,IncidentalP,MultipleUseSpecialP,MultipleUseP,CriticalHabitatP,TotalLengthP)
Tot <- rbind(round(Tot),round(TotP,1))
colnames(Tot) <- c("RiverSystem","RiparianSystem","IncidentalStrict","Incidental","MultipleUseSpecial","MultipleUse","CriticalHabitat","TotalLength")
write.csv(Tot,"outputs/Table1_SummaryProtectionPerCategory.csv")

RiverSystem <- sum(dataOver$RiverSystem[! dataOver$State %in% c("Alaska","Hawaii")])/1000
RiparianSystem <- sum(dataOver$RiparianSystem[!dataOver$State %in% c("Alaska","Hawaii")])/1000
IncidentalStrict <- sum(dataOver$IncidentalStrict[!dataOver$State %in% c("Alaska","Hawaii")])/1000
Incidental <- sum(dataOver$Incidental[!dataOver$State %in% c("Alaska","Hawaii")])/1000
MultipleUseSpecial <- sum(dataOver$MultipleUseSpecial[!dataOver$State %in% c("Alaska","Hawaii")])/1000
MultipleUse <- sum(dataOver$MultipleUse[!dataOver$State %in% c("Alaska","Hawaii")])/1000
CriticalHabitat <- sum(dataOver$CriticalHabitat[!dataOver$State %in% c("Alaska","Hawaii")])/1000
TotalLength <- sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])/1000

RiverSystemP <- sum(dataOver$RiverSystem[!dataOver$State %in% c("Alaska","Hawaii")])*100/sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])
RiparianSystemP <- sum(dataOver$RiparianSystem[!dataOver$State %in% c("Alaska","Hawaii")])*100/sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])
IncidentalStrictP <- sum(dataOver$IncidentalStrict[!dataOver$State %in% c("Alaska","Hawaii")])*100/sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])
IncidentalP <- sum(dataOver$Incidental[!dataOver$State %in% c("Alaska","Hawaii")])*100/sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])
MultipleUseSpecialP <- sum(dataOver$MultipleUseSpecial[!dataOver$State %in% c("Alaska","Hawaii")])*100/sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])
MultipleUseP <- sum(dataOver$MultipleUse[!dataOver$State %in% c("Alaska","Hawaii")])*100/sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])
CriticalHabitatP <- sum(dataOver$CriticalHabitat[!dataOver$State %in% c("Alaska","Hawaii")])*100/sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])
TotalLengthP <- sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])*100/sum(dataOver$TotalLen[!dataOver$State %in% c("Alaska","Hawaii")])

TotCONUS <- c(RiverSystem,RiparianSystem,IncidentalStrict,Incidental,MultipleUseSpecial,MultipleUse,CriticalHabitat,TotalLength)
TotPCONUS <- c(RiverSystemP,RiparianSystemP,IncidentalStrictP,IncidentalP,MultipleUseSpecialP,MultipleUseP,CriticalHabitatP,TotalLengthP)
TotCONUS <- rbind(round(TotCONUS),round(TotPCONUS,1))
colnames(TotCONUS) <- c("RiverSystem","RiparianSystem","IncidentalStrict","Incidental","MultipleUseSpecial","MultipleUse","CriticalHabitat","TotalLength")
write.csv(TotCONUS,"outputs/Table1_SummaryProtectionPerCategoryCONUS.csv")

#----------------Summary individual mechanisms of protection
pick <- c(grep("RIV_|RIP_|CRI_|INC1_|INC2_|MULT1_|MULT2_",names(dataOver)))
mat <- dataOver[,pick]
summ_all <- apply(mat,2,sum)*100/sum(dataOver$TotalLen)
summ_all <- round(summ_all,1)
write.csv(summ_all,"outputs/SummaryIndividualProtectionMechanism.csv")

#Save segment layer as a geopackage [layer provided]
#convert to percentages, except protected length (converted in miles) and PRI (2 digits for length and PRI and one digit percentages)
#add metadata for visualization purposes: name, COMID v2.1, stream order and floodplain extent (for Alaska)
#transform percentage for protection classes to binary protected vs not
# st_write(net, "data/NPRALayer_segment_download.gpkg",append=FALSE) 


#-------------------------------------------
#Network of mechanisms
library(sf); library(reshape2)
net <- st_read("data/NPRALayer_segment_download.gpkg")
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

#Select only protected segments
mat <- mat[-which(apply(mat,1,sum)==0),]
dim(mat)

#Estimate frequencies
DAAT_abs1 <- apply(mat,2,sum)/nrow(mat)
write.table(DAAT_abs1,"outputs/FrequenciesMechanisms.csv")

#Estimate prevalence (for nodes)
DAAT_abs <- apply(mat_len,2,sum)/sum(net$TotalLen)
write.table(DAAT_abs,"outputs/PrevalenceMechanisms.csv")

#Estimate co-occurence index
mat <- cbind(0,0,0,0,mat)#dummy variables at the beginning
cor_v <- ecospat.co_occurrences(data=mat)#remove the first 4 columns

write.table(cor_v,"outputs/CooccurrenceMechanisms.csv")


#-----------------------Figure
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


#-----------------------------------------
#Estimate number of mechanisms per segment
library(sf); library(reshape2)

net <- st_read("data/NPRALayer_segment_download.gpkg")
net <- st_drop_geometry(net)
pick <- c(grep("RIV_|RIP_|CRI_|INC1_|INC2_|MULT1_|MULT2_",names(net)))
mat <- net[,pick]

#Transform matrix to binary protected versus not
mat <- ifelse(mat>0,1,0)

#Estimate number of mechanisms
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

#------------------------Figure
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
