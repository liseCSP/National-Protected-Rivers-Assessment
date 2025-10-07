#---------------------------------------------
#Summary mechanisms & Protected Rivers Index
#---------------------------------------------
#Lise Comte, April 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

#Data can be found at: 10.5281/zenodo.17279334

#Upload the data
net_protect_seg_fin <- read.csv("data/NPRALayer_segment_download.csv",h=T) #tabular version of geopackage

 #--------------------------------------------------------------------
#All the protection fields in miles in the tabular version
#Each protection mechanism of class indicates the length of a given segment being protected by this given mechanism
#Class.1 = Comprehensive (in miles)
#Class.2 = Effective (in miles)
#Class.3 = Limited (in miles)
#Class.4 = Inadequate (in miles)
#TotalLen = length of the segment (in miles)
#FinalProt_LenT = protected length of the segment (in miles)

#Prepare the data for download as a geopackage
#convert to percentages, except protected length (in miles) and PRI (2 digits for length and PRI and one digit percentages)
# st_write(net, "data/NPRALayer_segment_download.gpkg",append=FALSE) 
#---------------------------------------------------------------------

#-------------------------------------------------
#Summary protection by PRI classes

L_P_cl1 <- sum(net_protect_seg_fin$Class.1,na.rm=T)
L_P_cl2 <- sum(net_protect_seg_fin$Class.2,na.rm=T)
L_P_cl3 <- sum(net_protect_seg_fin$Class.3,na.rm=T)
L_P_cl4 <- sum(net_protect_seg_fin$Class.4,na.rm=T)
L_P_clun <- sum(net_protect_seg_fin$Unprotected,na.rm=T)

L_P_mi <- c(L_P_clun, L_P_cl4, L_P_cl3, L_P_cl2, L_P_cl1)
L_P_per <- L_P_mi*100/sum(net_protect_seg_fin$TotalLen,na.rm=T)
ProtectedKmUS <- L_P_mi * 1.60934

#for CONUS
L_P_cl1_conter <- sum(net_protect_seg_fin$Class.1[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],na.rm=T)
L_P_cl2_conter <- sum(net_protect_seg_fin$Class.2[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],na.rm=T)
L_P_cl3_conter <- sum(net_protect_seg_fin$Class.3[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],na.rm=T)
L_P_cl4_conter <- sum(net_protect_seg_fin$Class.4[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],na.rm=T)
L_P_clun_conter <- sum(net_protect_seg_fin$Unprotected[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],na.rm=T)

L_P_mi_conter <- c(L_P_clun_conter, L_P_cl4_conter, L_P_cl3_conter, L_P_cl2_conter,L_P_cl1_conter)
L_P_per_conter <- L_P_mi_conter*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")],na.rm=T)
ProtectedKmCONUS <- L_P_mi_conter * 1.60934

RIPRivers <- data.frame(ProtectedMilesCONUS=L_P_mi_conter,ProtectedMilesUS=L_P_mi,
                        ProtectedKmCONUS=ProtectedKmCONUS,ProtectedKmUS=ProtectedKmUS,
                        ProtectedPerCONUS=L_P_per_conter,ProtectedPerUS=L_P_per)

rownames(RIPRivers) <- c("Unprotected","Inadequate","Limited","Effective","Comprehensive")
write.csv(RIPRivers,"outputs/Results_RiversRIP.csv")

#-------------------------------------------------
#Barplot PRI results
library(reshape2); library(dplyr); library(RColorBrewer); library(scales); library(sf); library(MESS)
library(ggplot2); library(waffle)

RIPRivers <- read.csv("outputs/Results_RiversRIP.csv")

jpeg("outputs/barplotPRI.jpeg", units="in", width=6, height=9, res=300, pointsize = 15)
par(mar=c(6,7,1,1))
b <- barplot((RIPRivers$ProtectedKmUS),col="white",ylab="",las=2,cex.lab=1.3,ylim=c(0,5000000),border=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),axes=F)
barplot((RIPRivers$ProtectedKmCONUS),col=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),border=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),add=T,axes=F,names="")
axis(2,pos=0,las=2,at=seq(0,5000000,1000000),scales::comma(seq(0,5000000,1000000)))
mtext(side=2,"River Length (km)",cex=1.4,line=5.4)
par(xpd=NA)
text((b+0.25),rep(-100000,length(b)),c("No protection","Inadequate","Limited","Effective","Comprehensive"),srt=45,pos=2)
per <- round(RIPRivers$ProtectedPerUS,1)
text(b,(RIPRivers$ProtectedKmUS)+60000/0.6214,paste0(format(per,nsmall=1),"%"),col=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),cex=1)
per_conter <- round(RIPRivers$ProtectedPerCONUS,1)
text(b,((RIPRivers$ProtectedKmCONUS)-45000/0.6214)[1:4],paste0(format(per_conter,nsmall=1),""),col="white",cex=1)
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
net_protect_seg_fin <- read.csv("data/NPRALayer_segment_download.csv",h=T) #tabular version of geopackage

#-----Remove overlap within categories 

#River conservation: ONRW = OTRW > NWS > SWS > Eligible
#Riparian conservation: RFPA > KW > WPA > NWFP buffer > state RipBuff
#Terrestrial protected area (strict) IUCN I > IUCN II
#Terrestrial protected area (others) IUCN III > IUCN IV > IUCN V > IUCN VI
#Multiple use (special management)
#Multiple use (others)
#------------------------------------------------------------------

#Get a summary per category (in km and %)
#-------------------------------
RiverSystem <- sum(net_protect_seg_fin$RiverSystem)*1.60934
RiparianSystem <- sum(net_protect_seg_fin$RiparianSystem)*1.60934
IncidentalStrict <- sum(net_protect_seg_fin$IncidentalStrict)*1.60934
Incidental <- sum(net_protect_seg_fin$Incidental)*1.60934
MultipleUseSpecial <- sum(net_protect_seg_fin$MultipleUseSpecial)*1.60934
MultipleUse <- sum(net_protect_seg_fin$MultipleUse)*1.60934
CriticalHabitat <- sum(net_protect_seg_fin$CriticalHabitat)*1.60934
TotalLength <- sum(net_protect_seg_fin$TotalLen)*1.60934

RiverSystemP <- sum(net_protect_seg_fin$RiverSystem)*100/sum(net_protect_seg_fin$TotalLen)
RiparianSystemP <- sum(net_protect_seg_fin$RiparianSystem)*100/sum(net_protect_seg_fin$TotalLen)
IncidentalStrictP <- sum(net_protect_seg_fin$IncidentalStrict)*100/sum(net_protect_seg_fin$TotalLen)
IncidentalP <- sum(net_protect_seg_fin$Incidental)*100/sum(net_protect_seg_fin$TotalLen)
MultipleUseSpecialP <- sum(net_protect_seg_fin$MultipleUseSpecial)*100/sum(net_protect_seg_fin$TotalLen)
MultipleUseP <- sum(net_protect_seg_fin$MultipleUse)*100/sum(net_protect_seg_fin$TotalLen)
CriticalHabitatP <- sum(net_protect_seg_fin$CriticalHabitat)*100/sum(net_protect_seg_fin$TotalLen)
TotalLengthP <- sum(net_protect_seg_fin$TotalLen)*100/sum(net_protect_seg_fin$TotalLen)

Tot <- c(RiverSystem,RiparianSystem,IncidentalStrict,Incidental,MultipleUseSpecial,MultipleUse,CriticalHabitat,TotalLength)
TotP <- c(RiverSystemP,RiparianSystemP,IncidentalStrictP,IncidentalP,MultipleUseSpecialP,MultipleUseP,CriticalHabitatP,TotalLengthP)
Tot <- rbind(round(Tot),round(TotP,1))
colnames(Tot) <- c("RiverSystem","RiparianSystem","IncidentalStrict","Incidental","MultipleUseSpecial","MultipleUse","CriticalHabitat","TotalLength")
write.csv(Tot,"outputs/Table1_SummaryProtectionPerCategory.csv")

RiverSystem <- sum(net_protect_seg_fin$RiverSystem[! net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*1.60934
RiparianSystem <- sum(net_protect_seg_fin$RiparianSystem[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*1.60934
IncidentalStrict <- sum(net_protect_seg_fin$IncidentalStrict[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*1.60934
Incidental <- sum(net_protect_seg_fin$Incidental[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*1.60934
MultipleUseSpecial <- sum(net_protect_seg_fin$MultipleUseSpecial[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*1.60934
MultipleUse <- sum(net_protect_seg_fin$MultipleUse[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*1.60934
CriticalHabitat <- sum(net_protect_seg_fin$CriticalHabitat[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*1.60934
TotalLength <- sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*1.60934

RiverSystemP <- sum(net_protect_seg_fin$RiverSystem[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])
RiparianSystemP <- sum(net_protect_seg_fin$RiparianSystem[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])
IncidentalStrictP <- sum(net_protect_seg_fin$IncidentalStrict[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])
IncidentalP <- sum(net_protect_seg_fin$Incidental[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])
MultipleUseSpecialP <- sum(net_protect_seg_fin$MultipleUseSpecial[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])
MultipleUseP <- sum(net_protect_seg_fin$MultipleUse[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])
CriticalHabitatP <- sum(net_protect_seg_fin$CriticalHabitat[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])
TotalLengthP <- sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])*100/sum(net_protect_seg_fin$TotalLen[!net_protect_seg_fin$State %in% c("Alaska","Hawaii")])

TotCONUS <- c(RiverSystem,RiparianSystem,IncidentalStrict,Incidental,MultipleUseSpecial,MultipleUse,CriticalHabitat,TotalLength)
TotPCONUS <- c(RiverSystemP,RiparianSystemP,IncidentalStrictP,IncidentalP,MultipleUseSpecialP,MultipleUseP,CriticalHabitatP,TotalLengthP)
TotCONUS <- rbind(round(TotCONUS),round(TotPCONUS,1))
colnames(TotCONUS) <- c("RiverSystem","RiparianSystem","IncidentalStrict","Incidental","MultipleUseSpecial","MultipleUse","CriticalHabitat","TotalLength")
write.csv(TotCONUS,"outputs/Table1_SummaryProtectionPerCategoryCONUS.csv")

#----------------Summary individual mechanisms of protection
pick <- c(grep("RIV_|RIP_|CRI_|INC1_|INC2_|MULT1_|MULT2_",names(net_protect_seg_fin)))
mat <- net_protect_seg_fin[,pick]
summ_all <- apply(mat,2,sum)*100/sum(net_protect_seg_fin$TotalLen)
summ_all <- round(summ_all,2)
write.csv(summ_all,"outputs/SummaryIndividualProtectionMechanism.csv")



#-------------------------------------------
#Network of mechanisms
library(sf); library(reshape2); library(ecospat)
net_protect_seg_fin <- st_read("data/NPRALayer_segment_download.gpkg")

net_protect_seg_fin <- st_drop_geometry(net_protect_seg_fin)  #using geopackage

pick <- c(grep("RIV_|RIP_|CRI_|INC1_|INC2_|MULT1_|MULT2_",names(net_protect_seg_fin)))
mat <- net_protect_seg_fin[,pick] 
mat_len <- mat * net_protect_seg_fin$TotalLen 

#rename
colnames(mat)[colnames(mat) =="RIP_State.riparian.buffers"] <- "RIP_State.Riparian.Buffers"
colnames(mat_len)[colnames(mat_len) =="RIP_State.riparian.buffers"] <- "RIP_State.Riparian.Buffers"

#Select only protected segments
mat <- mat[-which(apply(mat,1,sum)==0),]
dim(mat)

#Estimate frequencies
DAAT_abs1 <- apply(mat,2,sum)/nrow(mat)
write.csv(DAAT_abs1,"outputs/FrequenciesMechanisms.csv",row.names=T)

#Estimate prevalence (for nodes)
DAAT_abs <- apply(mat_len,2,sum)/sum(net_protect_seg_fin$TotalLen)
write.csv(DAAT_abs,"outputs/PrevalenceMechanisms.csv",row.names=T)

#Estimate co-occurence index
mat <- cbind(0,0,0,0,mat)#dummy variables at the beginning
cor_v <- ecospat.co_occurrences(data=mat)#remove the first 4 columns

write.csv(cor_v,"outputs/CooccurrenceMechanisms.csv",row.names=T)


#-----------------------Figure
library(igraph); library(corrplot); library(ecospat); library(pheatmap) ; library(ggraph); library(cowplot)

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}


cor_v <- read.csv("outputs/CooccurrenceMechanisms.csv",row.names = 1)
DAAT_abs1 <- read.csv("outputs/FrequenciesMechanisms.csv",row.names = 1)
DAAT_abs <- read.csv("outputs/PrevalenceMechanisms.csv",row.names = 1)

#quick analysis of mean co-occurrence
sort(apply(cor_v,1,mean))

DAAT_abs <- setNames(DAAT_abs$x,rownames(DAAT_abs))
DAAT_abs1 <- setNames(DAAT_abs1$x,rownames(DAAT_abs1))

#remove mechanisms that are not well represented (less than 1%)
DAAT_abs <- DAAT_abs[which(!DAAT_abs < 0.01)]
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

net_protect_seg_fin <- st_read("data/NPRALayer_segment_download.gpkg")
net_protect_seg_fin <- st_drop_geometry(net_protect_seg_fin) #if using geopackage
pick <- c(grep("RIV_|RIP_|CRI_|INC1_|INC2_|MULT1_|MULT2_",names(net_protect_seg_fin)))
mat <- net_protect_seg_fin[,pick]

#Transform matrix to binary protected versus not
mat <- ifelse(mat>0,1,0)

#Estimate number of mechanisms
net_protect_seg_fin$NumberMechanism <- apply(mat,1,sum)

#Mean number of mechansisms (across all segments)
mean(net_protect_seg_fin$NumberMechanism,na.rm=T)

#Mean number of mechanisms (across protected segments)
mean(net_protect_seg_fin$NumberMechanism[net_protect_seg_fin$NumberMechanism>0])

#Percentage of protected length with more than one mechanism
sum(net_protect_seg_fin$FinalProt_LenT[net_protect_seg_fin$NumberMechanism>1])*100/sum(net_protect_seg_fin$FinalProt_LenT)
tapply(net_protect_seg_fin$FinalProt_LenT[net_protect_seg_fin$NumberMechanism>0],net_protect_seg_fin$NumberMechanism[net_protect_seg_fin$NumberMechanism>0],sum)*100/sum(net_protect_seg_fin$FinalProt_LenT[net_protect_seg_fin$NumberMechanism>0])

#Mean number of mechanisms per PRI Categories
net_protect_seg_fin$PRI_Class <- ifelse(net_protect_seg_fin$PRI ==0,"Unprotected",ifelse(net_protect_seg_fin$PRI <= 1.25, "Class 4",
                                                                                         ifelse(net_protect_seg_fin$PRI <= 2.5 & net_protect_seg_fin$PRI > 1.25, "Class 3",
                                                                                                ifelse(net_protect_seg_fin$PRI <= 3.75  & net_protect_seg_fin$PRI > 2.5, "Class 2",
                                                                                                       ifelse(net_protect_seg_fin$PRI > 3.75, "Class 1",NA)))))

tapply(net_protect_seg_fin$NumberMechanism[net_protect_seg_fin$NumberMechanism>0],net_protect_seg_fin$PRI_Class[net_protect_seg_fin$NumberMechanism>0],mean)
net_protect_seg_fin$NumberMechanism <- ifelse(net_protect_seg_fin$NumberMechanism >= 5,"≥5",net_protect_seg_fin$NumberMechanism)
net_protect_seg_fin$NumberMechanism <- factor(net_protect_seg_fin$NumberMechanism,levels=c("1","2","3","4","≥5"))

vari = c("Class 4","Class 3","Class 2","Class 1")
F_stat_state <- aggregate(FinalProt_LenT ~ NumberMechanism+PRI_Class,sum,data=net_protect_seg_fin)
F_stat_state$protect <- F_stat_state$FinalProt_LenT*100/sum(net_protect_seg_fin$TotalLen)
F_stat_state$FinalProt_LenT <- F_stat_state$FinalProt_LenT*1.60934
write.csv(F_stat_state,"outputs/NumberMecha_s.csv",row.names=F)

#------------------------Figure
F_stat_state <- read.csv("outputs/NumberMecha_s.csv")

cols_bar <- rev(c("#CCCCCC","#C500FF","#00C5FF","#0070FF"))

jpeg("outputs/histNbMecha.jpeg", units="in", width=9, height=6, res=300, pointsize = 15)

par(mar=c(4,6,2,1));par(xpd=NA)
b <- barplot(F_stat_state$FinalProt_LenT,axes=F,col=rep(cols_bar,each=5))
axis(2,pos=0,las=2,at=seq(0,800000,200000),scales::comma(seq(0,800000,200000)),cex.axis=1)
mtext(side=2,"River Length (km)",cex=1.3,line=5)
text(b,rep(-20000,4),F_stat_state$NumberMechanism)
mtext(side=1,"Number of mechanisms",cex=1.3,line=2)
text(b,F_stat_state$FinalProt_LenT+20000,paste0(format(round(F_stat_state$protect,1),nsmall=1),"%"),col=rep(cols_bar,each=5),cex=0.7)
legend(bty="n","topleft",col=(cols_bar),pch=15,c("Comprehensive","Effective","Limited","Inadequate"),pt.cex=2.2,cex=1,inset=c(0.1,0))
dev.off()
