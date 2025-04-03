#--------------------------------------------------------
#Representativity
#---------------------------------------------------------
library(sf); library(reshape2); library(stringr); library(dplyr); library(ggplot2)

net_protect_seg_fin <- read.csv("data/Table_protection_segments_RIPAllCombined.csv")

#-------------------------------------------------------
#Import the data

#data available at: https://doi.org/10.6084/m9.figshare.c.4233740
#Add classification stream type
cl <- list.files("data/US SCS/Size_Gradient/")
dat <- read.csv(paste0("data/US SCS/Size_Gradient/",cl[1]))
for(i in 2:length(cl)){
  ii <- read.csv(paste0("data/US SCS/Size_Gradient/","/",cl[i]))
  names(ii) <- names(dat)
  dat <- rbind(dat,ii)
}

#Add temperature regime
cl <- list.files("data/US SCS/Temperature/")
dat2 <- read.csv(paste0("data/US SCS/Temperature/",cl[1]))
for(i in 2:length(cl)){
  ii <- read.csv(paste0("data/US SCS/Temperature/","/",cl[i]))
  names(ii) <- names(dat2)
  dat2 <- rbind(dat2,ii)
}

dat <- merge(dat,dat2,by="COMID")

#Add Hydrology regime
cl <- list.files("data/US SCS/Hydrology/")
cl <- cl[!cl %in% c("probabilities.zip")]
dat4 <- read.csv(paste0("data/US SCS/Hydrology/",cl[1]))
for(i in 2:length(cl)){
  ii <- read.csv(paste0("data/US SCS/Hydrology/","/",cl[i]))
  names(ii) <- names(dat4)
  dat4 <- rbind(dat4,ii)
}

dat <- data.frame(dat,dat4[match(dat$COMID,dat4$COMID),])

#keep only CONUS
net_s <- net_protect_seg_fin[!(net_protect_seg_fin$State %in% c("Alaska","Hawaii")),]
net_s <- net_s[!(is.na(net_s$State)),]
dim(net_s)

#keep only digitized flow
table(net_s$FLOWDIR[!net_s$COMID %in% dat$COMID],useNA="ifany")
net_s <- net_s[(net_s$FLOWDIR %in% c("With Digitized")),] 

#add elevation
EI <- read.csv("data/Contextualization_NPRA.csv")
net_s <- data.frame(net_s,Elev = EI$MINELEVRAW[match(net_s$COMID_LC,EI$COMID_L)])

#merge data
net_s <- data.frame(net_s,dat[match(net_s$COMID,dat$COMID),])
net_s <- net_s[,-which(names(net_s)%in% "COMID.1")]

#remove missing data
net_s[which(is.na(net_s$Size_Class)),]
net_s <- net_s[!is.na(net_s$Size_Class),]
dim(net_s) 
head(net_s)


#-------------------------------------------------------
#Figure elevation

#transform to m
range(net_s$Elev,na.rm=T) #in cm
net_s$Elev[net_s$Elev < -10] <- NA
net_s$Elev <- net_s$Elev/100

#assign classes
net_s$Elev_c <- ifelse(net_s$Elev < 200,"Lowlands",
                       ifelse(net_s$Elev >= 200 & net_s$Elev < 500,"Hills",
                              ifelse(net_s$Elev >= 500 & net_s$Elev < 1000,"Mid-altitude",
                                     ifelse(net_s$Elev >= 1000 & net_s$Elev < 2000,"High mountains","Very high mountains"))))

net_s$Elev_c <- factor(net_s$Elev_c,levels=c("Lowlands","Hills","Mid-altitude","High mountains","Very high mountains"))
table(net_s$Elev_c)

table(net_s$Elev_c,useNA="ifany")*100/sum(table(net_s$Elev_c,useNA="ifany"))

#prepare dataset
vari = rev(c("Class 4", "Class 3","Class 2","Class 1"))
cols_bar <- (c("#CCCCCC","#C500FF","#00C5FF","#0070FF"))
F_stat_state <- aggregate(OverallProtection_Len_m ~ Elev_c + RIP_Class,sum,data=net_s[net_s$RIP_Class %in% c("Class 1","Class 2","Class 3","Class 4"),])
F_stat_stateT <- aggregate(Total_Length_m ~ Elev_c,sum,data=net_s)
F_stat_state$protect <- F_stat_state$OverallProtection_Len_m*100/F_stat_stateT$Total_Length_m[match(F_stat_state$Elev_c,F_stat_stateT$Elev_c)]
F_stat_state$RIP_Class <- factor(F_stat_state$RIP_Class)

state_plot <- acast(F_stat_state, Elev_c~factor(RIP_Class,levels=vari), value.var="protect")
state_plot <- ifelse(is.na(state_plot)==T,0,state_plot)
state_plot <- state_plot[,!colnames(state_plot) %in% "Unprotected"]

forPie <- F_stat_stateT$Total_Length_m*100/sum(F_stat_stateT$Total_Length_m)
names(forPie) <- F_stat_stateT$Elev_c
forPie <- forPie[rownames(state_plot)]

#rename classes
colnames(state_plot) <- gsub("Class 1","Comprehensive",colnames(state_plot))
colnames(state_plot) <- gsub("Class 2","Effective",colnames(state_plot))
colnames(state_plot) <- gsub("Class 3","Limited",colnames(state_plot))
colnames(state_plot) <- gsub("Class 4","Inadequate",colnames(state_plot))

#assessing unbalances
p_obs <- apply(state_plot[,c("Comprehensive","Effective","Limited","Inadequate")],1,sum)
chisq.test(p_obs, p=forPie/100) #p-value < 2.2e-16

p_obs <- apply(state_plot[,c("Comprehensive","Effective","Limited")],1,sum)
chisq.test(p_obs, p=forPie/100) #p-value < 2.2e-16

state_plot <- state_plot[match(c("Lowlands","Hills","Mid-altitude","High mountains","Very high mountains"),rownames(state_plot)),]

#figure
jpeg("outputs/FigureElev.jpeg", units="in", width=8, height=8, res=300, pointsize = 16)

par(mar=c(4, 0,0,1))
b=barplot(t(state_plot),las=1,ylab="",cex.names=0.9,col=rev(cols_bar),xlim=c(-60,80),legend=F,cex.lab=1.4,las=2,axes=F,border=rev(cols_bar),names=rep(NA,length(rownames(state_plot))),horiz=T)#
barplot(forPie*-1,col="white",border="black",horiz=T,add=T,las=2,names=rep(NA,length(rownames(state_plot))),axes=F)#

axis(1,pos=-0.1,las=1,at=c(seq(0,80,20)),labels=abs(c(seq(0,80,20))))
text(forPie*-1-2,b,rownames(state_plot),las=2,tck=0,srt=0,pos=2,xpd=TRUE,cex=1.1)
legend(bty="n","bottomright",col=rev(cols_bar),pch=15,(gsub("."," ",colnames(state_plot),fixed=T)),pt.cex=2.2,cex=1.1)
mtext(side=1,"River length (%)",cex=1.4,line=2.8,adj=0.8)
dev.off()

#-----------------------------------------------------------------------
#Figure major basins

#get info HUC02
net_protect_seg_fin$HUC_12 <- as.character(net_protect_seg_fin$HUC_12)
net_protect_seg_fin$HUC_12 <- ifelse(sapply(strsplit(net_protect_seg_fin$HUC_12,"*"),length)==11,paste0("0",net_protect_seg_fin$HUC_12),net_protect_seg_fin$HUC_12)
net_protect_seg_fin$Basin <- sapply(strsplit(as.character(net_protect_seg_fin$HUC_12),"*"),function(x) paste(x[1:2],collapse="",sep=""))
net_protect_seg_fin$Basin <- ifelse(net_protect_seg_fin$Basin == "01","New England",
                                    ifelse(net_protect_seg_fin$Basin == "02","Mid-Atlantic",
                                           ifelse(net_protect_seg_fin$Basin == "03","S. Atlantic Gulf",
                                                  ifelse(net_protect_seg_fin$Basin == "04","Great Lakes",
                                                         ifelse(net_protect_seg_fin$Basin == "05","Ohio",
                                                                ifelse(net_protect_seg_fin$Basin == "06","Tennessee",
                                                                       ifelse(net_protect_seg_fin$Basin == "07","Upper Mississippi",
                                                                              ifelse(net_protect_seg_fin$Basin == "08","Lower Mississippi",
                                                                                     ifelse(net_protect_seg_fin$Basin == "09","Souris-Red-Rainy",
                                                                                            ifelse(net_protect_seg_fin$Basin == "10","Missouri",
                                                                                                   ifelse(net_protect_seg_fin$Basin == "11","Arkansas-White-Red",
                                                                                                          ifelse(net_protect_seg_fin$Basin == "12","Texas Gulf",
                                                                                                                 ifelse(net_protect_seg_fin$Basin == "13","Rio Grande",
                                                                                                                        ifelse(net_protect_seg_fin$Basin == "14","Upper Colorado",
                                                                                                                               ifelse(net_protect_seg_fin$Basin == "15","Lower Colorado",
                                                                                                                                      ifelse(net_protect_seg_fin$Basin == "16","Great Basin",
                                                                                                                                             ifelse(net_protect_seg_fin$Basin == "17","Pacific NW",
                                                                                                                                                    ifelse(net_protect_seg_fin$Basin == "18","California",
                                                                                                                                                           ifelse(net_protect_seg_fin$Basin == "19","Alaska",
                                                                                                                                                                  ifelse(net_protect_seg_fin$Basin == "20","Hawaii",NA))))))))))))))))))))
table(net_protect_seg_fin$Basin ,useNA="ifany")*100/sum(table(net_protect_seg_fin$Basin,useNA="ifany"))

#Prepare file
vari = c("Class 4","Class 3","Class 2","Class 1")
F_stat_state <- aggregate(OverallProtection_Len_m ~ Basin + RIP_Class,sum,data=net_protect_seg_fin)
F_stat_stateT <- aggregate(Total_Length_m ~ Basin,sum,data=net_protect_seg_fin)
F_stat_state$protect <- F_stat_state$OverallProtection_Len_m*100/F_stat_stateT$Total_Length_m[match(F_stat_state$Basin,F_stat_stateT$Basin)]
F_stat_state$RIP_Class <- factor(F_stat_state$RIP_Class)

state_plot <- acast(F_stat_state, Basin~RIP_Class, value.var="protect")
state_plot <- ifelse(is.na(state_plot)==T,0,state_plot)
state_plot <- state_plot[,!colnames(state_plot) %in% "Unprotected"]
state_plot <- data.frame(state_plot,Unprotected = as.vector(100 - apply(state_plot,1,sum)))
state_plot <- state_plot[,-ncol(state_plot)]

cols_bar <- (c("#CCCCCC","#C500FF","#00C5FF","#0070FF"))

colnames(state_plot) <- gsub("Class 1","Comprehensive",colnames(state_plot))
colnames(state_plot) <- gsub("Class 2","Effective",colnames(state_plot))
colnames(state_plot) <- gsub("Class 3","Limited",colnames(state_plot))
colnames(state_plot) <- gsub("Class 4","Inadequate",colnames(state_plot))

forPie <- F_stat_stateT$Total_Length_m
names(forPie) <- F_stat_stateT$Basin

#Figures
jpeg("outputs/PieBasins.jpeg", units="in", width=5, height=5, res=300, pointsize = 9)
pie(forPie,col=gray.colors(nlevels(factor(net_protect_seg_fin$Basin))),init.angle=90,clockwise = T,border="white")
dev.off()


jpeg("outputs/BarplotBasins.jpeg", units="in", width=8, height=8, res=300, pointsize = 16)

par(mar=c(4,12.5,1,1))
state_plot <- state_plot[rev(1:nrow(state_plot)),]
b=barplot(t(state_plot),las=1,ylab="",cex.names=0.9,col=rev(cols_bar),xlim=c(0,80),legend=F,cex.lab=1.4,las=2,axes=F,border=rev(cols_bar),names=rep(NA,length(rownames(state_plot))),horiz=T)#
axis(1,pos=-0.1,las=1)
text(rep(-2,length(b)),b,rownames(state_plot),las=2,tck=0,srt=0,pos=2,xpd=TRUE,cex=1.2)
mtext(side=1,"River length (%)",cex=1.4,line=2)

dev.off()


#-----------------------------------------------------------------------
#Figure ecoregions

#available at https://www.feow.org/
feow <- st_read("data/HUC12_FEOW.dbf")

#add to table major habitat types
net_protect_seg_fin$MHT_TXT <- feow$MHT_TXT[match(net_protect_seg_fin$HUC_12,feow$HUC_12)]
table(net_protect_seg_fin$MHT_TXT,useNA="ifany")

#add to table & rename ecoregions
net_protect_seg_fin$ECOREGION <- feow$ECOREGION[match(net_protect_seg_fin$HUC_12,feow$HUC_12)]
net_protect_seg_fin$ECOREGION <- gsub("Northeast","NE",net_protect_seg_fin$ECOREGION)
net_protect_seg_fin$ECOREGION <- gsub("Southeast","SE",net_protect_seg_fin$ECOREGION)
net_protect_seg_fin$ECOREGION <- gsub("Northwest","NW",net_protect_seg_fin$ECOREGION)
net_protect_seg_fin$ECOREGION <- gsub("Northern","N",net_protect_seg_fin$ECOREGION)
net_protect_seg_fin$ECOREGION <- gsub("Southern","S",net_protect_seg_fin$ECOREGION)

#add numbers
feow$ECO_ID[is.na(feow$ECOREGION)] <- NA
net_protect_seg_fin$ECO_ID <- feow$ECO_ID[match(net_protect_seg_fin$HUC_12,feow$HUC_12)]
net_protect_seg_fin$ECOREGION <- factor(paste0(net_protect_seg_fin$ECOREGION," (",net_protect_seg_fin$ECO_ID,")"))

table(net_protect_seg_fin$ECOREGION ,useNA="ifany")*100/sum(table(net_protect_seg_fin$ECOREGION,useNA="ifany"))#

#Prepare table
vari = c("Class 4","Class 3","Class 2","Class 1")
F_stat_state <- aggregate(OverallProtection_Len_m ~ ECOREGION + RIP_Class,sum,data=net_protect_seg_fin)
F_stat_stateT <- aggregate(Total_Length_m ~ ECOREGION,sum,data=net_protect_seg_fin)
F_stat_state$protect <- F_stat_state$OverallProtection_Len_m*100/F_stat_stateT$Total_Length_m[match(F_stat_state$ECOREGION,F_stat_stateT$ECOREGION)]
F_stat_state$RIP_Class <- factor(F_stat_state$RIP_Class)

state_plot <- acast(F_stat_state, ECOREGION~RIP_Class, value.var="protect")
state_plot <- ifelse(is.na(state_plot)==T,0,state_plot)
state_plot <- state_plot[,!colnames(state_plot) %in% "Unprotected"]
state_plot <- data.frame(state_plot,Unprotected = as.vector(100 - apply(state_plot,1,sum)))
state_plot <- state_plot[,-ncol(state_plot)]
state_plot <- state_plot[!rownames(state_plot)=="NA (NA)",]

cols_bar <- (c("#CCCCCC","#C500FF","#00C5FF","#0070FF"))

forPie <- F_stat_stateT$Total_Length_m[order(apply(state_plot[,1:3],1,sum))]
names(forPie) <- F_stat_stateT$ECOREGION[order(apply(state_plot[,1:3],1,sum))]
names(forPie) <- sapply(strsplit(as.character(names(forPie)),"(",fixed=T),'[',2)
names(forPie) <- gsub(")","",names(forPie))

#rename protection classes
colnames(state_plot) <- gsub("Class.1","Comprehensive",colnames(state_plot))
colnames(state_plot) <- gsub("Class.2","Effective",colnames(state_plot))
colnames(state_plot) <- gsub("Class.3","Limited",colnames(state_plot))
colnames(state_plot) <- gsub("Class.4","Inadequate",colnames(state_plot))

#figure
jpeg("outputs/BarplotECOREGION_v2.jpeg", units="in", width=10, height=8, res=300, pointsize = 16)

forPie <- F_stat_stateT$Total_Length_m*100/sum(F_stat_stateT$Total_Length_m)
names(forPie) <- F_stat_stateT$ECOREGION
forPie <- forPie[rownames(state_plot)]

par(mar=c(4,16.5,0,1))
state_plot <- state_plot[order(apply(state_plot[,1:3],1,sum)),]
b=barplot(t(state_plot),las=1,ylab="",cex.names=0.9,col=rev(cols_bar),xlim=c(0,100),legend=F,cex.lab=1.4,las=2,axes=F,border=rev(cols_bar),names=rep(NA,length(rownames(state_plot))),horiz=T)#
barplot(forPie*-1,col="white",border="black",horiz=T,add=T,las=2,names=rep(NA,length(rownames(state_plot))),axes=F)#

axis(1,pos=-0.1,las=1)
text(rep(-15,length(b)),b,rownames(state_plot),las=2,tck=0,srt=0,pos=2,xpd=TRUE,cex=0.7)
legend(bty="n","bottomright",col=rev(cols_bar),pch=15,gsub("."," ",colnames(state_plot),fixed=T),pt.cex=2.2,cex=1.1,inset=c(0,0.1))
mtext(side=1,"River length (%)",cex=1.4,line=2)

dev.off()


#------------------------------------------------------------------------------------------
#combined figure - Representative
#-----------------------------------------------------------------------------------------

#-------------------------------------------------------
#protection
#------------------------------------------------------
net_s$RIP_Class <- factor(net_s$RIP_Class,levels=c("Unprotected","Class 4","Class 3","Class 2","Class 1"))

#major habitat types
F_stat_state1 <- aggregate(OverallProtection_Len_m ~ MHT_TXT + RIP_Class,sum,data=net_protect_seg_fin,drop=F)
F_stat_stateT <- aggregate(Total_Length_m ~ MHT_TXT,sum,data=net_protect_seg_fin,drop=F)
F_stat_state1$OverallProtection_Len_m <- F_stat_state1$OverallProtection_Len_m*100/F_stat_stateT$Total_Length_m[match(F_stat_state1$MHT_TXT,F_stat_stateT$MHT_TXT)]
F_stat_state1$RIP_Class <- factor(F_stat_state1$RIP_Class)
names(F_stat_state1) <- c("individual","observation","value")
F_stat_state1$group = "Ecoregion"


#Size class
net_s$Size_Class <- ifelse(net_s$Size_Class=="HW","Headwater",
                                   ifelse(net_s$Size_Class=="SC","Small creek",
                                          ifelse(net_s$Size_Class=="LC","Large creek",
                                                 ifelse(net_s$Size_Class=="SR","Small river",
                                                        ifelse(net_s$Size_Class=="MR","Medium river",
                                                               ifelse(net_s$Size_Class=="MS","Mainstem",
                                                                      ifelse(net_s$Size_Class=="LR","Large river",
                                                                             ifelse(net_s$Size_Class=="GR","Great river",NA))))))))

net_s$Size_Class <- factor(net_s$Size_Class,levels=c("Headwater","Small creek","Large creek","Small river","Medium river","Mainstem","Large river","Great river"))
F_stat_state2 <- aggregate(OverallProtection_Len_m ~ Size_Class + RIP_Class,sum,data=net_s,drop=F)
F_stat_stateT <- aggregate(Total_Length_m ~ Size_Class,sum,data=net_s,drop=F)
F_stat_state2$OverallProtection_Len_m <- F_stat_state2$OverallProtection_Len_m*100/F_stat_stateT$Total_Length_m[match(F_stat_state2$Size_Class,F_stat_stateT$Size_Class)]
F_stat_state2$RIP_Class <- factor(F_stat_state2$RIP_Class)
names(F_stat_state2) <- c("individual","observation","value")
F_stat_state2$group = "Size class"
F_stat_state2 <- F_stat_state2[!is.na(F_stat_state2$individual)==T,]

#Temperature regime
net_s$JulAug_Class[net_s$JulAug_Class=="Very Cold"] <- "Cold"
net_s$JulAug_Class <- factor(net_s$JulAug_Class,levels=c("Cold","Cool","Cool-Warm","Warm"))
F_stat_state3 <- aggregate(OverallProtection_Len_m ~ JulAug_Class + RIP_Class,sum,data=net_s,drop=F)
F_stat_stateT <- aggregate(Total_Length_m ~ JulAug_Class,sum,data=net_s,drop=F)
F_stat_state3$OverallProtection_Len_m <- F_stat_state3$OverallProtection_Len_m*100/F_stat_stateT$Total_Length_m[match(F_stat_state3$JulAug_Class,F_stat_stateT$JulAug_Class)]
F_stat_state3$RIP_Class <- factor(F_stat_state3$RIP_Class)
names(F_stat_state3) <- c("individual","observation","value")
F_stat_state3$group = "Temperature class"

#Hydrology type
net_s$g8[net_s$g8 %in% c(1,8) ] <- "Perennial runoff"
net_s$g8[net_s$g8 %in% c(4) ] <- "Perennial flashy"
net_s$g8[net_s$g8 %in% c(5,7) ] <- "Intermittent"
net_s$g8[net_s$g8 %in% c(2,6) ] <- "Snowmelt"
net_s$g8[net_s$g8 %in% c(3) ] <- "Stable baseflow"
net_s$g8[net_s$g8 %in% c(0) ] <- NA

F_stat_state4 <- aggregate(OverallProtection_Len_m ~ g8 + RIP_Class,sum,data=net_s,drop=F)
F_stat_stateT <- aggregate(Total_Length_m ~ g8,sum,data=net_s,drop=F)
F_stat_state4$OverallProtection_Len_m <- F_stat_state4$OverallProtection_Len_m*100/F_stat_stateT$Total_Length_m[match(F_stat_state4$g8,F_stat_stateT$g8)]
F_stat_state4$RIP_Class <- factor(F_stat_state4$RIP_Class)
names(F_stat_state4) <- c("individual","observation","value")
F_stat_state4$group = "Hydrology class"

data <- rbind(F_stat_state1,F_stat_state2,F_stat_state3,F_stat_state4)
data$group <- factor(data$group,levels=c("Ecoregion","Size class","Hydrology class","Temperature class"))

#Total length per category
F_stat_stateT <- aggregate(Total_Length_m ~ MHT_TXT,sum,data=net_protect_seg_fin)
a <- F_stat_stateT$Total_Length_m*100/sum(F_stat_stateT$Total_Length_m)
names(a) <- F_stat_stateT$MHT_TXT

F_stat_stateT <- aggregate(Total_Length_m ~ Size_Class,sum,data=net_s)
b <- F_stat_stateT$Total_Length_m*100/sum(F_stat_stateT$Total_Length_m)
names(b) <- F_stat_stateT$Size_Class
b <- b[!is.na(names(b))]

F_stat_stateT <- aggregate(Total_Length_m ~ JulAug_Class,sum,data=net_s)
c <- F_stat_stateT$Total_Length_m*100/sum(F_stat_stateT$Total_Length_m)
names(c) <- F_stat_stateT$JulAug_Class

F_stat_stateT <- aggregate(Total_Length_m ~ g8,sum,data=net_s)
d <- F_stat_stateT$Total_Length_m*100/sum(F_stat_stateT$Total_Length_m)
names(d) <- F_stat_stateT$g8

data_c <- data.frame(individual = c(names(a),names(b),names(c),names(d)),
                     group=c(rep("Ecoregion",length(a)),rep("Size class",length(b)),rep("Temperature class",length(c)),rep("Hydrology class",length(d))),
                     value = c(a,b,c,d))

data_c$group <- factor(data_c$group,levels=c("Ecoregion","Size class","Hydrology class","Temperature class"))
data_c$value <- -data_c$value
data_c$observation <- "Representation"


#Prepare table
data$observation <- factor(data$observation)
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
#keep levels organized
le <- c(names(a),names(b),names(c),names(d))
data <- data %>% arrange(group, factor(individual,levels=le))
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
data <- data[-which(data$observation == "Unprotected"),]

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value[!observation == "Representation"]))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data$angle[angle==-90] <- 90 #small adjustement to avoid inverted label
label_data$hjust[angle==-90] <- 1 #small adjustement to avoid inverted label

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

#change labels
data$observation <- as.character(data$observation)
data$observation[which(data$observation=="Class 1")] <- "Comprehensive"
data$observation[which(data$observation=="Class 2")] <- "Effective"
data$observation[which(data$observation=="Class 3")] <- "Limited"
data$observation[which(data$observation=="Class 4")] <- "Inadequate"
data$observation <- factor(data$observation,levels=c("Comprehensive","Effective","Limited","Inadequate","Representation"))

label_data$individual[label_data$individual == "large lakes"] <- "large lake basins"
label_data$individual[label_data$individual == "temperate floodplain rivers and wetlands"] <- "temperate floodplain rivers"
label_data$individual[label_data$individual == "tropical and subtropical coastal rivers"] <- "tropical and subtropical coastal"
label_data$individual[label_data$individual == "xeric freshwaters and endorheic (closed) basins"] <- "xeric freshwaters and
endorheic (closed) basins"

#format labels
label_data$individual <- Hmisc::capitalize(label_data$individual)

# Make the plot  
colo <- c("Comprehensive" = '#0070FF', "Effective" = '#00C5FF', "Limited" = '#C500FF',"Inadequate" = '#CCCCCC',"Representation" = "white")

p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=1,color="black",lwd=0.2) +
  scale_fill_manual("", values = colo,labels=c("Comprehensive", "Effective", "Limited","Inadequate","Total river length"),na.translate = F) +

  # Add axes
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  
  geom_segment(data=grid_data, aes(x = end, y = -20, xend = start, yend = 20), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = -40, xend = start, yend = 40), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = -60, xend = start, yend = 60), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = -80, xend = start, yend = 80), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value axes
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 20, 40, 60, 80), label = c("0%", "20%", "40%", "60%", "80%") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-80,135) +
  theme_minimal() +
  theme(
    legend.position = c(.2,0.2),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+5, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3.5, angle= label_data$angle, inherit.aes = FALSE) +
  
  # Add base line information
  geom_text(data=base_data, aes(x = title, y = 115, label=c("(a)","(b)","(c)","(d)")), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

ggsave("outputs/Figurerepresentativity.jpeg", plot=p,width=8, height=8)

#------------------------------------------------------------------
#Assessing unbalances

#hydrology type
p_obs <- aggregate(value~individual,sum,data=data[data$group=="Hydrology class" & data$observation %in% c("Class 1","Class 2","Class 3","Class 4"),])
p_tot <- aggregate(value~individual,sum,data=data[data$group=="Hydrology class" & data$observation %in% c("Representation"),])

chisq.test(p_obs$value, p=p_tot$value/100) #p-value < 2.2e-16

#ecoregion (is in fact basin)
p_obs <- aggregate(value~individual,sum,data=data[data$group=="Ecoregion" & data$observation %in% c("Class 1","Class 2","Class 3","Class 4"),])
p_tot <- aggregate(value~individual,sum,data=data[data$group=="Ecoregion" & data$observation %in% c("Representation"),])

chisq.test(p_obs$value, p=p_tot$value/100) #p-value < 2.2e-16

#size class
p_obs <- aggregate(value~individual,sum,data=data[data$group=="Size class" & data$observation %in% c("Class 1","Class 2","Class 3","Class 4"),])
p_tot <- aggregate(value~individual,sum,data=data[data$group=="Size class" & data$observation %in% c("Representation"),])

chisq.test(p_obs$value, p=p_tot$value/100) #p-value < 2.2e-16

#temperature class
p_obs <- aggregate(value~individual,sum,data=data[data$group=="Temperature class" & data$observation %in% c("Class 1","Class 2","Class 3","Class 4"),])
p_tot <- aggregate(value~individual,sum,data=data[data$group=="Temperature class" & data$observation %in% c("Representation"),])

chisq.test(p_obs$value, p=p_tot$value/100) #p-value = 0.02623

