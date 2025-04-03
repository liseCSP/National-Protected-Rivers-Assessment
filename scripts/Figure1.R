#---------------------------------------------
#Summary mechanisms & Protected Rivers Index
#---------------------------------------------
#Lise Comte, April 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

#----------------- 
#Summary mechanisms

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
write.csv(pp,"outputs/Table1.csv")

#-------------
#Barplot PRI results
library(reshape2); library(dplyr); library(RColorBrewer); library(scales); library(sf); library(MESS)
library(ggplot2); library(waffle)

RIPRivers <- read.csv("data/Results_RiversRIP.csv")

#change to km
RIPRivers$ProtectedMilesUS <- RIPRivers$ProtectedMilesUS/0.6214
RIPRivers$ProtectedMilesCONUS <- RIPRivers$ProtectedMilesCONUS/0.6214


jpeg("outputs/barplotPRI.jpeg", units="in", width=6, height=9, res=300, pointsize = 15)
par(mar=c(6,7,1,1))
b <- barplot(RIPRivers$ProtectedMilesUS,col="white",ylab="",las=2,cex.lab=1.3,ylim=c(0,5000000),border=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),axes=F)
barplot(RIPRivers$ProtectedMilesCONUS,col=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),border=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),add=T,axes=F,names="")
axis(2,pos=0,las=2,at=seq(0,5000000,1000000),scales::comma(seq(0,5000000,1000000)))
mtext(side=2,"River Length (km)",cex=1.4,line=5.4)
par(xpd=NA)
text((b+0.25),rep(-100000,length(b)),c("No protection","Inadequate","Limited","Effective","Comprehensive"),srt=45,pos=2)
per <- round(RIPRivers$ProtectedPerUS,1)
text(b,RIPRivers$ProtectedMilesUS+60000/0.6214,paste0(format(per,nsmall=1),"%"),col=c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"),cex=1)
per_conter <- round(RIPRivers$ProtectedPerCONUS,1)
text(b,(RIPRivers$ProtectedMilesCONUS-45000/0.6214)[1:4],paste0(format(per_conter,nsmall=1),""),col="white",cex=1)
rect(4,2500000/0.6214,4.6,2800000/0.6214,border=grey(0.4))
text(4.7,2700000/0.6214,"U.S.",adj=0,col=grey(0.4))
rect(4,2500000/0.6214,4.6,2600000/0.6214,col=grey(0.4),border=grey(0.5))
text(4.7,2550000/0.6214,"CONUS",adj=0,col=grey(0.4))
lines(c(b[3]-0.5,b[5]+0.5),c(750000/0.6214,750000/0.6214),lwd=2)
text(b[4],850000/0.6214,"Viable protection",font=3,cex=1.2)
dev.off()


#-------------------------------------------
#Network of mechanisms

library(igraph); library(corrplot); library(ecospat); library(pheatmap) ; library(ggraph); library(cowplot)

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}


cor_v <- read.table("data/CooccurrenceMechanisms.csv")
DAAT_abs1 <- read.table("data/FrequenciesMechanisms.csv")
DAAT_abs <- read.table("data/PrevalenceMechanisms.csv")

#quick overview of mean co-occurrence
sort(apply(cor_v,1,mean))

DAAT_abs <- setNames(DAAT_abs$x,rownames(DAAT_abs))
DAAT_abs1 <- setNames(DAAT_abs1$x,rownames(DAAT_abs1))

#remove mechanisms that are not well represented (less than 1%)
DAAT_abs <- DAAT_abs[-which(DAAT_abs < 0.01)]

#reorder so that always start with most important mechanism (for colors)
mm <- DAAT_abs[order(DAAT_abs,decreasing=T),drop=FALSE]

#reorder all the tables
DAAT_abs <- DAAT_abs[match(names(mm),names(DAAT_abs)),drop=F]
DAAT_abs1 <- DAAT_abs1[match(names(DAAT_abs),names(DAAT_abs1)),drop=F]
cor_v <- cor_v[match(names(DAAT_abs),rownames(cor_v)),match(names(DAAT_abs),colnames(cor_v))]

#set up color palette
ccl_w <- data.frame(Label = names(mm),group = sapply(strsplit(names(mm),"_"),'[',1))
ccl_w$cols <- NA
ccl_w$cols[ccl_w$group == "RIV"] <- (colorRampPalette(colors = c("#355c7d", "#deebf7"))(length(ccl_w$cols[ccl_w$group == "RIV"])))
ccl_w$cols[ccl_w$group == "RIP"] <- (colorRampPalette(colors = c("chartreuse4", "lightgreen"))(length(ccl_w$cols[ccl_w$group == "RIP"])))
ccl_w$cols[ccl_w$group== "CRI"] <- "#c06c84"
ccl_w$cols[ccl_w$group == "MULT1"] <-rev(colorRampPalette(colors = c(grey(0.5), grey(0.3)))(length(ccl_w$cols[ccl_w$group == "MULT1"])))
ccl_w$cols[ccl_w$group == "INC1"] <- (colorRampPalette(colors = c("#fa8072", "lightpink"))(length(ccl_w$cols[ccl_w$group == "INC1"])))
ccl_w$cols[ccl_w$group == "INC2"] <- (colorRampPalette(colors = c("#fcae91", "beige"))(length(ccl_w$cols[ccl_w$group=="INC2"])))
ccl_w$cols[ccl_w$group == "MULT2"] <- (colorRampPalette(colors = c("grey", "floralwhite"))(length(ccl_w$cols[ccl_w$group == "MULT2"])))

#visualization
g = graph_from_adjacency_matrix(as.matrix(cor_v),weighted=T, mode="undirected", diag=F)

#set node colors
V(g)$membership <- sapply(strsplit(V(g)$name,"_",T),'[',1)

V(g)$colorB = "black"
V(g) [ membership == "RIV" ]$colorB <- "#628dbd"  
V(g) [ membership == "RIP" ]$colorB <- "chartreuse4"
V(g) [ membership == "CRI" ]$colorB <- "#c06c84"
V(g) [ membership == "INC1" ]$colorB <- "#fa8072"
V(g) [ membership == "INC2" ]$colorB <- "#fcae91"
V(g) [ membership == "MULT1" ]$colorB <- grey(0.3) 
V(g) [ membership == "MULT2" ]$colorB <- "grey" 

V(g)$color <- ccl_w$cols[match(V(g)$name,ccl_w$Label)]

#set nodes names
name = sapply(strsplit(V(g)$name,"_",T),'[',2)
name = gsub("."," ",name,fixed=T)
name[name=="ONRW OTRW"] <- "ONRW/OTRW"
name = gsub("NWFP","Northwest Forest Plan",name)

# Set node size
V(g)$size <- log(DAAT_abs)

jpeg("outputs/NetworkMechanisms.jpeg", units="in", width=8.5, height=8, res=300, pointsize = 19.5)
g %>%
  ggraph(layout = "drl") +
  geom_edge_arc(colour="steelblue",
                lineend = "round",
                strength = .1,
                aes(edge_width = E(g)$weight,edge_alpha = E(g)$weight)) +
  geom_node_point(size=sqrt(DAAT_abs)*10, 
                  colour=alpha(V(g)$colorB,1),
                  fill=alpha(V(g)$color,1), shape = 21) +
  geom_node_text(label = name, 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(sqrt(DAAT_abs1)*5), 
                 colour=darken(alpha(V(g)$color,1)),max.overlaps=Inf) +
  #colour="transparent") +
  scale_edge_width(range = c(0, 2)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)
dev.off()


#-------------------------------------------------------
#Figure number of mechanisms

F_stat_state <- read.csv("data/NumberMecha_s.csv")

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

#------------------------------
#summary statistics mechanisms

library(sf); library(reshape2)

net <- st_read("data/NPRALayer_segment.gpkg")
net <- st_drop_geometry(net)
pick <- c(grep("RIV_|RIP_|CRI_|INC1_|INC2_|MULT1_|MULT2_",names(net)))
mat <- net[,pick]

net$PRI_Class <- ifelse(net$PRI <= 1.25, "Inadequate",
                        ifelse(net$PRI <= 2.5, "Limited",
                               ifelse(net$PRI <= 3.75, "Effective",
                                      ifelse(net$PRI >3.75, "Comprehensive",NA))))


mat <- ifelse(mat>0,1,0)

net$NumberMechanism <- apply(mat,1,sum)
mean(net$NumberMechanism[net$NumberMechanism>0])#1.562081
tapply(net$NumberMechanism[net$NumberMechanism>0],net$PRI_Class[net$NumberMechanism>0],mean)
#Comprehensive     Effective    Inadequate       Limited 
#2.799704      2.101047      1.298412      1.252820 

