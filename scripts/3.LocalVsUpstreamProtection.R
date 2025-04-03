#---------------------------------------------------------
#Analysis in/out protection

#Lise Comte, April 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

#Data can be found at: https://figshare.com/s/62d2f6da57b9526a9aee

library(circlize); library(reshape2); library(googleVis); library(networkD3); library(tidyverse); library(viridis)
library(patchwork); library(hrbrthemes); library(webshot); library(Polychrome); library(vcd)

PROTECTION <- read.csv("data/Protection_states_InOut_huc12.csv")
names(PROTECTION)[names(PROTECTION)=="IN"] <- "To"
names(PROTECTION)[names(PROTECTION)=="State"] <- "From"

PROTECTION$Dir <- paste(PROTECTION$From,PROTECTION$To,sep="_")
PROTECTION$INOUT <-  ifelse(PROTECTION$From == PROTECTION$To,"in","out")

#--------------------------------------------------------
#sankey diagram - viable protection across states
#-------------------------------------------------------
#percentage of state-specific protection
#river miles viable protection for each connection from --> to
P <- tapply(PROTECTION$OverallProtection_Len_m[PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3")],PROTECTION$Dir[PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3")],sum)

#percentage of viable protection in-state versus out of state
inState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "in")],PROTECTION$To[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="in")],sum)
outOfState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "out")],PROTECTION$Dir[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="out")],sum)

OeIn <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "in"],PROTECTION$To[PROTECTION$INOUT == "in"],sum)#total river miles in state
OeOut <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "out"],PROTECTION$To[PROTECTION$INOUT == "out"],sum)#total river miles coming from other state

inState <- inState*100/OeIn[match(names(inState),names(OeIn))]
outOfState <- outOfState*100/OeOut[match(sapply(strsplit(names(outOfState),"_"),'[',2),names(OeOut))]

df <- data.frame(from=c(names(inState),sapply(strsplit(names(outOfState),"_"),'[',1)),to=c(names(inState),sapply(strsplit(names(outOfState),"_"),'[',2)),length_Protected = c(inState,outOfState))

#reorder according to contribution
df <- df[order(df$length_Protected,decreasing=T),]
 
#reorder both in and out using the same order
ff <- ifelse(df$to==df$from,1,0)#identify in state connections

le <- df$from[ff==1] #set levels
df$from <- factor(df$from,levels=le)
nff1 <- order(df$from[ff==1])
nff2 <- order(df$from[ff==0])
df <- rbind(df[ff==1,][nff1,],df[ff==0,][nff2,])

#little trick to order the nodes
df$to <- paste(df$to, " ")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(df$from), as.character(df$to)) %>% unique())
nodes$node_group <- gsub(" ","_", nodes$name) #spaces are not allowed

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
df$IDsource <- match(df$from, nodes$name)-1 
df$IDtarget <- match(df$to, nodes$name)-1
df$group <- as.factor(gsub(" ","_",df$from))

# prepare colour scale
Pstate = createPalette(51,  c("#332288", "#44AA99", "#661100"))
#swatch(Pstate)
colors <- paste(Pstate,collapse = '", "')
ColourScal <- paste('d3.scaleOrdinal(["', colors, '"])')

#ITERATIONS STOPS THE ALGORITHM FROM REORDERING
sk1 <- sankeyNetwork(Links = df, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "length_Protected", NodeID = "name",NodeGroup = "node_group", 
                     sinksRight=FALSE, nodeWidth=40, fontSize=24, nodePadding=2,LinkGroup="group",iterations = 0,colourScale=ColourScal) 
sk1

#push labels outside the plot
sk1$x$nodes <-
  sk1$x$nodes %>% 
  mutate(is_source_node = name %in% df$from)

sk1 <- htmlwidgets::onRender(
  sk1,
  '
  function(el,x) {
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return d.is_source_node; })
    .attr("x", x.options.nodeWidth - 50)
    .attr("text-anchor", "end");
  
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return !d.is_source_node; })
    .attr("x", x.options.nodeWidth +5)
    .attr("text-anchor", "start");
  }
  '
)
sk1

# save it as an html
saveNetwork(sk1, "outputs/SankeyProtection_huc12.html")

# convert it as jpeg
webshot("outputs/SankeyProtection_huc12.html","outputs/SankeyProtection_huc12.jpeg", vwidth = 1800, vheight = 2200, zoom = 3)


#--------------------------------------------
#Barplot with protection in versus out

inState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "in")],PROTECTION$To[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="in")],sum)
outOfState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "out")],PROTECTION$To[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="out")],sum)
OeIn <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "in"],PROTECTION$To[PROTECTION$INOUT == "in"],sum)
OeOut <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "out"],PROTECTION$To[PROTECTION$INOUT == "out"],sum)
inState <- inState*100/OeIn[match(names(inState),names(OeIn))]
outOfState <- outOfState*100/OeOut[match(names(outOfState),names(OeOut))]

OR <- names(inState[order(inState,decreasing=T)]) #use to order states according to total protection

PROTECTION$INOUT <-  ifelse(PROTECTION$From == PROTECTION$To,"in","out")
PROTECTION$RIP_Class <- factor(PROTECTION$RIP_Class)
PROTECTION$To <- factor(PROTECTION$To,levels=OR)

vari = c("Class 3","Class 2","Class 1")

F_stat_state <- aggregate(OverallProtection_Len_m ~ To + RIP_Class,sum,data=PROTECTION[which(PROTECTION$INOUT == "in"),],drop=F,na.action=na.pass)
F_stat_stateT <- aggregate(Total_Length_m ~ To,sum,data=PROTECTION[which(PROTECTION$INOUT == "in"),],na.action=na.pass)
F_stat_state$protect <- F_stat_state$OverallProtection_Len_m*100/F_stat_stateT$Total_Length_m[match(F_stat_state$To,F_stat_stateT$To)]
F_stat_state$INOUT = "in"


F_stat_stateOut <- aggregate(OverallProtection_Len_m ~ To + RIP_Class,sum,data=PROTECTION[which(PROTECTION$INOUT == "out"),],drop=F,na.action=na.pass)
F_stat_stateTOut <- aggregate(Total_Length_m ~ To,sum,data=PROTECTION[which(PROTECTION$INOUT == "out"),],na.action=na.pass)
F_stat_stateOut$protect <- F_stat_stateOut$OverallProtection_Len_m*100/F_stat_stateTOut$Total_Length_m[match(F_stat_stateOut$To,F_stat_stateTOut$To)]
F_stat_stateOut$INOUT = "out"

state_plot <- acast(F_stat_state, To~RIP_Class, value.var="protect")
state_plot <- ifelse(is.na(state_plot)==T,0,state_plot)
state_plot <- state_plot[,!colnames(state_plot) %in% c("Class 4","Unprotected")]

state_plotOut <- acast(F_stat_stateOut, To~RIP_Class, value.var="protect")
state_plotOut <- ifelse(is.na(state_plotOut)==T,0,state_plotOut)
state_plotOut <- state_plotOut[,!colnames(state_plotOut) %in% c("Class 4","Unprotected")]

#little trick to have negative values for out os state
state_plotOut <- state_plotOut*-1

sort(apply(state_plot,1,sum) + apply(state_plotOut,1,sum))

cols_bar <- (c("black","#CCCCCC","#C500FF","#00C5FF","#0070FF"))

jpeg("outputs/BarplotStateInOut.jpeg", units="in", width=18, height=13, res=300, pointsize = 19.5)
par(mar=c(1,4,1,0))
b=barplot(t(state_plot),horiz=F,las=1,ylab="",cex.names=0.9,col=rev(cols_bar),ylim=c(-60,60),legend=F,cex.lab=1.4,las=2,axes=F,border=rev(cols_bar),names=rep(NA,length(rownames(state_plot))))#
barplot(t(state_plotOut),horiz=F,col=alpha(rev(cols_bar),0.5),axes=F,border=alpha(rev(cols_bar),0.5),names=rep(NA,length(rownames(state_plot))),add=T)#
par(xpd=NA)
text(b-0.6,apply(state_plot,1,sum)+1,colnames(t(state_plot)),srt=90,pos=4,hjust=0)
axis(2,las=3,pos=-0.5,cex.axis=1.2,at=c(-60,-40,-20,0,20,40,60),labels=c(60,40,20,0,20,40,60))
mtext(side=2,"Protected river length (%)",cex=1.5,line=1.8)
arrows(x0=62.5,y0=0,x1=62.5,y1=60,length=0.1,lwd=2)
arrows(x0=62.5,y0=0,x1=62.5,y1=-60,length=0.1,col=grey(0.4),lwd=2)
text(61.5, 50, labels = 'In-state', xpd = NA, srt = -270,cex=1.2,font=3)
text(61.5, -48, labels = 'Out-of-state', xpd = NA, srt = -270,cex=1.2,font=3,col=grey(0.4))

#legend(50,60,pch=15,pt.cex=2.5,col=c("#C500FF","#00C5FF","#0070FF"),c("Class 3","Class 2","Class 1"),bty="n",cex=1.2)

#add custom legend
g = grid_legend(0.5, 0.25, pch=15, col=rev(c("#C500FF","#00C5FF","#0070FF")),
                frame = F,
                size=2,
                hgap = 0,
                vgap=1,
                labels=rev(c("Limited","Effective","Comprehensive")),
                gp_title = gpar(cex = 1.2),
                title = "Viable protection", draw=FALSE)

grid.draw(grobTree(g, vp=viewport(x=0.8,y=0.2,angle=-270)))

dev.off()

#----------------------------------------------------------------
#Exemple Colorado River Basin
#---------------------------------------------------------------
PROTECTION <- read.csv("data/Protection_CRBstates_InOut_huc12.csv")
names(PROTECTION)[names(PROTECTION)=="IN"] <- "To"
names(PROTECTION)[names(PROTECTION)=="State"] <- "From"

PROTECTION$Dir <- paste(PROTECTION$From,PROTECTION$To,sep="_")


#--------------------------------------------------------
#sankey diagram CRB states - viable protection

#percentage of state-specific protection
#river miles viable protection for each connection from --> to
P <- tapply(PROTECTION$OverallProtection_Len_m[PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3")],PROTECTION$Dir[PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3")],sum)

#Option 3: percentage of viable protection in-state versus out of state
PROTECTION$INOUT <-  ifelse(PROTECTION$From == PROTECTION$To,"in","out")

inState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "in")],PROTECTION$To[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="in")],sum)
outOfState <- tapply(PROTECTION$OverallProtection_Len_m[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT == "out")],PROTECTION$Dir[which(PROTECTION$RIP_Class %in% c("Class 1","Class 2","Class 3") & PROTECTION$INOUT=="out")],sum)

OeIn <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "in"],PROTECTION$To[PROTECTION$INOUT == "in"],sum)#total river miles in state
OeOut <- tapply(PROTECTION$Total_Length_m[PROTECTION$INOUT == "out"],PROTECTION$To[PROTECTION$INOUT == "out"],sum)#total river miles coming from other state

inState <- inState*100/OeIn[match(names(inState),names(OeIn))]
outOfState <- outOfState*100/OeOut[match(sapply(strsplit(names(outOfState),"_"),'[',2),names(OeOut))]

df <- data.frame(from=c(names(inState),sapply(strsplit(names(outOfState),"_"),'[',1)),to=c(names(inState),sapply(strsplit(names(outOfState),"_"),'[',2)),length_Protected = c(inState,outOfState))

#reorder both in and out using the same order
ff <- ifelse(df$to==df$from,1,0)#identify in state connections

le <- df$from[ff==1] #set levels
df$from <- factor(df$from,levels=le)
nff1 <- order(df$from[ff==1])
nff2 <- order(df$from[ff==0])
df <- rbind(df[ff==1,][nff1,],df[ff==0,][nff2,])

#little trick to order the nodes
df$to <- paste(df$to, " ")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(df$from), as.character(df$to)) %>% unique())
nodes$node_group <- gsub(" ","_", nodes$name) #spaces are not allowed

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
df$IDsource <- match(df$from, nodes$name)-1 
df$IDtarget <- match(df$to, nodes$name)-1
df$group <- as.factor(gsub(" ","_",df$from))

# prepare colour scale
Pstate <- c("#332288", "#44AA99", "#661100","#FF9200","#FFCD73","#7BA246","#D37681")

colors <- paste(Pstate,collapse = '", "')
ColourScal <- paste('d3.scaleOrdinal(["', colors, '"])')

#ITERATIONS STOPS THE ALGORITHM FROM REORDERING
sk1 <- sankeyNetwork(Links = df, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "length_Protected", NodeID = "name",NodeGroup = "node_group", 
                     sinksRight=FALSE, nodeWidth=40, fontSize=24, nodePadding=2,LinkGroup="group",iterations = 0,colourScale=ColourScal) 
sk1

#push labels outside the plot
sk1$x$nodes <-
  sk1$x$nodes %>% 
  mutate(is_source_node = name %in% df$from)

sk1 <- htmlwidgets::onRender(
  sk1,
  '
  function(el,x) {
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return d.is_source_node; })
    .attr("x", x.options.nodeWidth - 50)
    .attr("text-anchor", "end");
  
  d3.select(el)
    .selectAll(".node text")
    .filter(function(d) { return !d.is_source_node; })
    .attr("x", x.options.nodeWidth +5)
    .attr("text-anchor", "start");
  }
  '
)
sk1

# save it as an html
saveNetwork(sk1, "outputs/SankeyProtection_CRB.html")

# convert it as jpeg
webshot("outputs/SankeyProtection_CRB.html","outputs/SankeyProtection_CRB.jpeg", vwidth = 1000, vheight = 700, zoom = 3)


#--------------------------------------------
#Inset summary

df$INOUT <-  ifelse(df$from == df$to,"in","out")
df$to <- factor(df$to,levels=rev(levels(factor(df$to))))
df$from <- factor(df$from,levels=rev(levels(factor(df$from))))

tapply(df$length_Protected[df$INOUT=="out"],df$to[df$INOUT=="out"],sum)

tapply(df$length_Protected[df$INOUT=="in"],df$to[df$INOUT=="in"],sum)

state_plotOut <- acast(df[df$INOUT=="out",], to~from, value.var="length_Protected")
state_plotOut <- ifelse(is.na(state_plotOut)==T,0,state_plotOut*-1)

state_plot <- acast(df[df$INOUT=="in",], to~from, value.var="length_Protected")
state_plot <- ifelse(is.na(state_plot)==T,0,state_plot)

Pstate <- c("#332288", "#44AA99", "#661100","#FF9200","#FFCD73","#7BA246","#D37681")

jpeg("outputs/Inset_CRB.jpeg", units="in", width=4, height=2, res=300, pointsize = 9.5)
par(mar=c(.5,0.5,1.7,0.5))
b=barplot(t(state_plot),horiz=T,las=1,ylab="",cex.names=0.9,col=alpha(rev(Pstate),0.9),xlim=c(-50,50),legend=F,cex.lab=1.4,las=2,axes=F,border="transparent",names=rep(NA,length(rownames(state_plot))))#
barplot(t(state_plotOut),horiz=T,col=alpha(rev(Pstate),0.4),axes=F,border="transparent",names=rep(NA,length(rownames(state_plot))),add=T)#
abline(v=0)
par(xpd=NA)
#text(apply(state_plot,1,sum)+1,b,colnames(t(state_plot)),srt=0,pos=4,hjust=0)
axis(3,las=1,pos=8.5,cex.axis=1.2,at=c(-50,-25,0,25,50),labels=c(50,25,0,25,50))
dev.off()
