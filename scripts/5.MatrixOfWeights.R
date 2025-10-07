#-----------------------------------------------
#Matrix of weights
#----------------------------------------------

#Lise Comte, April 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

#Data can be found at: 10.5281/zenodo.17279334

library(corrplot)

#------------------------------------
#Figure -full

W_nice <- read.csv('data/Matrix of weights for figure_clean.csv')#to get the labels

rownames(W_nice) <- W_nice[,1]
W_nice <- as.matrix(W_nice[,-1])

jpeg("outputs/HeatMap_complex.jpeg", units="in", width=7, height=10, res=300, pointsize = 15)
par(xpd=NA)
corrplot((W_nice),addCoef.col="transparent",method="square",col.lim=c(0,1),tl.col ="black",tl.srt =90, mar = c(0, 0, 0, 0),tl.cex=0.6, cl.pos="n")
dev.off()

#---------------------------------------------------------------
#Simplified version
#---------------------------------------------------------------
#which are the most common subcategories?
net_protect_seg_fin <- read.csv("data/NPRALayer_segment_managementType.csv")
table(net_protect_seg_fin$IUCNI_Des_Tp,net_protect_seg_fin$IUCNI_Gap_Sts) 
table(net_protect_seg_fin$IUCNII_Des_Tp,net_protect_seg_fin$IUCNII_Gap_Sts) 
table(net_protect_seg_fin$RFPA_Des_Tp,net_protect_seg_fin$RFPA_Gap_Sts) 
table(net_protect_seg_fin$FPA_Des_Tp,net_protect_seg_fin$FPA_Gap_Sts) 
table(net_protect_seg_fin$ONRW_Des_Tp)
table(net_protect_seg_fin$IUCNIII_Des_Tp,net_protect_seg_fin$IUCNIII_Gap_Sts) 
table(net_protect_seg_fin$IUCNIV_Des_Tp,net_protect_seg_fin$IUCNIV_Gap_Sts)
table(net_protect_seg_fin$IUCNV_Des_Tp,net_protect_seg_fin$IUCNV_Gap_Sts)
table(net_protect_seg_fin$IUCNVI_Des_Tp,net_protect_seg_fin$IUCNVI_Gap_Sts)
table(net_protect_seg_fin$IUCNOTH_Des_Tp,net_protect_seg_fin$IUCNOTH_Gap_Sts)
bn <- table(net_protect_seg_fin$Gap3_Des_Tp,net_protect_seg_fin$Gap3_Gap_Sts)
bn[order(bn[,2]),]

#------------
#-edit the file by hand to pick categories [Matrix of weights for figure_clean.csv]
#------------

#FIGURE
library(RColorBrewer); library(corrplot)
W_nice <- read.csv('data/Matrix of weights for figure_clean_simplified.csv')#to get the labels


jpeg("outputs/HeatMap_simplified.jpeg", units="in", width=10, height=7, res=300, pointsize = 15)
M <- t(W_nice)
rownames(M)[rownames(M)=='WaterQuality'] <- "Water Quality"

# palettes
pal_blue <- colorRampPalette(brewer.pal(5, "Blues"))(5)
pal_red  <- colorRampPalette(c("#fff5f0", "#cb181d"))(200)

par(xpd = NA)

# 1) draw the key attributes
res <- corrplot(
  M, method = "square",
  col = pal_blue,
  is.corr = FALSE,          
  col.lim = c(0, 1),
  tl.col = "black", tl.srt = 90, tl.cex = 0.6,
  cl.pos = "n", mar = c(0, 0, 0, 0),
  #tl.pos = "b"
)

# 2) add total weights
nr <- nrow(M)
last_row_name <- rownames(M)[nr]
pos <- res$corrPos                             
pos_last <- pos[pos$yName == last_row_name, ]   

vals <- pmin(1, pmax(0, M[nr, ]))              
idx  <- floor(vals * (length(pal_red) - 1)) + 1
cols <- pal_red[idx]

with(pos_last, rect(x - 0.5, y - 0.5, x + 0.5, y + 0.5, col = cols, border = "grey"))

dev.off()




