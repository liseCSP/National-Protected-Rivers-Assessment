#-----------------------------------------------
#Matrix of weights
#----------------------------------------------

#Lise Comte, April 2025
#in RStudio 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.06.0+421 Chrome/110.0.5481.208 Electron/23.3.0 Safari/537.36

#Data can be found at: https://figshare.com/s/62d2f6da57b9526a9aee

library(corrplot)

#------------------------------------
#Figure

W_nice <- read.csv('data/Matrix of weights for figure_clean.csv')#to get the labels

rownames(W_nice) <- W_nice[,1]
W_nice <- as.matrix(W_nice[,-1])

jpeg("outputs/HeatMap_complex.jpeg", units="in", width=7, height=10, res=300, pointsize = 15)
par(xpd=NA)
corrplot((W_nice),addCoef.col="transparent",method="square",col.lim=c(0,1),tl.col ="black",tl.srt =90, mar = c(0, 0, 0, 0),tl.cex=0.6, cl.pos="n")
dev.off()




