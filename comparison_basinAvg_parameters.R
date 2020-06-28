# Basin variables comparison
# This script plots and compares basin averaged geomorphic parameters
# from basins in the Northern Andes of Colombia
# This script is part of a paper by Nicolas Perez

dataBasins <- read.csv("/Users/penico93/Documents/Papers in prep/Paper Geomorphology/Methods-analysis/basinsNorthCCV2_exportdata.csv")


dataBasins$color <- NA
dataBasins[dataBasins$location=='easternCC','Color'] <- '#a6cee3'
dataBasins[dataBasins$location=='northernCC','Color'] <- '#1f78b4'
dataBasins[dataBasins$location=='westernCC','Color'] <- '#b2df8a'
dataBasins[dataBasins$location=='easternWC','Color'] <- '#33a02c'
dataBasins[dataBasins$location=='northernWC','Color'] <- '#fb9a99'
dataBasins[dataBasins$location=='westernWC','Color'] <- '#e31a1c'


dataBasins$order <- NA
dataBasins[dataBasins$location=='easternCC','order'] <- 6
dataBasins[dataBasins$location=='northernCC','order'] <- 5
dataBasins[dataBasins$location=='westernCC','order'] <- 4
dataBasins[dataBasins$location=='easternWC','order'] <- 3
dataBasins[dataBasins$location=='northernWC','order'] <- 2
dataBasins[dataBasins$location=='westernWC','order'] <- 1


dataBasins <- dataBasins[order(dataBasins$order),]


plot(density(dataBasins$mean_gradi[dataBasins$location=='easternCC']), ylim=c(0,10))
lines(density(dataBasins$mean_gradi[dataBasins$location=='northernCC']))
lines(density(dataBasins$mean_gradi[dataBasins$location=='westernCC']))
lines(density(dataBasins$mean_gradi[dataBasins$location=='easternWC']))
lines(density(dataBasins$mean_gradi[dataBasins$location=='northernWC']))
lines(density(dataBasins$mean_gradi[dataBasins$location=='westernWC']))

# 166,206,227
# 31,120,180
# 178,223,138
# 51,160,44
# 251,154,153
# 227,26,28

#a6cee3
#1f78b4
#b2df8a
#33a02c
#fb9a99


filename1 <- "Basin_stats.eps"
setEPS()
# 
postscript(filename1,width=(4),height=(12))

par(fig=c(0,0.8,0,0.8))
par(mar=c(3, 0.5, 1.5, 0.2), mfrow=c(3,1),
    oma = c(4.5, 6, 0.2, 0.2))
#c(bottom, left, top, right))

plot( dataBasins$mean_ksn,dataBasins$mean_gradi,
      ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$Color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hillslope gradient [m/m]", side = 2.5, line = 3, cex = 1.5)
mtext("Channel steepness [ksn]", side = 1, line = 3, cex = 1.5)
arrows(dataBasins$mean_ksn, dataBasins$mean_gradi+(dataBasins$std_gradie), 
       dataBasins$mean_ksn, dataBasins$mean_gradi-(dataBasins$std_gradie), length=0, angle=90,col='gray')
arrows(dataBasins$mean_ksn+dataBasins$std_ksn, dataBasins$mean_gradi, 
       dataBasins$mean_ksn-dataBasins$std_ksn, dataBasins$mean_gradi, length=0, angle=90,col='gray')
points(dataBasins$mean_ksn,dataBasins$mean_gradi,
       bg=dataBasins$Color,cex=1.5,pch=21)


plot(dataBasins$mean_ksn,dataBasins$mean_preci,
      ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$Color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Precipitation [m]", side = 2.5, line = 3, cex = 1.5)
mtext("Channel steepness [ksn]", side = 1, line = 3, cex = 1.5)
arrows(dataBasins$mean_ksn, dataBasins$mean_preci+(dataBasins$std_precip), 
       dataBasins$mean_ksn, dataBasins$mean_preci-(dataBasins$std_precip), length=0, angle=90,col='gray')
arrows(dataBasins$mean_ksn+dataBasins$std_ksn, dataBasins$mean_preci, 
       dataBasins$mean_ksn-dataBasins$std_ksn, dataBasins$mean_preci, length=0, angle=90,col='gray')
points(dataBasins$mean_ksn,dataBasins$mean_preci,
       bg=dataBasins$Color,cex=1.5,pch=21)

plot(dataBasins$mean_gradi,dataBasins$mean_preci,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$Color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Precipitation [m]", side = 2.5, line = 3, cex = 1.5)
mtext("Hillslope gradient [m/m]", side = 1, line = 3, cex = 1.5)
arrows(dataBasins$mean_gradi, dataBasins$mean_preci+(dataBasins$std_precip), 
       dataBasins$mean_gradi, dataBasins$mean_preci-(dataBasins$std_precip), length=0, angle=90,col='gray')
arrows(dataBasins$mean_gradi+dataBasins$std_gradie, dataBasins$mean_preci, 
       dataBasins$mean_gradi-dataBasins$std_gradie, dataBasins$mean_preci, length=0, angle=90,col='gray')
points(dataBasins$mean_gradi,dataBasins$mean_preci,
       bg=dataBasins$Color,cex=1.5,pch=21)


dev.off()



plot(density(dataBasins$mean_gradi[dataBasins$location=='easternCC']), ylim=c(0,10))
lines(density(dataBasins$mean_gradi[dataBasins$location=='northernCC']))
lines(density(dataBasins$mean_gradi[dataBasins$location=='westernCC']))
lines(density(dataBasins$mean_gradi[dataBasins$location=='easternWC']))
lines(density(dataBasins$mean_gradi[dataBasins$location=='northernWC']))
lines(density(dataBasins$mean_gradi[dataBasins$location=='westernWC']))




filename1 <- "Gradient_boxplot2.eps"


setEPS()
# 
postscript(filename1,width=(4),height=(4))

par(mar=c(5, 3, 1.5, 0.2))
#c(bottom, left, top, right))

boxplot(dataBasins$mean_gradi ~ dataBasins$order, col= c('#e31a1c','#fb9a99', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
       ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,boxwex=0.2)
axis(1,cex.axis=1,at = c(1:6),labels = c("west WC","north WC","east WC","west CC","north CC","east CC"),las=3)
axis(2,cex.axis=1)
mtext("Hillslope gradient", side = 2.5, line =2, cex = 1)


mylevels <- c("westernWC","northernWC","easternWC","westernCC","northernCC","easternCC")
levelProportions <- summary(dataBasins$location)/nrow(dataBasins)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- dataBasins[dataBasins$location==thislevel, "mean_gradi"]

  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=21, bg=c('#e31a1c','#fb9a99', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3')[i]) 
}

dev.off()


filename1 <- "KSN_boxplot2.eps"


setEPS()
# 
postscript(filename1,width=(4),height=(4))

par(mar=c(5, 3, 1.5, 0.2))
#c(bottom, left, top, right))
boxplot(dataBasins$mean_ksn ~ dataBasins$order, col= c('#e31a1c','#fb9a99', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
        ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,boxwex=0.2)
axis(1,cex.axis=1,at = c(1:6),labels = c("west WC","north WC","east WC","west CC","north CC","east CC"),las=3)
axis(2,cex.axis=1)
mtext("Channel steepness [ksn]", side = 2.5, line =2, cex = 1)


mylevels <- c("westernWC","northernWC","easternWC","westernCC","northernCC","easternCC")
levelProportions <- summary(dataBasins$location)/nrow(dataBasins)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- dataBasins[dataBasins$location==thislevel, "mean_ksn"]
  
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=21, bg=c('#e31a1c','#fb9a99', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3')[i]) 
}


dev.off()





filename1 <- "Precip_boxplot2.eps"


setEPS()
# 
postscript(filename1,width=(4),height=(4))

par(mar=c(5, 3, 1.5, 0.2))

boxplot(dataBasins$mean_preci ~ dataBasins$order, col= c('#e31a1c','#fb9a99', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
        ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,boxwex=0.2)
axis(1,cex.axis=1,at = c(1:6),labels = c("west WC","north WC","east WC","west CC","north CC","east CC"),las=3)
axis(2,cex.axis=1)
mtext("Precipitation [m]", side = 2.5, line =2, cex = 1)


mylevels <- c("westernWC","northernWC","easternWC","westernCC","northernCC","easternCC")
levelProportions <- summary(dataBasins$location)/nrow(dataBasins)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- dataBasins[dataBasins$location==thislevel, "mean_preci"]
  
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=21, bg=c('#e31a1c','#fb9a99', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3')[i]) 
}

dev.off()



plot( dataBasins$mean_ksn,dataBasins$hyp_int,
      ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$Color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hypsometric integral", side = 2.5, line = 3, cex = 1.5)
mtext("Channel steepness [ksn]", side = 1, line = 3, cex = 1.5)
arrows(dataBasins$mean_ksn, dataBasins$mean_gradi+(dataBasins$std_gradie), 
       dataBasins$mean_ksn, dataBasins$mean_gradi-(dataBasins$std_gradie), length=0, angle=90,col='gray')
arrows(dataBasins$mean_ksn+dataBasins$std_ksn, dataBasins$mean_gradi, 
       dataBasins$mean_ksn-dataBasins$std_ksn, dataBasins$mean_gradi, length=0, angle=90,col='gray')
points(dataBasins$mean_ksn,dataBasins$mean_gradi,
       bg=dataBasins$Color,cex=1.5,pch=21)


plot(dataBasins$mean_preci,dataBasins$hyp_int,
      ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$Color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hypsometric integral", side = 2.5, line = 3, cex = 1.5)
mtext("Precipitation [m]", side = 1, line = 3, cex = 1.5)
arrows(dataBasins$mean_ksn, dataBasins$mean_gradi+(dataBasins$std_gradie), 
       dataBasins$mean_ksn, dataBasins$mean_gradi-(dataBasins$std_gradie), length=0, angle=90,col='gray')
arrows(dataBasins$mean_ksn+dataBasins$std_ksn, dataBasins$mean_gradi, 
       dataBasins$mean_ksn-dataBasins$std_ksn, dataBasins$mean_gradi, length=0, angle=90,col='gray')
points(dataBasins$mean_ksn,dataBasins$mean_gradi,
       bg=dataBasins$Color,cex=1.5,pch=21)

boxplot(dataBasins$hyp_int ~ dataBasins$order, col= c('#e31a1c','#fb9a99', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3'), 
        ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,boxwex=0.2)
axis(1,cex.axis=1,at = c(1:6),labels = c("west WC","north WC","east WC","west CC","north CC","east CC"),las=3)
axis(2,cex.axis=1)
mtext("Precipitation [m]", side = 2.5, line =2, cex = 1)


mylevels <- c("westernWC","northernWC","easternWC","westernCC","northernCC","easternCC")
levelProportions <- summary(dataBasins$location)/nrow(dataBasins)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- dataBasins[dataBasins$location==thislevel, "mean_preci"]
  
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=21, bg=c('#e31a1c','#fb9a99', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3')[i]) 
}