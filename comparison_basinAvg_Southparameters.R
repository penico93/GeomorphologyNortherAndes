# Basin variables comparison
# This script plots and compares basin averaged geomorphic parameters
# from basins in the Northern Andes of Colombia
# This script is part of a paper by Nicolas Perez



dataBasins <- read.csv("/Users/penico93/Documents/Papers in prep/Paper Geomorphology/Methods-analysis/basinsSouth_exportdata.csv")

dataBasins$color <- "NoColor"
dataBasins$location <- as.character(dataBasins$location)
dataBasins[dataBasins$river_mout==45,'location']  <- as.factor("Cauca")

dataBasins <- na.omit(dataBasins)
# Patia      upperCauca westCC     westWC     eastWC     <NA>      
#   Levels: eastWC Patia upperCauca westCC westWC

# #unique location 2 [1] PatiaBasin        CaucaBasin        CaucaBasinPlateau PatiaBasinPlateau
# Levels: CaucaBasin CaucaBasinPlateau PatiaBasin PatiaBasinPlateau

# dataBasins[dataBasins$location=='easternCC','Color'] <- '#a6cee3'
# dataBasins[dataBasins$location=='northernCC','Color'] <- '#1f78b4'
# dataBasins[dataBasins$location=='westernCC','Color'] <- '#b2df8a'
# dataBasins[dataBasins$location=='easternWC','Color'] <- '#33a02c'
dataBasins[dataBasins$location2=='PatiaBasin','color'] <- '#fb9a99'

dataBasins[( (dataBasins$location=='eastWC') & (dataBasins$location2=='PatiaBasinPlateau')),'color']  <- '#a6cee3'
dataBasins[( (dataBasins$location=='westCC') & (dataBasins$location2=='PatiaBasinPlateau')),'color']  <- '#a6cee3'
dataBasins[( (dataBasins$location=='eastWC') & (dataBasins$location2=='CaucaBasinPlateau')),'color']  <- '#ffff33'
dataBasins[( (dataBasins$location=='westCC') & (dataBasins$location2=='PatiaBasin')),'color']  <- '#a6cee3'
dataBasins[( (dataBasins$location=='westCC') & (dataBasins$location2=='CaucaBasin')),'color']  <- '#1f78b4'
dataBasins[( (dataBasins$location=='westCC') & (dataBasins$location2=='CaucaBasinPlateau')),'color']  <- '#ffff33'

dataBasins[( dataBasins$location=='Patia'),'color']  <- 'black'
dataBasins[( dataBasins$location=='upperCauca') ,'color']  <- 'grey'


dataBasins[(dataBasins$river_mout %in% c(50,49,46,45,56,47,67,66,48)),'color']  <- '#ffff33'



dataBasins[( (dataBasins$location=='eastWC') & (dataBasins$location2=='PatiaBasin')),'color'] <- '#b2df8a'
dataBasins[( (dataBasins$location=='eastWC') & (dataBasins$location2=='CaucaBasin')),'color'] <- '#33a02c'

dataBasins[( (dataBasins$location=='westWC') & (dataBasins$location2=='PatiaBasin')),'color'] <- '#e31a1c'
dataBasins[( (dataBasins$location=='westWC') & (dataBasins$location2=='CaucaBasin')),'color'] <- '#fb9a99'

dataBasins[(dataBasins$river_mout %in% c(50,49,46,45,56,47,67,66,48)),'color']  <- '#ffff33'

dataBasins[( dataBasins$location=='Patia'),'color']  <- 'black'
dataBasins[( dataBasins$location=='upperCauca') ,'color']  <- 'grey'

#Order dataframe
dataBasins$order <- NA

dataBasins[dataBasins$color=='#ffff33','order'] <- 7 #Plateau upper Cauca Basin 255, 255, 51
dataBasins[dataBasins$color=='#a6cee3','order'] <- 6 #west CC Patia Basin 166, 206, 227
dataBasins[dataBasins$color=='#1f78b4','order'] <- 5 #west CC Cauca Basin 31, 120, 180
dataBasins[dataBasins$color=='#b2df8a','order'] <- 4 #east WC Patia Basin 178, 223, 138
dataBasins[dataBasins$color=='#33a02c','order'] <- 3 #east WC Cauca Basin 51, 160, 44
dataBasins[dataBasins$color=='#e31a1c','order'] <- 2 #West WC Patia Basin 227, 26, 28
dataBasins[dataBasins$color=='#fb9a99','order'] <- 1 #West WC Cauca Basin 251, 154, 153


dataBasins <- dataBasins[order(dataBasins$order),]




setwd('/Users/penico93/Documents/Papers in prep/Paper Geomorphology/Methods-analysis/Geomorphology_github_repo/GeomorphologyNortherAndes')


filename1 <- "Basin_statsPatiaScatter.eps"
setEPS()
# 
postscript(filename1,width=(4),height=(12))

par(fig=c(0,0.8,0,0.8))
par(mar=c(3, 0.5, 1.5, 0.2), mfrow=c(3,1),
    oma = c(4.5, 6, 0.2, 0.2))
#c(bottom, left, top, right))

plot(dataBasins$mean_ksn,dataBasins$mean_gradi,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hillslope gradient [m/m]", side = 2.5, line = 3, cex = 1.5)
mtext("Channel steepness [ksn]", side = 1, line = 3, cex = 1.5)
arrows(dataBasins$mean_ksn, dataBasins$mean_gradi+(dataBasins$std_gradie), 
       dataBasins$mean_ksn, dataBasins$mean_gradi-(dataBasins$std_gradie), length=0, angle=90,col='gray')
arrows(dataBasins$mean_ksn+dataBasins$std_ksn, dataBasins$mean_gradi, 
       dataBasins$mean_ksn-dataBasins$std_ksn, dataBasins$mean_gradi, length=0, angle=90,col='gray')
points(dataBasins$mean_ksn,dataBasins$mean_gradi,
       bg=dataBasins$color,cex=1.5,pch=21)

plot(dataBasins$mean_ksn,dataBasins$mean_preci,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Precipitation [m]", side = 2.5, line = 3, cex = 1.5)
mtext("Channel steepness [ksn]", side = 1, line = 3, cex = 1.5)
arrows(dataBasins$mean_ksn, dataBasins$mean_preci+(dataBasins$std_precip), 
       dataBasins$mean_ksn, dataBasins$mean_preci-(dataBasins$std_precip), length=0, angle=90,col='gray')
arrows(dataBasins$mean_ksn+dataBasins$std_ksn, dataBasins$mean_preci, 
       dataBasins$mean_ksn-dataBasins$std_ksn, dataBasins$mean_preci, length=0, angle=90,col='gray')
points(dataBasins$mean_ksn,dataBasins$mean_preci,
       bg=dataBasins$color,cex=1.5,pch=21)

plot(dataBasins$mean_gradi,dataBasins$mean_preci,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Precipitation [m]", side = 2.5, line = 3, cex = 1.5)
mtext("Hillslope gradient [m/m]", side = 1, line = 3, cex = 1.5)
arrows(dataBasins$mean_gradi, dataBasins$mean_preci+(dataBasins$std_precip), 
       dataBasins$mean_gradi, dataBasins$mean_preci-(dataBasins$std_precip), length=0, angle=90,col='gray')
arrows(dataBasins$mean_gradi+dataBasins$std_gradie, dataBasins$mean_preci, 
       dataBasins$mean_gradi-dataBasins$std_gradie, dataBasins$mean_preci, length=0, angle=90,col='gray')
points(dataBasins$mean_gradi,dataBasins$mean_preci,
       bg=dataBasins$color,cex=1.5,pch=21)


dev.off()

### Plot Boxplots


filename1 <- "Basin_statsWesternCordilleraKSN.eps"
setEPS()
# 
postscript(filename1,width=(4),height=(4))

par(fig=c(0,0.8,0,0.8))
par(mar=c(3, 0.5, 1.5, 0.2), mfrow=c(1,1),
    oma = c(4.5, 6, 0.2, 0.2))
#c(bottom, left, top, right))


dataBasinsWC <- dataBasins[dataBasins$location=='westWC',]

plot(dataBasinsWC$outlet_lat,dataBasinsWC$mean_ksn,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasinsWC$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Channel steepness [ksn]", side = 2.5, line = 3, cex = 1.5)
mtext("Latitude [deg N]", side = 1, line = 3, cex = 1.5)

arrows(dataBasinsWC$outlet_lat, dataBasinsWC$mean_ksn+(dataBasinsWC$std_ksn), 
       dataBasinsWC$outlet_lat, dataBasinsWC$mean_ksn-(dataBasinsWC$std_ksn), length=0, angle=90,col='gray')

points(dataBasinsWC$outlet_lat,dataBasinsWC$mean_ksn,
       bg=dataBasinsWC$color,cex=1.5,pch=21)

dev.off()




plot(dataBasinsWC$outlet_lat,dataBasinsWC$mean_gradi,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasinsWC$color,cex=1.5,pch=21,ylim=c(0.1,.7))
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hillslope gradient [m/m]", side = 2.5, line = 3, cex = 1.5)
mtext("Latitude [deg N]", side = 1, line = 3, cex = 1.5)

arrows(dataBasinsWC$outlet_lat, dataBasinsWC$mean_gradi+(dataBasinsWC$std_gradie), 
       dataBasinsWC$outlet_lat, dataBasinsWC$mean_gradi-(dataBasinsWC$std_gradie), length=0, angle=90,col='gray')

points(dataBasinsWC$outlet_lat,dataBasinsWC$mean_ksn,
       bg=dataBasinsWC$color,cex=1.5,pch=21)

plot(dataBasinsWC$outlet_lat,dataBasinsWC$hyp_int,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasinsWC$color,cex=1.5,pch=21,ylim=c(0.1,.7))
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hypsometric integral [m/m]", side = 2.5, line = 3, cex = 1.5)
mtext("Latitude [deg N]", side = 1, line = 3, cex = 1.5)

arrows(dataBasinsWC$outlet_lat, dataBasinsWC$mean_gradi+(dataBasinsWC$std_gradie), 
       dataBasinsWC$outlet_lat, dataBasinsWC$mean_gradi-(dataBasinsWC$std_gradie), length=0, angle=90,col='gray')

points(dataBasinsWC$outlet_lat,dataBasinsWC$mean_ksn,
       bg=dataBasinsWC$color,cex=1.5,pch=21)



dataBasinsCaPa <- dataBasins[!dataBasins$location=='westWC',]


plot(dataBasinsCaPa$outlet_lat,dataBasinsCaPa$mean_ksn,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasinsCaPa$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Channel steepness [ksn]", side = 2.5, line = 3, cex = 1.5)
mtext("Latitude [deg N]", side = 1, line = 3, cex = 1.5)

arrows(dataBasinsCaPa$outlet_lat, dataBasinsCaPa$mean_ksn+(dataBasinsCaPa$std_ksn), 
       dataBasinsCaPa$outlet_lat, dataBasinsCaPa$mean_ksn-(dataBasinsCaPa$std_ksn), length=0, angle=90,col='gray')

points(dataBasinsCaPa$outlet_lat,dataBasinsCaPa$mean_ksn,
       bg=dataBasinsCaPa$color,cex=1.5,pch=21)



plot(dataBasins$mean_preci,dataBasins$hyp_int,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hypsometric Integral", side = 2.5, line = 3, cex = 1.5)
mtext("Precipiation [m/yr]", side = 1, line = 3, cex = 1.5)

cor.test(dataBasins$mean_preci,dataBasins$hyp_int)


dataBasins[dataBasins$color=='#ffff33','order'] <- 7 #Plateau upper Cauca Basin
dataBasins[dataBasins$color=='#a6cee3','order'] <- 6 #west CC Patia Basin
dataBasins[dataBasins$color=='#1f78b4','order'] <- 5 #west CC Cauca Basin
dataBasins[dataBasins$color=='#b2df8a','order'] <- 4 #east WC Patia Basin
dataBasins[dataBasins$color=='#33a02c','order'] <- 3 #east WC Cauca Basin
dataBasins[dataBasins$color=='#e31a1c','order'] <- 2 #West WC Patia Basin
dataBasins[dataBasins$color=='#fb9a99','order'] <- 1 #West WC Cauca Basi





filename1 <- "Basin_boxplotsPatia.eps"
setEPS()
# 
postscript(filename1,width=(4),height=(12))

par(fig=c(0,0.8,0,0.8))
par(mar=c(3, 0.5, 1.5, 0.2), mfrow=c(3,1),
    oma = c(4.5, 6, 0.2, 0.2))
#c(bottom, left, top, right))



boxplot(dataBasins$mean_ksn ~ dataBasins$order, col= c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33'), 
        ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,boxwex=0.2)
axis(1,cex.axis=1,at = c(1:7),labels = c("west WC Cauca","west WC Patia","east WC Cauca",
                                         "east WC Patia","west CC Cauca","west CC Patia", "upper Cauca"),las=3)
axis(2,cex.axis=1)
mtext("Channel steepness [ksn]", side = 2.5, line =2, cex = 1)


mylevels <- c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33')

levelProportions <- summary(as.factor(dataBasins$color))/nrow(dataBasins)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- dataBasins[dataBasins$color==thislevel, "mean_ksn"]
  
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=21, bg=c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33')[i]) 
}



boxplot(dataBasins$mean_gradi ~ dataBasins$order, col= c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33'), 
        ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,boxwex=0.2)
axis(1,cex.axis=1,at = c(1:7),labels = c("west WC Cauca","west WC Patia","east WC Cauca",
                                         "east WC Patia","west CC Cauca","west CC Patia", "upper Cauca"),las=3)
axis(2,cex.axis=1)
mtext("Hillslope gradient [m/m]", side = 2.5, line =2, cex = 1)


mylevels <- c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33')

levelProportions <- summary(as.factor(dataBasins$color))/nrow(dataBasins)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- dataBasins[dataBasins$color==thislevel, "mean_gradi"]
  
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=21, bg=c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33')[i]) 
}




boxplot(dataBasins$mean_preci ~ dataBasins$order, col= c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33'), 
        ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,boxwex=0.2)
axis(1,cex.axis=1,at = c(1:7),labels = c("west WC Cauca","west WC Patia","east WC Cauca",
                                         "east WC Patia","west CC Cauca","west CC Patia", "upper Cauca"),las=3)

axis(2,cex.axis=1)
mtext("Precipitation [m]", side = 2.5, line =2, cex = 1)


mylevels <- c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33')

levelProportions <- summary(as.factor(dataBasins$color))/nrow(dataBasins)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- dataBasins[dataBasins$color==thislevel, "mean_preci"]
  
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=21, bg=c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33')[i]) 
}

dev.off()





filename3 <- "Hypsometry_PatiaBoxplotandScatter3.eps"

setEPS()
# 
postscript(filename3,width=(10),height=(8))

par(fig=c(0,0.8,0,0.8))
par(mar=c(3, 0.5, 3, 3), mfrow=c(2,2),
    oma = c(4.5, 6, 0.2, 0.2))


plot(dataBasinsWC$outlet_lat,dataBasinsWC$mean_ksn,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasinsWC$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Channel steepness [ksn]", side = 2.5, line = 3, cex = 1.5)
mtext("Latitude [deg N]", side = 1, line = 3, cex = 1.5)

arrows(dataBasinsWC$outlet_lat, dataBasinsWC$mean_ksn+(dataBasinsWC$std_ksn), 
       dataBasinsWC$outlet_lat, dataBasinsWC$mean_ksn-(dataBasinsWC$std_ksn), length=0, angle=90,col='gray')

points(dataBasinsWC$outlet_lat,dataBasinsWC$mean_ksn,
       bg=dataBasinsWC$color,cex=1.5,pch=21)



plot(dataBasinsCaPa$outlet_lat,dataBasinsCaPa$mean_ksn,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasinsCaPa$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Channel steepness [ksn]", side = 2.5, line = 3, cex = 1.5)
mtext("Latitude [deg N]", side = 1, line = 3, cex = 1.5)

arrows(dataBasinsCaPa$outlet_lat, dataBasinsCaPa$mean_ksn+(dataBasinsCaPa$std_ksn), 
       dataBasinsCaPa$outlet_lat, dataBasinsCaPa$mean_ksn-(dataBasinsCaPa$std_ksn), length=0, angle=90,col='gray')

points(dataBasinsCaPa$outlet_lat,dataBasinsCaPa$mean_ksn,
       bg=dataBasinsCaPa$color,cex=1.5,pch=21)





boxplot(dataBasins$hyp_int ~ dataBasins$order, col= c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33'), 
        ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,boxwex=0.2)
axis(1,cex.axis=1,at = c(1:7),labels = c("west WC Cauca","west WC Patia","east WC Cauca",
                                         "east WC Patia","west CC Cauca","west CC Patia", "upper Cauca"),las=3)
axis(2,cex.axis=1)
mtext("Hypsometric Integral", side = 2.5, line =2, cex = 1)


mylevels <- c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33')

levelProportions <- summary(as.factor(dataBasins$color))/nrow(dataBasins)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- dataBasins[dataBasins$color==thislevel, "hyp_int"]
  
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=21, bg=c('#fb9a99','#e31a1c', '#33a02c', '#b2df8a', '#1f78b4', '#a6cee3','#ffff33')[i]) 
}



plot(dataBasins$mean_preci,dataBasins$hyp_int,
     ylab = "", xlab = "", axes = FALSE, main = NULL,bg=dataBasins$color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hypsometric Integral", side = 2.5, line = 3, cex = 1.5)
mtext("Precipiation [m/yr]", side = 1, line = 3, cex = 1.5)

cor.test(dataBasins$mean_preci,dataBasins$hyp_int)

dev.off()
