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



#a6cee3
#1f78b4
#b2df8a
#33a02c
#fb9a99
#e31a1c


plot( dataBasins$mean_ksn,dataBasins$mean_gradi,
      ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,bg=dataBasins$Color,cex=1.5,pch=21)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Hillslope gradient [m/m]", side = 2.5, line = 3, cex = 1.5)
mtext("Channel steepness [ksn]", side = 1, line = 3, cex = 1.5)
arrows(data2_subset1.5$FocalSt50c, data2_subset1.5$TRMMproj+(data2_subset1.5$extract_trmm_sd/1000), 
       data2_subset1.5$FocalSt50c, data2_subset1.5$TRMMproj-(data2_subset1.5$extract_trmm_sd/1000), length=0, angle=90,col='gray')
arrows(data2_subset1.5$FocalSt50c+data2_subset1.5$extract_hillslope_sd, data2_subset1.5$TRMMproj, 
       data2_subset1.5$FocalSt50c-data2_subset1.5$extract_hillslope_sd, data2_subset1.5$TRMMproj, length=0, angle=90,col='gray')


points(data2_subset1.5$FocalSt50c,data2_subset1.5$TRMMproj,
       bg=data2_subset1.5$Color,pch=data2_subset1.5$Pch)
