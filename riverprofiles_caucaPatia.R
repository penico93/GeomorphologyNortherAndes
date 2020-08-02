#Figure River Profiles



# setwd('/Users/penico93/Documents/Papers in prep/Paper Geomorphology/Methods-analysis/Geomorphology_github_repo/GeomorphologyNortherAndes/')

data2 = read.table("riverdata_200m.txt", header = TRUE, sep= '\t')

# id_profile = 1200

cc <- data2[data2$id_profile==1201,]

cc$L2 <- cc$L/1000

plot(cc$L2,cc$z,type='l',lwd=3,xlab = "Distance [km]",ylab = "Elevation [m]",cex.lab =1.5,cex.axis = 1.5)

setwd("/Users/penico93/Documents/Abstracts for meetings/GSA 2018 Presentation/Figures river profiles/")
png(filename="PatíaRiverProfile.png", units = 'cm', width=(15),height=(15), res = 600)
plot(cc$L2,cc$z,type='l',lwd=3,xlab = "Distance [km]",ylab = "Elevation [m]",cex.lab =1.5,cex.axis = 1.5)
dev.off()




cc2 <- data2[data2$id_profile==1,]

cc2$L2 <- cc2$L/1000
cc2$z2 <- cc2$z/1000
plot(cc2$L2,cc2$z2,type='l',lwd=3,xlab = "Distance [km]",ylab = "Elevation [km]",cex.lab =1.5,cex.axis = 1.5,las=1)

setwd("/Users/penico93/Documents/Abstracts for meetings/GSA 2018 Presentation/Figures river profiles/")
png(filename="CaucaRiverProfile.png", units = 'cm', width=(16),height=(12), res = 600)
plot(cc2$L2,cc2$z2,type='l',lwd=3,xlab = "Distance [km]",ylab = "Elevation [m]",cex.lab =1.5,cex.axis = 1.5,las=1)
dev.off()



setEPS()
postscript("CaucaProf.eps")
plot(cc$L2,cc$z2,type='l',lwd=3,xlab = "Distance [km]",ylab = "Elevation [m]",cex.lab =1.5,cex.axis = 1.5,las=1)
dev.off()






#Plot Elevation vs Chi for Patia river

plot(cc2$chi,cc2$z)
points(cc$chi,cc$z)
plot(cc$L,cc$ksn)
     
     #type='l',lwd=3,xlab = "Distance [km]",ylab = "Elevation [m]",cex.lab =1.5,cex.axis = 1.5)







plot(cc$L2,cc$z,
     ylab = "", xlab = "", axes = FALSE, main = NULL,cex=1.5,type='l')
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Elevation [m]", side = 2.5, line = 3, cex = 1.5)
mtext("Distance [km]", side = 1, line = 3, cex = 1.5)

lines(cc2$L2,cc2$z)




plot(cc2$L2,cc2$z2)

dataBasins <- dataBasins[order(dataBasins$order),]

cc2 <- cc2[order(cc2$L2),]

CaucaRiverData <- cc2[cc2$L2<250,c('L2','z')]

CaucaRiverData <- CaucaRiverData[!(CaucaRiverData$L2>35 & CaucaRiverData$L2<70),c('L2','z')]

write.csv(CaucaRiverData,'upperCaucaRaw.csv')



cc2 <- cc2[!(cc2$L2>35 & cc2$L2<70),]

plot(cc2$L2,cc2$z)

x <- seq(35,69,0.2)
y<- 0.0245*(x^2) - 6.2009*x + 1341.9
points(x,y)

caucaProfileCorrected <- data.frame(x = c(x,cc2$L2),y = c(y,cc2$z))
caucaProfileCorrected <- caucaProfileCorrected[order(caucaProfileCorrected$x),]








setwd('/Users/penico93/Documents/Papers in prep/Paper Geomorphology/Methods-analysis/')
filename1 <- "Profiles_comparison_CaucaPatia.eps"
setEPS()

postscript(filename1,width=(6),height=(8))

par(fig=c(0,0.8,0,0.8))
par(mar=c(3, 0.5, 1.5, 0.2), mfrow=c(2,1),
    oma = c(4.5, 6, 0.2, 0.2))

smoothingSpline1 = smooth.spline(cc$L2,cc$z, spar=0.5)

plot(smoothingSpline1,
     ylab = "", xlab = "", axes = FALSE, main = NULL,lwd=1.5,type='l', lty=1,col='#377eb8')
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Elevation [m]", side = 2.5, line = 3, cex = 1.5)
mtext("Distance [km]", side = 1, line = 3, cex = 1.5)

smoothingSpline2 = smooth.spline(caucaProfileCorrected$x,caucaProfileCorrected$y, spar=0.35)

lines(smoothingSpline2,col='#4daf4a',lwd=2)

min(caucaProfileCorrected[caucaProfileCorrected$x<250,'y'])

smoothingSpline3 = smooth.spline(cc2$chi,cc2$z, spar=0.5)
plot(smoothingSpline3,col='#4daf4a',type='l',lwd=2,ylab = "", xlab = "", axes = FALSE, main = NULL)
smoothingSpline4 = smooth.spline(cc$chi,cc$z, spar=0.5)
lines(smoothingSpline4,col='#377eb8',lwd=2)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Elevation [m]", side = 2.5, line = 3, cex = 1.5)
mtext("Chi", side = 1, line = 3, cex = 1.5)

dev.off()

