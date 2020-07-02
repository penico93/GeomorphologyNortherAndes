#Figure River Profiles


setwd("/Users/penico93/Google Drive/Doctoral_Research_Exploratory_Ideas/Geomorphology of Cauca Patia Basins/R_plots")



data2 = read.table("riverdata_200m.txt", header = TRUE, sep= '\t')

write.table(data2, 'data2.txt', sep = '\t')

#Mac

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

