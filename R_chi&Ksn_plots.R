#Plots river profiles ksn and other variables to find knickpoints
# Graph of some statistics like river concavity

#Windows
#setwd("C:/Users/penico93/Documents/Documentos Doctorado/Geomorphology of Cauca Patia Basins/R_plots")


#Mac
setwd("/Users/penico93/Google Drive/Doctoral_Research_Exploratory_Ideas/Geomorphology of Cauca Patia Basins/R_plots")
library(ggplot2)
library(grid)
library(gridExtra)


data = read.table("riverdata.txt", header = TRUE, sep= '\t')

## Pat?a river

# id_profile = 1201

cc <- data[data$id_profile==1,]

write.csv(cc, file = "dataCaucaProfile.csv")

plot(cc$L,cc$z)

ggplot(cc,aes(x=L,y=z)) + geom_point()  + theme_bw() +theme() + labs(title="Pat?a River elevation - profile ")

ggplot(cc,aes(x=-chi,y=z)) + geom_point()  + theme_bw() +theme() + labs(title="Pat?a River \u03C7 - profile")

ggplot(cc,aes(x=area,y=slope)) + geom_point()  +scale_x_log10() + scale_y_log10() + theme_bw() +theme() + labs(title="Pat?a River Slope-Area")



##

## Cauca river

# id_profile = 1

cauca <- data[data$id_profile==c(1,13,175,23,268,142,100,194,114,240,182,298,144,427,85),]


ggplot(cauca,aes(x=L,y=z)) + geom_point()  + theme_bw() +theme() + labs(title="Cauca River elevation - profile ")

ggplot(cauca,aes(x=-chi,y=z)) + geom_point()  + theme_bw() +theme() + labs(title="Cauca River \u03C7 - profile")

ggplot(cauca,aes(x=area,y=slope)) + geom_point()  +scale_x_log10() + scale_y_log10() + theme_bw() +theme()


#plot chi profiles vs. elevation of both basins

ggplot(cc,aes(x=chi,y=z)) + geom_point() + geom_point(data = cauca) + theme_bw() +theme() + labs(title="Pat?a River \u03C7 - profile")




#Both plots in same layout


#
ggplot(cc,aes(x=L,y=z)) + geom_point()  + theme_bw() +
  labs(title="Pat?a and Cauca - river elevation profiles ") + 
  geom_point(data = cauca, colour = 'red')+ 
  annotate("text", x = 2e+05, y = 1100, label = "Cauca river") + 
  annotate("text", x = 2.7e+05, y = 400, label = "Pat?a river")

###################


#PLOT with data2 file...



data2 = read.table("riverdata_200m.txt", header = TRUE, sep= '\t')


## Cauca river

# id_profile = 1


cauca <- data2[data2$id_profile==1,]

plot(cauca$L,cauca$z,type='p')
#identify(cauca$L,cauca$z) #### to click and get the point value


ggplot(cauca,aes(x=L,y=z)) + geom_point()  + theme_bw() +theme() + labs(title="Cauca River elevation - profile ")

ggplot(cauca,aes(x=-chi,y=z)) + geom_point()  + theme_bw() +theme() + labs(title="Cauca River \u03C7 - profile")

ggplot(cauca,aes(x=area,y=slope)) + geom_point()  +scale_x_log10() + scale_y_log10() + theme_bw() +theme()



## Pat?a river

# id_profile = 1200

cc <- data2[data2$id_profile==1201,]

plot(cc$L,cc$z,type='p')

ggplot(cc,aes(x=L,y=z)) + geom_point() +theme_bw() +theme() + labs(title="Pat?a River elevation - profile ")

ggplot(cc,aes(x=-chi,y=z)) + geom_point()  + theme_bw() +theme() + labs(title="Pat?a River \u03C7 - profile")

ggplot(cc,aes(x=area,y=slope)) + geom_point()  +scale_x_log10() + scale_y_log10() + theme_bw() +theme() + labs(title="Pat?a River Slope-Area")


plot(cc$L,cc$area,type='p')



# Make plot for presentation

cc$L <- cc$L/1000
cauca$L <- cauca$L/1000

layout(matrix(c(1,2), 2, 1, byrow = TRUE))
par(mar=c(4, 5, 1.5, 2.7))
plot(cc$L,cc$z,type='l',lwd = 2,xlim=c(0, 250), ylim = c(100, 1500),xlab="Distance [Km]", ylab="Elevation [m]", main = "River profile - Patia")
plot(cauca$L,cauca$z,type='l',lwd = 2,xlim=c(0, 750), ylim = c(100, 1500),xlab="Distance [Km]", ylab="Elevation [m]", main = "River profile - Cauca")


#Plot certain rivers 
#or all the rivers for the Cauca drainage basin

#Rivers draining the Plateau surface



# 85 is rio san andres in Antioquia
# 126 is rio Espiritu Santo
# 210 Quebrada santa maria
# 298 Quebrada Juan Garcia
# 182 Rio Aurra
# 457 Rio Amagá
# 194 Rio Buey
# 48 Rio Arma
# 100 Rio Pozo
# 3,23,16,13

#Rivers draining the western cordillera into the Cauca river
# 427 Quebrada del infierno
# 144 Rio ituango
# 327 Quebrada Peque
# 240 Rio Tonusco
# 114 Rio San Juan

riverID <- c(85,126,210,298,182,457,485,194,48,100,427,144,327,240,114)
colorID <- c(rep("darkblue",10),rep("green",5))


setwd('/Users/penico93/Documents/Papers in prep/Paper Geomorphology/Methods-analysis/')


filename1 <- "Chi_CaucaRiverCanyon.eps"
setEPS()
# 
postscript(filename1,width=(8),height=(5))

par(mar=c(5, 5, 1.5, 0.2))
#c(bottom, left, top, right))


cauca <- data2[data2$id_profile==1,]
plot(cauca$chi,cauca$z, ylab = "", xlab = "", axes = FALSE, main = NULL, xlim = c(0,1500), ylim = c(0,4000), type = 'l',col = 'lightblue',lwd = 2)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Elevation [m]", side = 2.5, line = 3, cex = 1.5)
mtext("Chi", side = 1, line = 3, cex = 1.5)
for(i in 1:length(riverID)){
  cauca <- data2[data2$id_profile==riverID[i],]
  lines(cauca$chi,cauca$z,col= colorID[i],lty = 1)
}

dev.off()





library(raster)
library(rgdal)
library(sp)


river_shape <- readOGR("/Users/penico93/Google Drive/Doctoral_Research_Exploratory_Ideas/Projected/output_shapefile_riversCauca and Patia.shp")

plot(river_shape)

head(river_shape@data)

river_shape$id_profile


river_shape_subset <- river_shape[river_shape$id_profile %in% c(85,126,210,298,182,457,485,194,48,100),]
river_shape_subset2 <- river_shape[river_shape$id_profile %in% c(427,144,327,240,114),]
river_shape_subset3 <- river_shape[river_shape$id_profile %in% c(1),]


plot(river_shape_subset)
plot(river_shape_subset2)



writeOGR(river_shape_subset, "tributariesCC.shp", "river_shape_subset", driver = "ESRI Shapefile")

writeOGR(river_shape_subset2, "tributariesWC.shp", "river_shape_subset", driver = "ESRI Shapefile")

writeOGR(river_shape_subset3, "CaucaOnly.shp", "river_shape_subset", driver = "ESRI Shapefile")


