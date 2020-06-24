# Basin variables comparison







plot( data2_subset1.5$FocalSt50c,data2_subset1.5$TRMMproj,
      xlim=c(0,80),ylim = c(0,7),
      ylab = "", xlab = "", axes = FALSE, main = NULL,lty=1,bg=data2_subset1.5$Color,cex=1,pch=data2_subset1.5$Pch)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("TRMM mean annual precipitation [m]", side = 2.5, line = 3, cex = 1.2)
mtext("Hillslope gradient % rise/run [m/m]", side = 1, line = 3, cex = 1.2)
arrows(data2_subset1.5$FocalSt50c, data2_subset1.5$TRMMproj+(data2_subset1.5$extract_trmm_sd/1000), 
       data2_subset1.5$FocalSt50c, data2_subset1.5$TRMMproj-(data2_subset1.5$extract_trmm_sd/1000), length=0, angle=90,col='gray')
arrows(data2_subset1.5$FocalSt50c+data2_subset1.5$extract_hillslope_sd, data2_subset1.5$TRMMproj, 
       data2_subset1.5$FocalSt50c-data2_subset1.5$extract_hillslope_sd, data2_subset1.5$TRMMproj, length=0, angle=90,col='gray')


points(data2_subset1.5$FocalSt50c,data2_subset1.5$TRMMproj,
       bg=data2_subset1.5$Color,pch=data2_subset1.5$Pch)
