## Comparison AfSoilGrids250m validation using OFRA samples:
## by Tom.Hengl@isric.org

library(rgdal)
library(plotKML)
library(GSIF)
library(RSAGA)
library(raster)
library(plyr)
library(sp)
library(hexbin)
library(gridExtra)
library(lattice)
library(grDevices)
rm(list=ls(all=TRUE))
load("OFRA.Rdata")

ofra <- read.csv("G:\\soilstorage\\SoilData\\OFRA\\OFRA_RespFuncRev_9-10-15.csv")
str(ofra)

val.PHI <- ofra[,c("Text_ID_Fixed","Latitude","Longitude","Soil.pH")]
val.PHI$Longitude <- as.numeric(paste(val.PHI$Longitude))
sel <- !is.na(val.PHI$Longitude) & val.PHI$Latitude>-90 & val.PHI$Latitude<35 & val.PHI$Longitude > -20 & val.PHI$Longitude < 120 & val.PHI$Soil.pH > 2 & !is.na(val.PHI$Soil.pH)
val.PHI <- val.PHI[sel,]
coordinates(val.PHI) <- ~ Longitude + Latitude
proj4string(val.PHI) <- CRS("+proj=longlat +datum=WGS84")

## plot in Google Earth:
PHI.brks <- c(soil.legends[["PHIHOX"]]$MIN[1], soil.legends[["PHIHOX"]]$MAX)
val.PHI$Value <- cut(val.PHI$Soil.pH*10, PHI.brks)
PHI.pal <- as.character(soil.legends[["PHIHOX"]]$COLOR)
kml(val.PHI, folder.name = "pH (0-20 cm)", subfolder.name = "Observed", shape=shape, colour=Value, colour_scale=PHI.pal, file.name=paste0("OFRA_PHIHOX_20cm.kml"), labels=Value,size=.7, kmz=TRUE)

## Overlay / compare pH:
val.PHI.xy <- spTransform(val.PHI, CRS("+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +units=m +ellps=WGS84 +datum=WGS84"))
ov <- extract.list(path="H:\\AFSIS\\zipped", y=c("af_PHIHOX_T__M_sd1_250m.tif", "af_PHIHOX_T__M_sd2_250m.tif"), x=val.PHI.xy, ID="Text_ID_Fixed")
ov$Soil.pH.soilgrids <- ((0.05*ov$af_PHIHOX_T__M_sd1_250m.tif + 0.1*ov$af_PHIHOX_T__M_sd1_250m.tif)/.15)/10
ov$Soil.pH <- val.PHI$Soil.pH
## RMSE:
sqrt(sum((ov$Soil.pH.soilgrids-ov$Soil.pH)^2, na.rm=TRUE)/nrow(ov))
## 0.70

## plot the differences:
pfun <- function(x,y, ...){ 
         panel.hexbinplot(x,y, ...)  
         panel.abline(0,1,lty=1,lw=2,col="black") 
}
plt.OFRA <- hexbinplot(ov$Soil.pH.soilgrids~ov$Soil.pH, colramp=colorRampPalette(R_pal[["bpy_colors"]]), main="Soil pH (2197 points)", xlab="measured (OFRA)", ylab="predicted (AfSoilGrids250m)", type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8, xlim=c(3,11), ylim=c(3,11), asp=1, xbins=25, density=40, panel=pfun) ## range(ov$Soil.pH)
plot(plt.OFRA)
save.image("OFRA.Rdata")
