"
getwd()
setwd('d:\\Users\\geo21071\\Downloads\\data')
"
library(sf)
library(terra)
library(dplyr)
library(tmap)
library(ggpubr)

st_layers("Exported2/Vectors.gpkg")

acto = st_read("Exported2/Vectors.gpkg", layer="acto", quiet = T )
oik = st_read("Exported2/Vectors.gpkg", layer="oik", quiet = T )
nuclear = st_read("Exported2/Vectors.gpkg", layer="nuclear", quiet = T )
#stations = st_read("Exported2/Vectors.gpkg", layer="stations", quiet = T )
enotites= st_read("Exported2/Vectors.gpkg", layer="enotites", quiet = T )

corine = rast("Exported2/Corine.tif")
dem = rast("Exported2/dem.tif") # Δεν θα χρησημοποιηθεί
ActualPollution = rast("Exported2/ActualPollution.tif")

par(mar=c(1,1,1,3) )
mycol = c("grey30","orange","green","purple","blue")
plot(corine, legend = T, col = mycol)
#legend(x='bottomleft', xpd=T, inset=c(0.98, 0.40),
#       legend=levels(corine)[[1]]$value,
#       fill = mycol,bg =NULL, box.lwd = 0)
plot(st_geometry(acto), add=T, border="grey40")

par(mar=c(1,1,1,1) )
plot(st_geometry(oik), main="Towns", pch=19, cex=0.6, col="red")
plot(st_geometry(acto), add=T, border="grey40")
plot(st_geometry(nuclear), add=T, pch=23, col="black", bg="black"  )
legend(x='bottomleft', pch=c(19,23), legend=c("Towns","Plant"),
       col=c("red","black"), pt.bg="black", bg=NULL, box.lwd=0)
box()

plot((ActualPollution), main="Actual Pollution")
plot(st_geometry(acto), add=T, border="grey40")
#plot(st_geometry(stations), add=T, pch=6, col="blue")
plot(st_geometry(nuclear), add=T, pch=23, col="black", bg="black")

legend(x='bottomleft', xpd=T, inset=c(0.10, 0.03),
       pch=c(6,23),
       legend=c("Nuclear Plant"),
       col = c("black"), pt.bg="black",
       bg =NULL, box.lwd = 0)

TheThreshold=7500
plot(st_geometry(enotites), main="Administrative sub-units", border="grey40" )
plot(st_geometry(acto), add=T  )
text(st_coordinates(st_centroid( enotites)), label=enotites$kalcode,  cex=0.4,col="blue" ) #
box()

oik$ActualPollution = terra::extract(ActualPollution, oik, ID=F)[[1]]
oik2 = subset(oik, ActualPollution >= TheThreshold)
dim(oik2)

sum(oik2$pop2011)

par(mar=c(1,1,2,1) )
plot(st_geometry(acto), main=sprintf("Οικισμοί σε επικύνδινη ζώνη\n>= %s ng",TheThreshold) )
plot(st_geometry(oik2), main="Towns", pch=19, cex=0.6, col="red", add=T)
plot(st_geometry(nuclear), add=T, pch=23, bg="black", col="black")
legend(x='bottomleft', pch=c(19,23), legend=c("Οικισμοι","Εργοστάσιο"),
       col=c("red","black"), pt.bg="black", bg=NULL, box.lwd=0)
box()

par(mar=c(0,0,0,0) )
terra::plot(ActualPollution, axes=F, box=F, main="Οικισμοί σε επικύνδινη ζώνη", plg=list(loc="bottom") )
plot(st_geometry(acto), main=sprintf("Οικισμοί σε επικύνδινη ζώνη\n>= %s ng",TheThreshold), add=T)
plot(st_geometry(oik),   pch=19, cex=0.5, col=rgb(red=0.1,green=0,blue=0,alpha=0.1), add=T)
plot(st_geometry(oik2),  pch=19, cex=0.6, col="red", add=T)
plot(st_geometry(nuclear), add=T, pch=23, bg="black", col="black")
legend(xpd=T, x='bottomleft', pch=c(19,23), legend=c("Οικισμοι","Εργοστάσιο"),  col=c("red","black"), pt.bg="black", bg=NULL, box.lwd=0)
box()

corine

levels(corine)[[1]]

#pol = rasterToPolygons(corine, fun=function(x){x==2})
table(corine[])

cellSize = cellSize(corine[[1]])[][[1]] / 1000000
ola = sum(table(corine[]))
agr = sum((corine==2)[], na.rm=T)
emvado_agrotika = round((agr*cellSize) ,1)
emvado_agrotika

pososto = 100*(agr/ola)
pososto

res(corine==2) %>% prod() # Εναλακτικός τρόπος υπολογισμού του cell size (m)

plot( (corine==2), main=sprintf("Αγροτικές περιοχές (%s%% των κελιών)\n(%s χλμ^2)",
                                round(pososto,1), emvado_agrotika ))
plot(st_geometry(acto), add=T )

bigPolution = ( ActualPollution >= TheThreshold )
#bigPolution = terra::crop(  bigPolution, vect(acto) ) %>% mask(vect(acto))
#bigPolution = classify(bigPolution, cbind(c(TRUE, FALSE), c(1,NA))  )

table(bigPolution[])

emvado_bigPollution = res(bigPolution) %>% prod()
(emvado_bigPollution)


plot(bigPolution, main=sprintf("Μεγάλη μόλυνση (%s χλμ^2)", round(emvado_bigPollution ,1) ), legend=F, col=c(NA,"orange"))
plot(st_geometry(acto), add=T)
plot(st_geometry(nuclear), add=T, pch=23, bg="black", col="black")
legend(x='bottomleft', pch=c(19,23), legend=c("Big Pollution","Plant"),
       col=c("green4","black"), pt.bg="black", bg=NULL, box.lwd=0)

par(mfrow = c(2, 1))
plot(corine==2, main="CORINE (Αγροτικά)", col=c(NA,"orange"))
plot(acto, add=T, col=NA)

plot(bigPolution, main="Μεγάλη μόλυνση", col=c(NA,"green"))
plot(acto, add=T, col=NA)

res(corine)
res(bigPolution)


r2 = resample((corine==2), bigPolution, method='near')
plot(r2, main="Αγροτικά + Mόλυνση", col=c(NA,rgb(1,0.64,0,0.7)), legend=F )
plot(bigPolution,  col=c(NA,rgb(0,1,0,0.5)), add=T, legend=F)
plot(st_geometry( acto), add=T, col=NA)

table((corine==2)[])
table(r2[])
table(bigPolution[])
result = (r2 + (bigPolution))==2
plot(result , main="Αποτέλεσμα: Αγροτικά (CORINE)+\n μεγάλη μολυνση")
table(result[])
ncells = sum((result[]==1), na.rm=T)
ncells
emvado_cell = res(result) %>% prod()
emvado_cell

result_emvado = (ncells * emvado_cell)/1000
result_emvado


enotites$meanPolution = raster::extract(ActualPollution, enotites, fun=mean )$layer
enotites$area = st_area(enotites)
enotites$danger = ifelse(enotites$meanPolution>=TheThreshold, TRUE, FALSE)


ggbarplot(enotites %>% as_tibble(), x = "lektiko", y = "meanPolution",
          fill = "danger",
          color = "danger",
          sort.val = "asc",          # Sort the value in dscending order
          rotate = T,
          x.text.angle = 90,           # Rotate vertically x axis texts
          ggtheme = theme_bw(),
          palette = "Set2", legend = "bottom"
)

qtm(enotites, fill = "meanPolution" )+ tm_layout(legend.position = c("right", "top")  )
qtm(enotites, fill = "danger" )+ tm_layout(legend.position = c("right", "top")  )


