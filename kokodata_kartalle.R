## Aktivoidaan tarvittavat paketit
library(sp)
library(maptools)
library(maps)

# Asetetaan hakemistopolku sinne, missä aineisto on
dir="I:/Gradu/Koko aineisto";
setwd(dir)

# Luetaan graduaineisto
da_papanat <- read.csv("suomenLepakot20132016_papanat.csv",sep=";")
da_topsit <- read.csv("suomenLepakot20132016_topsit.csv",sep=";")
da_pontot <- read.csv("suomenLepakot20132016_pöntöt.csv",sep=";")

da_papanat$Tyyppi <- "papana"
da_topsit$Tyyppi <- "topsi"
da_pontot$Tyyppi <- "pönttö"

da_papanat <- subset(da_papanat, select = c("Laji", "Latitude..ETRS.TM35FIN.", "Longitude..ETRS.TM35FIN.", "Korona.positiivinen", "Tyyppi"))
da_topsit <- subset(da_topsit, select = c("Laji", "Latitude..ETRS.TM35FIN.", "Longitude..ETRS.TM35FIN.", "Korona.positiivinen", "Tyyppi"))
da_pontot <- subset(da_pontot, select = c("Laji", "Latitude..ETRS.TM35FIN.", "Longitude..ETRS.TM35FIN.", "Korona.positiivinen", "Tyyppi"))


locs <- rbind.data.frame(da_papanat, da_topsit, da_pontot)
head(locs)

# Määritetään koordinaatisto
coordinates(locs) <- c("Longitude..ETRS.TM35FIN.", "Latitude..ETRS.TM35FIN.")  # set spatial coordinates
plot(locs)

crs.geo <- CRS("+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs")  # geographical, datum WGS84

proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)

plot(locs, pch = 20, cex = 1, col = "red")

# Vaihdetaan koordinaatisto
crs.wgs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")
locs.wgs <- spTransform(locs, crs.wgs)

plot(locs.wgs, pch = 20, cex = 1, col = "black")

plot(subset(locs.wgs, locs.wgs$Korona.positiivinen == "NEG"), pch = 1, cex = 2, lwd = 2, col = "steelblue")
plot(subset(locs.wgs, locs.wgs$Korona.positiivinen == "POS"), pch = 3, cex = 2, lwd = 2, col = "red", add = T)

### VALMIS KARTTA
#Luetaan Suomen vesistötiedot
vpd_jarvet <- readShapeSpatial(file.choose())
proj4string(vpd_jarvet) <- crs.geo  # define projection system of our data
vpd_suuret_jarvet <- subset(vpd_jarvet, vpd_jarvet$Shape_area > 17000000)
vpd_suuret_jarvet.wgs <- spTransform(vpd_suuret_jarvet, crs.wgs)

# Tehdään pohjakartta
map("world", "Sweden", xlim=c(17,32), ylim=c(58.7,63.63), col="gray80", fill=TRUE)
map("world", "Estonia", col="gray80", fill=TRUE, add=TRUE)
map("world", "Russia", col="gray80", fill=TRUE, add=TRUE)
map("lakes", col="white", fill=TRUE, add=TRUE)
map("world", "Finland", col="gray95", fill=TRUE, add=TRUE)
map(vpd_suuret_jarvet.wgs, fill=TRUE, col="gray80", border=FALSE, add=TRUE)
map("world", "Finland", add=TRUE)

# Lisätään näytepisteet
points(subset(locs.wgs, locs.wgs$Tyyppi == "papana"), pch = 3, cex = 1, lwd = 1)
points(subset(locs.wgs, locs.wgs$Tyyppi == "topsi"), pch = 1, cex = 1, lwd = 1)
points(subset(locs.wgs, locs.wgs$Tyyppi == "pönttö"), pch = 2, cex = 1, lwd = 1)

# Lisätään mittakaava ja pohjoisnuoli
map.scale(x=26.3, y=59.1, ratio=FALSE, relwidth=0.2)
library(GISTools)
north.arrow(xb=31.5, yb=59, len=0.1, lab="N", col="black")
detach("package:GISTools", unload=T)

# Lisätään nimi ja laatikoidaan kartta
title(sub = "Kaikki näytteet 2013-2016, ristit papanoita, ympyrät topseja, kolmiot pönttöjä")
box()
#rect(xleft=23.9, ybottom=59.9, xright=25.4, ytop=60.5, lwd=2)

kartta <- recordPlot()

pdf("näytekartta_kaikki.pdf", width = 11, height = 8 )
kartta
dev.off()


## INDEKSIKARTTA
map("world", xlim=c(-11,39), ylim=c(49,71.1), col="gray80", fill=TRUE)

# piirrä laatikko haluttuihin koordinaatteihin
map("lakes", col="white", fill=TRUE, add=TRUE)
map("world", "Finland", col="gray95", fill=TRUE, add=TRUE)
rect(xleft=17, ybottom=58.7, xright=32, ytop=63.63, lwd=2)
box()

indeksikartta <- recordPlot()

pdf("indeksikartta.pdf", width = 11, height = 8 )
indeksikartta
dev.off()
