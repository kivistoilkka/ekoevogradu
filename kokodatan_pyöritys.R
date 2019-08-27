### Pylväsplottaajafunktio
## Jokaisella plotilla omat indeksinsä
#pylväsplotti <- function(y_akseli, x_akseli, mukaan){
#  counts <- table(y_akseli, x_akseli)
#  return(barplot(counts, main="Ulostenäytteet",
#          xlab=mukaan, ylab="Näytteiden lkm", col=c("blue","red"),
#          legend = rownames(counts)))
#}

## Plotti ilman indeksiä ja ilman otsikkoa
pylväsplotti <- function(y_akseli, x_akseli, mukaan){
  counts <- table(y_akseli, x_akseli)
  return(barplot(counts,
                 xlab=mukaan, ylab="Näytteiden lkm", col=c("blue","red")))
}


#aseta hakemistopolku sinne missä aineisto on
dir="I:/Gradu/Koko aineisto";
setwd(dir)

#lue graduaineisto
da<-read.csv("suomenLepakot20132016_papanat.csv",sep=";")
#da<-read.csv("suomenLepakot20132016_topsit.csv",sep=";")
#da<-read.csv("suomenLepakot20132016_pöntöt.csv",sep=";")

da$Rengasnumero<-as.factor(da$Rengasnumero)
da$KK<-as.factor(da$KK)
da$Vuosi<-as.factor(da$Vuosi)

for (i in 1:length(da$Korona.positiivinen)) {
  tarkasteltava <- da$Korona.positiivinen[i]
  if (tarkasteltava == "POS") {
    da$CoV[i] <- 1
  } else {
    da$CoV[i] <- 0
  }
}
da$CoV<-as.factor(da$CoV)


#katso aineiston ensimmäisiä rivejä
head(da)

#da <- da[which(da$Eliömaakunta == "N"),]
#da <- da[which(da$Ikä == "ad"),]

#plot(da$Sukupuoli,da$Korona.positiivinen)
#plot(da$Eliömaakunta,da$Korona.positiivinen)
#plot(da$KK,da$Korona.positiivinen)
#plot(da$Vuosi,da$Korona.positiivinen)
#plot(da$Laji,da$Korona.positiivinen)
#plot(da$Paikka,da$Korona.positiivinen)

#plot(da$Laji)
#plot(da$Vuosi)
#plot(da$KK)
#plot(da$Sukupuoli)
#plot(da$Eliömaakunta)

#plot(da$Vuosi,da$Sukupuoli)

## Stacked Bar Plot with Colors and Legend

# Tulokset lajeittain
#counts <- table(da$Korona.positiivinen, da$Laji)
#barplot(counts, main="Ulostenäytteet lajeittain",
#        xlab="Lajit", ylab="Näytteiden lkm", col=c("blue","red"),
#        legend = rownames(counts))
#
#pylväsplotti(da$Korona.positiivinen, da$Laji, "Lajit")

# Tulokset sukupuolittain
#counts <- table(da$Korona.positiivinen, da$Sukupuoli)
#barplot(counts, main="Ulostenäytteet sukupuolittain",
#        xlab="Sukupuolet", ylab="Näytteiden lkm", col=c("blue","red"),
#        legend = rownames(counts))

#pylväsplotti(da$Korona.positiivinen, da$Sukupuoli, "Sukupuolet")

# Tulokset vuosittain
#counts <- table(da$Korona.positiivinen, da$Vuosi)
#barplot(counts, main="Ulostenäytteet vuosittain",
#        xlab="Vuodet", ylab="Näytteiden lkm", col=c("blue","red"),
#        legend = rownames(counts))

#pylväsplotti(da$Korona.positiivinen, da$Vuosi, "Vuodet")

# Tulokset kuukausittain
#counts <- table(da$Korona.positiivinen, da$KK)
#barplot(counts, main="Ulostenäytteet kuukausittain",
#        xlab="Kuukaudet", ylab="Näytteiden lkm", col=c("blue","red"),
#        legend = rownames(counts))

#pylväsplotti(da$Korona.positiivinen, da$KK, "Kuukaudet")

# Tulokset eliömaakunnittain
#counts <- table(da$Korona.positiivinen, da$Eliömaakunta)
#barplot(counts, main="Ulostenäytteet eliömaakunnittain",
#        xlab="Eliömaakunnat", ylab="Näytteiden lkm", col=c("blue","red"),
#        legend = rownames(counts))

#pylväsplotti(da$Korona.positiivinen, da$Eliömaakunta, "Eliömaakunnat")

# Tulokset ikäryhmittäin
#counts <- table(da$Korona.positiivinen, da$Ikä)
#barplot(counts, main="Ulostenäytteet ikäryhmittäin",
#        xlab="Ikäryhmät", ylab="Näytteiden lkm", col=c("blue","red"),
#        legend = rownames(counts))

#pylväsplotti(da$Korona.positiivinen, da$Ikä, "Ikäryhmät")

## Plotit kerralla
layout(matrix(c(1,2,
                3,4,
                5,6), 3, 2, byrow = TRUE))
pylväsplotti(da$Korona.positiivinen, da$Laji, "Lajit")
title("A.", adj=0)
pylväsplotti(da$Korona.positiivinen, da$Eliömaakunta, "Eliömaakunnat")
title("B.", adj=0)
pylväsplotti(da$Korona.positiivinen, da$Vuosi, "Vuodet")
title("C.", adj=0)
pylväsplotti(da$Korona.positiivinen, da$KK, "Kuukaudet")
title("D.", adj=0)
pylväsplotti(da$Korona.positiivinen, da$Sukupuoli, "Sukupuolet")
title("E.", adj=0)
pylväsplotti(da$Korona.positiivinen, da$Ikä, "Ikäryhmät")
title("F.", adj=0)
legend(x="right", legend=c("NEG", "POS"), fill=c("blue","red"))
par(mfrow=c(1,1))


## Taulukko: lepakkolajit, maantieteelliset sijainnit, näytteet kok/pos

da_pos <- da[which(da$Korona.positiivinen == "POS"),]
da_pos

t_posneg_maak <- table(da$Korona.positiivinen, da$Eliömaakunta)
t_laji_maak <- table(da$Laji, da$Eliömaakunta)
t_laji_maak
t_pos_laji_maak <- table(da_pos$Laji, da_pos$Eliömaakunta)
t_pos_laji_maak

#lopullinen_taulukko <- matrix(sprintf("%s/%s", t_pos_laji_maak, t_laji_maak), nrow = 6, ncol = 7, byrow = FALSE)
lopullinen_taulukko <- matrix(sprintf("%s/%s", t_pos_laji_maak, t_laji_maak), nrow = 4, ncol = 2, byrow = FALSE)

rownames(lopullinen_taulukko) <- rownames(t_laji_maak)

colnames(lopullinen_taulukko) <- colnames(t_laji_maak)

#write.table(lopullinen_taulukko, file = "pos_neg_lajit_eliömaakunnat.csv",sep=";")
#write.table(lopullinen_taulukko, file = "pos_neg_lajit_eliömaakunnat_topsit.csv",sep=";")
write.table(lopullinen_taulukko, file = "pos_neg_lajit_eliömaakunnat_pöntöt.csv",sep=";")

########## Selvitetään papana- ja topsinäytteiden päällekäisyydet

#lue graduaineisto
da1<-read.csv("suomenLepakot20132016_papanat.csv",sep=";")
da2<-read.csv("suomenLepakot20132016_topsit.csv",sep=";")
#da<-read.csv("suomenLepakot20132016_pöntöt.csv",sep=";")

da1$Rengasnumero<-as.factor(da1$Rengasnumero)
da1$KK<-as.factor(da1$KK)
da1$Vuosi<-as.factor(da1$Vuosi)

da2$Rengasnumero<-as.factor(da2$Rengasnumero)
da2$KK<-as.factor(da2$KK)
da2$Vuosi<-as.factor(da2$Vuosi)

#katso aineiston ensimmäisiä rivejä
head(da1)
head(da2)

da_papanat <- subset(da1, select = c("Näytenumero", "Rengasnumero", "Laji", "PV", "KK", "Vuosi", "Korona.positiivinen"))
da_topsit <- subset(da2, select = c("Näytenumero", "Rengasnumero", "Laji", "PV", "KK", "Vuosi", "Korona.positiivinen"))

da <- rbind.data.frame(da_papanat, da_topsit)

summary(da)



##### Tilastolliset testit
# ## Lineaarinen malli
# #sovitetaan lineaarinen malli
# m<-lm(Korona.positiivinen~Sukupuoli,data=da)
# 
# #katsotaan mallin ominaisuuksia ja piirretään malli aineistoon
# summary(m)
# abline(m)
# 
# #mallin standardisoidut residuaalit
# nres <- rstandard(m)
# hist(nres)
# 
# #residuaalit vs. mallin ennnusteet
# preds <- fitted.values(m)
# plot(preds,nres)
# abline(a=0,b=0)

## Yleistetty lineaarinen sekamalli
library(lme4)

m<-glmer(CoV~Ikä+KK+Eliömaakunta+Laji(1|Kunta),data=da)
summary(m)
res<-resid(m,type="pearson")
par(mfrow=c(1,2))
boxplot(res~da$plotindex)
preds <- fitted.values(m)
plot(preds,res)
abline(a=0,b=0)
