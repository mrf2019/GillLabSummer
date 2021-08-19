setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("Invertebrate abundance.csv")

dim(data)
head(data)
colnames(data)

boxplot(data$Abundance~data$U_L, xlab = "Site", ylab = "# Individuals",
        col=c("red", "blue"))

#################Rank abundance of order
LowerAB<-data$Abundance[1:24]
UpperAB<-data$Abundance[25:48]

LowerAB_sort<-sort(LowerAB, decreasing=T)
UpperAB_sort<-sort(UpperAB, decreasing=T)
LowerAB_sort_sum<-cumsum(LowerAB_sort)
UpperAB_sort_sum<-cumsum(UpperAB_sort)
xs<-1:24
plot(xs, LowerAB_sort_sum, col='red', ylim=c(0,100), xaxt='n', ylab = "Proportional Abundance",
     type="b", pch=16, xlab = "")
lines(xs, LowerAB_sort_sum, col='red', ylim=c(0,100), xaxt='n', ylab = "Proportional Abundance",
    main = "Rank Abundance of Order", type="b", pch=16)
lines(xs, UpperAB_sort_sum, col='blue', add=T, type="b", pch=16)
legend("topleft", c("Lower Site", "Upper Site"), col=c("red", "blue"), 
       pch=c(16,20), bty="n")

#####Relative abundance of species
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")

#Earthworm
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
EarthwormRelAb<-data["Lumbricidae",]/colSums(data, na.rm=T)
EarthwormRelAb<-as.matrix(EarthwormRelAb)
EarthwormRelAb[is.nan(EarthwormRelAb)]<-NA
EarthwormRelAb<-t(as.vector(EarthwormRelAb))

#Collembola
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
CollembolaRelAb<-data["Collembola",]/colSums(data, na.rm=T)
CollembolaRelAb<-as.matrix(CollembolaRelAb)
CollembolaRelAb[is.nan(CollembolaRelAb)]<-NA
CollembolaRelAb<-t(as.vector(CollembolaRelAb))

#Diptera
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
DipteraRelAb<-data["Diptera",]/colSums(data, na.rm=T)
DipteraRelAb<-as.matrix(DipteraRelAb)
DipteraRelAb[is.nan(DipteraRelAb)]<-NA
DipteraRelAb<-t(as.vector(DipteraRelAb))

#Nematoda
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
NematodaRelAb<-data["Nematoda",]/colSums(data, na.rm=T)
NematodaRelAb<-as.matrix(NematodaRelAb)
NematodaRelAb[is.nan(NematodaRelAb)]<-NA
NematodaRelAb<-t(as.vector(NematodaRelAb))

#Opiliones
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
OpilionesRelAb<-data["Opiliones",]/colSums(data, na.rm=T)
OpilionesRelAb<-as.matrix(OpilionesRelAb)
OpilionesRelAb[is.nan(OpilionesRelAb)]<-NA
OpilionesRelAb<-t(as.vector(OpilionesRelAb))

#Polyxenida
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
PolyxenidaRelAb<-data["Polyxenida",]/colSums(data, na.rm=T)
PolyxenidaRelAb<-as.matrix(PolyxenidaRelAb)
PolyxenidaRelAb[is.nan(PolyxenidaRelAb)]<-NA
PolyxenidaRelAb<-t(as.vector(PolyxenidaRelAb))

#Polyzoniida
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
PolyzoniidaRelAb<-data["Polyzoniida",]/colSums(data, na.rm=T)
PolyzoniidaRelAb<-as.matrix(PolyzoniidaRelAb)
PolyzoniidaRelAb[is.nan(PolyzoniidaRelAb)]<-NA
PolyzoniidaRelAb<-t(as.vector(PolyzoniidaRelAb))

#Formicidae
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
FormicidaeRelAb<-data["Formicidae",]/colSums(data, na.rm=T)
FormicidaeRelAb<-as.matrix(FormicidaeRelAb)
FormicidaeRelAb[is.nan(FormicidaeRelAb)]<-NA
FormicidaeRelAb<-t(as.vector(FormicidaeRelAb))

#Acari
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
AcariRelAb<-data["Acari",]/colSums(data, na.rm=T)
AcariRelAb<-as.matrix(AcariRelAb)
AcariRelAb[is.nan(AcariRelAb)]<-NA
AcariRelAb<-t(as.vector(AcariRelAb))

#Chordeumida
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
ChordeumidaRelAb<-data["Chordeumida",]/colSums(data, na.rm=T)
ChordeumidaRelAb<-as.matrix(ChordeumidaRelAb)
ChordeumidaRelAb[is.nan(ChordeumidaRelAb)]<-NA
ChordeumidaRelAb<-t(as.vector(ChordeumidaRelAb))

#Hymenoptera
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
HymenopteraRelAb<-data["Hymenoptera",]/colSums(data, na.rm=T)
HymenopteraRelAb<-as.matrix(HymenopteraRelAb)
HymenopteraRelAb[is.nan(HymenopteraRelAb)]<-NA
HymenopteraRelAb<-t(as.vector(HymenopteraRelAb))

#Neuroptera
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
NeuropteraRelAb<-data["Neuroptera",]/colSums(data, na.rm=T)
NeuropteraRelAb<-as.matrix(NeuropteraRelAb)
NeuropteraRelAb[is.nan(NeuropteraRelAb)]<-NA
NeuropteraRelAb<-t(as.vector(NeuropteraRelAb))

#Gastropoda
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
GastropodaRelAb<-data["Gastropoda",]/colSums(data, na.rm=T)
GastropodaRelAb<-as.matrix(GastropodaRelAb)
GastropodaRelAb[is.nan(GastropodaRelAb)]<-NA
GastropodaRelAb<-t(as.vector(GastropodaRelAb))

#Diplura
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
DipluraRelAb<-data["Diplura",]/colSums(data, na.rm=T)
DipluraRelAb<-as.matrix(DipluraRelAb)
DipluraRelAb[is.nan(DipluraRelAb)]<-NA
DipluraRelAb<-t(as.vector(DipluraRelAb))

#Enchytraeidae
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
EnchytraeidaeRelAb<-data["Enchytraeidae",]/colSums(data, na.rm=T)
EnchytraeidaeRelAb<-as.matrix(EnchytraeidaeRelAb)
EnchytraeidaeRelAb[is.nan(EnchytraeidaeRelAb)]<-NA
EnchytraeidaeRelAb<-t(as.vector(EnchytraeidaeRelAb))

#Geophilomorpha
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
GeophilomorphaRelAb<-data["Geophilomorpha",]/colSums(data, na.rm=T)
GeophilomorphaRelAb<-as.matrix(GeophilomorphaRelAb)
GeophilomorphaRelAb[is.nan(GeophilomorphaRelAb)]<-NA
GeophilomorphaRelAb<-t(as.vector(GeophilomorphaRelAb))

#Symphyla
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
SymphylaRelAb<-data["Symphyla",]/colSums(data, na.rm=T)
SymphylaRelAb<-as.matrix(SymphylaRelAb)
SymphylaRelAb[is.nan(SymphylaRelAb)]<-NA
SymphylaRelAb<-t(as.vector(SymphylaRelAb))

#Araneida
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
AraneidaRelAb<-data["Araneida",]/colSums(data, na.rm=T)
AraneidaRelAb<-as.matrix(AraneidaRelAb)
AraneidaRelAb[is.nan(AraneidaRelAb)]<-NA
AraneidaRelAb<-t(as.vector(AraneidaRelAb))

AllRelAb<-cbind(AraneidaRelAb,CollembolaRelAb,DipteraRelAb,EarthwormRelAb,NematodaRelAb,
                OpilionesRelAb,PolyxenidaRelAb,PolyzoniidaRelAb,FormicidaeRelAb,AcariRelAb,
                ChordeumidaRelAb,HymenopteraRelAb,NeuropteraRelAb,GastropodaRelAb,
                DipluraRelAb,EnchytraeidaeRelAb,GeophilomorphaRelAb,SymphylaRelAb)
head(AllRelAb)



#############Rel Ab by Order
install.packages("ggplot")
library(ggplot)
head(data)
dim(data)
data<-data[1:20,]
UpperData<-data[,25:48]
LowerData<-data[,1:24]
head(UpperData)
UpperData$TotalIndividuals<-rowSums(UpperData, na.rm=T)
LowerData$TotalIndividuals<-rowSums(LowerData, na.rm=T)
Upper_PopulationSize<-sum(UpperData$TotalIndividuals)
Lower_PopulationSize<-sum(LowerData$TotalIndividuals)

Upper_Spp_RelAb<-UpperData$TotalIndividuals/Upper_PopulationSize
Lower_Spp_RelAb<-LowerData$TotalIndividuals/Lower_PopulationSize

RelAb<-rbind(Upper_Spp_RelAb, Lower_Spp_RelAb)
colnames(RelAb)<-c("Araneida", "Coleoptera", "Collembola", "Diptera", "Lumbricidae", "Nematoda",
                   "Opiliones", "Polyxenida", "Polyzoniida", "Formicidae", "Acari",
                   "Chordeumida", "Hymenoptera", "Julida", "Neuroptera", "Gastropoda",
                   "Diplura", "Enchytraeidae", "Geophilomorpha", "Symphyla")


LowerRelABSPP_sort<-sort(RelAb['Lower_Spp_RelAb',], decreasing=T)
UpperRelABSPP_sort<-sort(RelAb['Upper_Spp_RelAb',], decreasing=T)

######################## AG individual curves with labels
color_range1 <-colorRampPalette(c("#FF0000", "#FBD5D5"))
my_red<-color_range1(24)

plot(LowerRelABSPP_sort, col= my_red, ylab="Relative Abundance",
     main="Relative Abundance by Order", xaxt="n", xlab="", mar=c(1,4,5,1))
position.text<-1:20
axis(1, lty=2, at = position.text, labels = names(LowerRelABSPP_sort), las=2, tick = F, cex.axis=0.75) 

color_range2 <-colorRampPalette(c("#0110FA", "#A9DBFA"))
my_blue<-color_range2(24)
plot(UpperRelABSPP_sort, col= my_blue, ylab="Relative Abundance",
     main="Relative Abundance by Order", xaxt="n", xlab="", mar=c(1,4,5,1))
position.text<-1:20
axis(1, lty=2, at = position.text, labels = names(UpperRelABSPP_sort), las=2, tick = F, cex.axis=0.75) 

######################################


color_range1 <-colorRampPalette(c("#FF0000", "#FBD5D5"))
my_red<-color_range1(24)
plot(LowerRelABSPP_sort, col= my_red, ylab="Relative Abundance",
     main="Relative Abundance by Order", xaxt="n", xlab="", mar=c(1,4,5,1))
position.text<-1:19
names(LowerRelABSPP_sort)
axis(1, lty=2, at = position.text, labels = c("Lumbricidae", "Collembola", "Diptera", "Nematoda",
                                              "Opiliones", "Polyxenida", "Polyzoniida", "Formicidae", "Acari",
                                              "Chordeumida", "Hymenoptera", "Julida", "Neuroptera", "Gastropoda",
                                              "Diplura", "Enchytraeidae", "Geophilomorpha", "Symphyla", "Araneida"), las=2, tick = F, cex.axis=0.75) 
color_range2 <-colorRampPalette(c("#0110FA", "#A9DBFA"))
my_blue<-color_range2(24)
points(UpperRelABSPP_sort,col=my_blue)

########################### Rel ab of orders by plot
dim(AllRelAb)
library(colorspace)

names<-colnames(data)
colnames(AllRelAb)<-names
LowerRelAB<-AllRelAb[,1:24]
UpperRelAB<-AllRelAb[,25:48]

par(mfrow=c(2,1), mar=c(1,4,3,1))
plot(sort(LowerRelAB[1,], decreasing=T), xaxt="n", col="#E74C3C", 
     ylab = "Relative Abundance", main = "Relative Abundance of Orders per Site")
points(sort(LowerRelAB[2,], decreasing=T), col="#F1948A")
points(sort(LowerRelAB[3,], decreasing=T), col="#D35400")
points(sort(LowerRelAB[4,], decreasing=T), col="#E59866")
points(sort(LowerRelAB[5,], decreasing=T), col="#E67E22")
points(sort(LowerRelAB[6,], decreasing=T), col="#F0B27A")
points(sort(LowerRelAB[7,], decreasing=T), col="#F39C12")
points(sort(LowerRelAB[8,], decreasing=T), col="#F8C471")
points(sort(LowerRelAB[9,], decreasing=T), col="#F1C40F")
points(sort(LowerRelAB[10,], decreasing=T), col="#F7DC6F")
points(sort(LowerRelAB[11,], decreasing=T), col="#2ECC71")
points(sort(LowerRelAB[12,], decreasing=T), col="#27AE60")
points(sort(LowerRelAB[13,], decreasing=T), col="#16A085")
points(sort(LowerRelAB[14,], decreasing=T), col="#1ABC9C")
points(sort(LowerRelAB[15,], decreasing=T), col="#3498DB")
points(sort(LowerRelAB[16,], decreasing=T), col="#85C1E9")
points(sort(LowerRelAB[17,], decreasing=T), col="#2980B9")
points(sort(LowerRelAB[18,], decreasing=T), col="#7FB3D5")


par(mar=c(1,4,1,1))
plot(sort(UpperRelAB[1,], decreasing=T), xaxt="n", 
     ylab = "Relative Abundance", xlab="Plots 1-24", col="#E74C3C")
points(sort(UpperRelAB[2,], decreasing=T), col="#F1948A")
points(sort(UpperRelAB[3,], decreasing=T), col="#D35400")
points(sort(UpperRelAB[4,], decreasing=T), col="#E59866")
points(sort(UpperRelAB[5,], decreasing=T), col="#E67E22")
points(sort(UpperRelAB[6,], decreasing=T), col="#F0B27A")
points(sort(UpperRelAB[7,], decreasing=T), col="#F39C12")
points(sort(UpperRelAB[8,], decreasing=T), col="#F8C471")
points(sort(UpperRelAB[9,], decreasing=T), col="#F1C40F")
points(sort(UpperRelAB[10,], decreasing=T), col="#F7DC6F")
points(sort(UpperRelAB[11,], decreasing=T), col="#2ECC71")
points(sort(UpperRelAB[12,], decreasing=T), col="#27AE60")
points(sort(UpperRelAB[13,], decreasing=T), col="#16A085")
points(sort(UpperRelAB[14,], decreasing=T), col="#1ABC9C")
points(sort(UpperRelAB[15,], decreasing=T), col="#3498DB")
points(sort(UpperRelAB[16,], decreasing=T), col="#85C1E9")
points(sort(UpperRelAB[17,], decreasing=T), col="#2980B9")
points(sort(UpperRelAB[18,], decreasing=T), col="#7FB3D5")







