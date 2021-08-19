setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("Agrestis.csv")
head(data)

library("Matrix")
library("Hmisc")

###########
par(mar=c(10,4,1,1))
means<-(tapply(data$MicromolCO2.hr.g.soil.C, data$Species, mean, na.rm=T))
means<-c(means[1], means[3], means[2], means[4])
se<-tapply(data$MicromolCO2.hr.g.soil.C, data$Species, sd, na.rm=T)/sqrt(tapply(data$MicromolCO2.hr.g.soil.C, data$Species, nnzero, na.counted=F))
se<-c(se[1], se[3], se[2], se[4])
upper<-means+se
lower<-means-se

barplot(tapply(data$MicromolCO2.hr.g.soil.C, data$Species, mean, na.rm=T), las=2, ylab = "Micromol CO2/hr/g soil C", ylim=c(0, 500))
arrows(xs, lower, xs, upper, angle=90, length=0.1, lwd=3,col="red", code=3)

#######
means<-(tapply(data$CaConcentration, data$Species, mean, na.rm=T))
means<-c(means[1], means[3], means[2], means[4])
se<-tapply(data$CaConcentration, data$Species, sd, na.rm=T)/sqrt(tapply(data$CaConcentration, data$Species, nnzero, na.counted=F))
se<-c(se[1], se[3], se[2], se[4])
upper<-means+se
lower<-means-se

barplot(tapply(data$CaConcentration, data$Species, mean, na.rm=T), las=2, ylab = "Mean Soil Ca Concentration", ylim=c(0,2))

############
means<-(tapply(data$Soil.T, data$Species, mean, na.rm=T))
means<-c(means[1], means[3], means[2], means[4])
se<-tapply(data$Soil.T, data$Species, sd, na.rm=T)/sqrt(tapply(data$Soil.T, data$Species, nnzero, na.counted=F))
se<-c(se[1], se[3], se[2], se[4])
upper<-means+se
lower<-means-se

barplot(tapply(data$Soil.T, data$Species, mean, na.rm=T), las=2, ylab = "Mean Soil T", ylim=c(0,23))

############
means<-(tapply(data$Soil.M, data$Species, mean, na.rm=T))
means<-c(means[1], means[3], means[2], means[4])
se<-tapply(data$Soil.M, data$Species, sd, na.rm=T)/sqrt(tapply(data$Soil.M, data$Species, nnzero, na.counted=F))
se<-c(se[1], se[3], se[2], se[4])
upper<-means+se
lower<-means-se

barplot(tapply(data$Soil.M, data$Species, mean, na.rm=T), las=2, ylab = "Mean Soil M", ylim=c(0,40))

###################
means<-(tapply(data$TOC_MIB, data$Species, mean, na.rm=T))
means<-c(means[1], means[3], means[2], means[4])
se<-tapply(data$TOC_MIB, data$Species, sd, na.rm=T)/sqrt(tapply(data$TOC_MIB, data$Species, nnzero, na.counted=F))
se<-c(se[1], se[3], se[2], se[4])
upper<-means+se
lower<-means-se

barplot(tapply(data$TOC_MIB, data$Species, mean, na.rm=T), las=2, ylab = "Mean Microbial Biomass C", ylim=c(0,1))


