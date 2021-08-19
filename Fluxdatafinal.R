setwd("~/Desktop/Summer_Science_Gill_2021/Flux data/Summaries")
data1<- read.csv("20210615_FluxMeasurement1_SummaryRespirationData_Means.csv")
data2<- read.csv("20210705_SummaryRespirationData.csv")
data3<- read.csv("202707and203007_SummaryRespirationData.csv")
data4<- read.csv("21071510_SummarySoilRespData.csv")

library("Matrix")
library("Hmisc")

par(mfrow = c(1,3), mar= c(5,4,2,2))
###########June 15
L_data1<- subset(data1, subset = data1$U_L=="L")
U_data1<- subset(data1, subset = data1$U_L=="U")

L_means<-tapply(L_data1$slopes, L_data1$CollarNumber, mean, na.rm=T)
U_means<-tapply(U_data1$slopes, U_data1$CollarNumber, mean, na.rm=T)
means<-c(L_means, U_means)

L_se<-tapply(L_data1$slopes, L_data1$CollarNumber, sd, na.rm=T)/sqrt(tapply(L_data1$slopes, L_data1$CollarNumber, nnzero, na.counted=F))
U_se<-tapply(U_data1$slopes, U_data1$CollarNumber, sd, na.rm=T)/sqrt(tapply(U_data1$slopes, U_data1$CollarNumber, nnzero, na.counted=F))
se<-c(L_se, U_se)

upper<-means+se
lower<-means-se

boxplot(means~data1$U_L, col=c("red", "blue"), xlab = "Day 166", ylab = "CO2 umol m2", ylim=c(0,13))

##############July 15
L_data4<- subset(data4, subset = data4$U_L=="L")
U_data4<- subset(data4, subset = data4$U_L=="U")

L_means<-tapply(L_data4$slopes, L_data4$CollarNumber, mean, na.rm=T)
U_means<-tapply(U_data4$slopes, U_data4$CollarNumber, mean, na.rm=T)
means<-c(L_means, U_means)

L_se<-tapply(L_data4$slopes, L_data4$CollarNumber, sd, na.rm=T)/sqrt(tapply(L_data4$slopes, L_data4$CollarNumber, nnzero, na.counted=F))
U_se<-tapply(U_data4$slopes, U_data4$CollarNumber, sd, na.rm=T)/sqrt(tapply(U_data4$slopes, U_data4$CollarNumber, nnzero, na.counted=F))
se<-c(L_se, U_se)

upper<-means+se
lower<-means-se

boxplot(means~data4$U_L, col=c("red", "blue"), xlab = "Day 196", ylab = "", yaxt = "n", ylim=c(0,12))

#################July 21
L_data3<- subset(data3, subset = data3$U_L=="L")
U_data3<- subset(data3, subset = data3$U_L=="U")

L_means<-tapply(L_data3$slopes, L_data3$CollarNumber, mean, na.rm=T)
U_means<-tapply(U_data3$slopes, U_data3$CollarNumber, mean, na.rm=T)
means<-c(L_means, U_means)

L_se<-tapply(L_data3$slopes, L_data3$CollarNumber, sd, na.rm=T)/sqrt(tapply(L_data3$slopes, L_data3$CollarNumber, nnzero, na.counted=F))
U_se<-tapply(U_data3$slopes, U_data3$CollarNumber, sd, na.rm=T)/sqrt(tapply(U_data3$slopes, U_data3$CollarNumber, nnzero, na.counted=F))
se<-c(L_se, U_se)

upper<-means+se
lower<-means-se

boxplot(means~data3$U_L, col=c("red", "blue"), xlab = "Day 208", ylab = "", yaxt = "n", ylim = c(0,12))

###############Soil T 
par(mfrow = c(1,3), mar= c(5,4,2,2))
plot(data1$Soil.T, data1$slopes, col=c(rep("red", 24), rep("blue",24)), xlab = "Day 166", ylab = "CO2 umol m2")
fit1<-lm(data1$slopes~data1$Soil.T)
abline(fit1, col="slategrey")
summary(fit1)
legend("topleft", "p = 0.03656")

plot(data4$Soil.T, data4$slopes, col=c(rep("red", 24), rep("blue",24)), xlab = "Day 196", ylab = "", yaxt = "n", ylim = c(0,10))
fit1<-lm(data4$slopes~data4$Soil.T)
abline(fit1, col="slategrey")
summary(fit1)

plot(data3$Soil.T, data3$slopes, col=c(rep("red", 24), rep("blue",24)), xlab = "Day 208", ylab = "", yaxt = "n")
fit1<-lm(data3$slopes~data3$Soil.T)
abline(fit1, col="slategrey")
summary(fit1)


###############Soil M
plot(data1$Soil.M, data1$slopes, col=c(rep("red", 24), rep("blue",24)), xlab = "Day 166", ylab = "CO2 umol m2")
fit1<-lm(data1$slopes~data1$Soil.M)
abline(fit1, col="slategrey")
summary(fit1)
legend("topleft", "p = 0.00618")

plot(data4$Soil.M, data4$slopes, col=c(rep("red", 24), rep("blue",24)), xlab = "Day 196", ylab = "",yaxt = "n", ylim = c(0,12))
fit1<-lm(data4$slopes~data4$Soil.M)
abline(fit1, col="slategrey")
summary(fit1)

plot(data3$Soil.M, data3$slopes, col=c(rep("red", 24), rep("blue",24)), xlab = "Day 208", ylab = "", yaxt = "n")
fit1<-lm(data3$slopes~data3$Soil.M)
abline(fit1, col="slategrey")
summary(fit1)
legend("topright", "p = 0.003217")





