setwd("~/Desktop/Summer_Science_Gill_2021/Flux data/Summaries")
data<- read.csv("20210615_FluxMeasurement1_SummaryRespirationData_Means.csv")
colnames(data)

#####Plot Soil T and Flux mean
plot(data$Soil.T, data$Flux_mean, main = "Co2 Flux as a Function of Soil Temperature, Measurement 1",
     xlab = "Soil Temperature (C)", ylab = "Co2 Flux (umol/m^2)") 
fit1<-lm(data$Flux_mean~data$Soil.T)
abline(fit1, col="red")
summary(fit1)
summary(lm(data$Flux_mean~data$Soil.T))$adj.r.squared

#####Plot Soil M and Flux mean
plot(data$Soil.M, data$Flux_mean, main = "Co2 Flux as a Function of Soil Moisture, Measurement 1",
     xlab = "Soil Moisture", ylab = "Co2 Flux (umol/m^2)") 
fit1<-lm(data$Flux_mean~data$Soil.M)
abline(fit1, col="red")
summary(fit1)
summary(lm(data$Flux_mean~data$Soil.M))$adj.r.squared

#####Plot Soil T and Soil M
plot(data$Soil.T, data$Soil.M, main = "Soil Moisture as a Function of Soil Temperature, Measurement 1",
     xlab = "Soil Temperature", ylab = "Soil Moisture") 
fit1<-lm(data$Soil.M~data$Soil.T)
abline(fit1, col="red")
summary(fit1)
summary(lm(data$Soil.T~data$Soil.M))$adj.r.squared


       