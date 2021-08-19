setwd("~/Desktop/Summer_Science_Gill_2021")
data<- read.csv("Summer2021MasterData.csv")
head(data)

plot(data$MicromolCO2_hr_gdrysoil, data$p_soil_C, xlab = "% Soil C",
     ylab = "MicromolCO2/hr/gdrysoil", main = "Soil Respiration per g Soil C")
fit1<-lm(data$p_soil_C~data$MicromolCO2_hr_gdrysoi)
abline(fit1, col="red")
summary(fit1)


