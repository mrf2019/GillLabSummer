setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates")
data<-read.csv("diversitymetrics.csv")
head(data)

#Plot carbon and shannon
plot(data$Carbon_Percent, data$Shannon)
fit1<-lm(data$Shannon~data$Carbon_Percent)
abline(fit1, col="red")
summary(fit1)

#Plot nitrogen and shannon
plot(data$Nitrogen_Percent, data$Shannon)
fit1<-lm(data$Shannon~data$Nitrogen_Percent)
abline(fit1, col="red")
summary(fit1)

#Plot earthworm and carbon
plot(data$Carbon_Percent, data$EarthwormRelAb)
fit1<-lm(data$EarthwormRelAb~data$Carbon_Percent)
abline(fit1, col="red")
summary(fit1)

#Plot earthworm and nitrogen
plot(data$Nitrogen_Percent, data$EarthwormRelAb)
fit1<-lm(data$EarthwormRelAb~data$Nitrogen_Percent)
abline(fit1, col="red")
summary(fit1)
