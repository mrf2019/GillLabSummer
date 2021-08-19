setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates")
data<-read.csv("Shannon1.csv")
head(data)

#Plot carbon
plot(data$Carbon_Percent, data$Shannon)
fit1<-lm(data$Shannon~data$Carbon_Percent)
abline(fit1, col="red")
summary(fit1)

#Plot nitrogen 
plot(data$Nitrogen_Percent, data$Shannon)
fit1<-lm(data$Shannon~data$Nitrogen_Percent)
abline(fit1, col="red")
summary(fit1)
