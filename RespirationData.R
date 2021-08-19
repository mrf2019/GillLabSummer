setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("Lab respiration data - with inverts.csv")
head(data)

rep(c(rep("Lower", 24), rep("Upper", 24)),4)
data$Site<-rep(c(rep("Lower", 24), rep("Upper", 24)),4)
data$Site_Date<-paste(data$Site, data$Date, sep="_")

Day200<- subset(data, subset = data$Date=="200")
Day207<- subset(data, subset = data$Date=="207")
Day214<- subset(data, subset = data$Date=="214")
Day221<-subset(data, subset = data$Date=="221")

par(mfrow=c(1,1))
plot(Day200$MicromolCO2.hr.g.soil.C, xlab = "Plots 1-48", xaxt = 'n', ylab = "umol CO2/hr/g soil C")
points(Day207$MicromolCO2.hr.g.soil.C, col='blue')
points(Day214$MicromolCO2.hr.g.soil.C, col='forest green')
points(Day221$MicromolCO2.hr.g.soil.C, col='red')
abline(v=24, col="dark grey", lwd=2, lty=2)
legend("topright", c("Week 1", "Week 2", "Week 3", "Week 4"), col=c("black", "blue", "forest green", "red"), pch=c(16), bty="n")

par(mfrow=c(2,1), mar=c(1,4,3,1))
fit1<-lm(data$MicromolCO2.hr.g.soil.C~data$Date*data$U_L)
summary(fit1)
anova(fit1)
boxplot(data$MicromolCO2.hr.g.soil.C~data$U_L*data$Date, col=c("red", "blue"))
abline(v=2.5, col="dark grey", lwd=2, lty=2)
abline(v=4.5, lwd=3, lty=2, col="grey")
abline(v=6.5, lwd=3, lty=2, col="grey")

#################################################################
library("Matrix")
library("Hmisc")

means<-tapply(data$MicromolCO2.hr.g.soil.C, data$Site_Date, mean, na.rm=T)
means<-c(means[1], means[3], means[2], means[4],means[5],means[6],means[7],means[8])
se<-tapply(data$MicromolCO2.hr.g.soil.C, data$Site_Date, sd, na.rm=T)/sqrt(tapply(data$MicromolCO2.hr.g.soil.C, data$Site_Date, nnzero, na.counted=F))
se<-c(se[1], se[3], se[2], se[4],se[5],se[6])
upper<-means+se
lower<-means-se

xs<-c(1,2,3,4,5,6)
plot(xs, as.vector(means), ylab = "Mean Micromol CO2/hr/g soil C", ylim=c(min(lower-10), max(upper+10)), xaxt="n", xlab="Incubation Day", xlim=c(0.5,4.5), col=c("red", "blue", "red", "blue","red","blue"), pch=16)
arrows(xs, lower, xs, upper, angle=90, length=0.1, lwd=3,col=c("red", "blue", "red", "blue","red","blue"), code=3 )
abline(v=2.5, lwd=3, lty=2, col="grey")
legend("topright", c("Lower", "Upper"), col=c("red", "blue"), pch=16, bty="n")
axis(1, las=1, at=c(1.5, 3.5), labels = c("Day 1", "Day 7", "Day 14", "Day 21"))

dim(data)

setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
dataNI<- read.csv("LabRespNoinvert.csv")
dataNI<-dataNI[,c(2:145)]
head(dataNI)
dim(dataNI)

###dataNI$Site_Date<-c(rep("Lower1", 24), rep("Upper1", 24), rep("Lower2",24), rep("Upper2",24), rep("Lower3",24), rep("Upper3",24))

rep(c(rep("Lower", 24), rep("Upper", 24)),3)
dataNI$Site<-rep(c(rep("Lower", 24), rep("Upper", 24)),3)
dataNI$Site_Date<-paste(dataNI$Site, dataNI$Date, sep="_")

means<-tapply(dataNI$MicromolCO2.hr.g.soil.C, dataNI$Site_Date, mean, na.rm=T)
means<-c(means[1], means[3], means[2], means[4], means[5], means[6])
se<-tapply(dataNI$MicromolCO2.hr.g.soil.C, dataNI$Site_Date, sd, na.rm=T)/sqrt(tapply(dataNI$MicromolCO2.hr.g.soil.C, dataNI$Site_Date, nnzero, na.counted=F))
se<-c(se[1], se[3], se[2], se[4], se[5], se[6])
upper<-means+se
lower<-means-se

xs<-c(1,2,3,4,5,6)
plot(xs, as.vector(means), ylab = "Mean Micromol CO2/hr/g soil C", ylim=c(min(lower-10), max(upper+10)), xaxt="n", xlab="Incubation Day", 
     xlim=c(0.5,6.5), col=c("red", "blue", "red", "blue"), pch=16)
arrows(xs, lower, xs, upper, angle=90, length=0.1, lwd=3,col=c("red", "blue", "red", "blue", "red","blue"), code=3 )
abline(v=2.5, lwd=3, lty=2, col="grey")
abline(v=4.5, lwd=3, lty=2, col="grey")
legend("topright", c("Lower", "Upper"), col=c("red", "blue"), pch=16, bty="n")
axis(1, las=1, at=c(1.5, 3.5, 5.5), labels = c("Day 1", "Day 7", "Day 14"))





