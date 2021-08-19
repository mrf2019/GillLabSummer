##### Script to process soil respiration fluxes
setwd("~/Desktop/Summer_Science_Gill_2021/Flux data") # Update to match individual computer directories

install.packages("lubridate")  # need to run this line once to install lubridate R package. After you run it once, you can just use the library command
library(lubridate)

filenames <- list.files("Flux measurement 1", pattern="*.csv", full.names=TRUE) 
ldf <- lapply(filenames, read.csv)

### Bind all the relevent files in the "CSV files" folder into one big table. 
##### This step will take several minutes to run if there are a lot of files.
load_data <- function(path) { 
   files <- dir("Flux measurement 4", pattern = '\\.csv', full.names = TRUE)
   tables <- lapply(files, read.csv)
   do.call(rbind, tables)
}
data <- load_data("Flux measurement 4")
dim(data)

data <- read.csv("20210615_LowerSite_SoilRespiration.csv")
dim(data)
head(data)
colnames(data)


################ Flux calculation Collar 1
Collar1<-subset(data, subset = data$Plot_No=="1")
dim(Collar1)

plot(Collar1$Rec_No, Collar1$CO2)

Collar1_short <- Collar1[c(8:60), ]
plot(Collar1_short$Rec_No, Collar1_short$CO2)
Collar1_short[,"Rec_No"]
Collar1_short$Rec_No


##### n = RT/PV
n = ((Collar1$Pressure[1]/1000)*2)/((0.08314)*(Collar1$Tair[1]+273.15))
Collar1_short$CO2_umol_m2 <- (Collar1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar1_short$Rec_No,Collar1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main= "Co2 Flux Collar 1")
#lm(y~x)
fit1<-lm(Collar1_short$CO2_umol_m2~Collar1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_1<-(summary(lm(Collar1_short$CO2_umol_m2~Collar1_short$Rec_No))$coefficients[2])
R1<-summary(lm(Collar1_short$CO2_umol_m2~Collar1_short$Rec_No))$adj.r.squared


################ Flux calculation Collar 2
Collar2<-subset(data, subset = data$Plot_No=="2")
dim(Collar2)

plot(Collar2$Rec_No, Collar2$CO2)

Collar2_short <- Collar2[c(15:60), ]
plot(Collar2_short$Rec_No, Collar2_short$CO2)
Collar2_short[,"Rec_No"]
Collar2_short$Rec_No


##### n = RT/PV
n = ((Collar2$Pressure[1]/1000)*2)/((0.08314)*(Collar2$Tair[1]+273.15))
Collar2_short$CO2_umol_m2 <- (Collar2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar2_short$Rec_No,Collar2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", xlab = "Rec No", main= "Co2 Flux Collar 2")
#lm(y~x)
fit1<-lm(Collar2_short$CO2_umol_m2~Collar2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_2<-(summary(lm(Collar2_short$CO2_umol_m2~Collar2_short$Rec_No))$coefficients[2])
R2<-summary(lm(Collar2_short$CO2_umol_m2~Collar2_short$Rec_No))$adj.r.squared

####################################################################################################
################ Flux calculation Collar 3
Collar3<-subset(data, subset = data$Plot_No=="3")
dim(Collar3)

plot(Collar3$Rec_No, Collar3$CO2)

Collar3_short <- Collar3[c(15:60), ]
plot(Collar3_short$Rec_No, Collar3_short$CO2)
Collar3_short[,"Rec_No"]
Collar3_short$Rec_No


##### n = RT/PV
n = ((Collar3$Pressure[1]/1000)*2)/((0.08314)*(Collar3$Tair[1]+273.15))
Collar3_short$CO2_umol_m2 <- (Collar3_short$CO2*n/0.008)

#plot(x,y)
plot(Collar3_short$Rec_No,Collar3_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main= "Co2 Flux Collar 3")
#lm(y~x)
fit1<-lm(Collar3_short$CO2_umol_m2~Collar3_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_3<-(summary(lm(Collar3_short$CO2_umol_m2~Collar3_short$Rec_No))$coefficients[2])
R3<-summary(lm(Collar3_short$CO2_umol_m2~Collar3_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 4
Collar4<-subset(data, subset = data$Plot_No=="4")
dim(Collar4)

plot(Collar4$Rec_No, Collar4$CO2)

Collar4_short <- Collar4[c(8:60), ]
plot(Collar4_short$Rec_No, Collar4_short$CO2)
Collar4_short[,"Rec_No"]
Collar4_short$Rec_No


##### n = RT/PV
n = ((Collar4$Pressure[1]/1000)*2)/((0.08314)*(Collar4$Tair[1]+273.15))
Collar4_short$CO2_umol_m2 <- (Collar4_short$CO2*n/0.008)

#plot(x,y)
plot(Collar4_short$Rec_No,Collar4_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main= "Co2 Flux Collar 4")
#lm(y~x)
fit1<-lm(Collar4_short$CO2_umol_m2~Collar4_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_4<-(summary(lm(Collar4_short$CO2_umol_m2~Collar4_short$Rec_No))$coefficients[2])
R4<-summary(lm(Collar4_short$CO2_umol_m2~Collar4_short$Rec_No))$adj.r.squared

################ Flux calculation Collar 5
Collar5<-subset(data, subset = data$Plot_No=="5")
dim(Collar5)

plot(Collar5$Rec_No, Collar5$CO2)

Collar5_short <- Collar5[c(16:60), ]
plot(Collar5_short$Rec_No, Collar5_short$CO2)
Collar5_short[,"Rec_No"]
Collar5_short$Rec_No


##### n = RT/PV
n = ((Collar5$Pressure[1]/1000)*2)/((0.08314)*(Collar5$Tair[1]+273.15))
Collar5_short$CO2_umol_m2 <- (Collar5_short$CO2*n/0.008)

#plot(x,y)
plot(Collar5_short$Rec_No,Collar5_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 5")
#lm(y~x)
fit1<-lm(Collar5_short$CO2_umol_m2~Collar5_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_5<-(summary(lm(Collar5_short$CO2_umol_m2~Collar5_short$Rec_No))$coefficients[2])
R5<-summary(lm(Collar5_short$CO2_umol_m2~Collar5_short$Rec_No))$adj.r.squared


################ Flux calculation Collar 6
Collar6<-subset(data, subset = data$Plot_No=="6")
dim(Collar6)

plot(Collar6$Rec_No, Collar6$CO2)

Collar6_short <- Collar6[c(8:60), ]
plot(Collar6_short$Rec_No, Collar6_short$CO2)
Collar6_short[,"Rec_No"]
Collar6_short$Rec_No


##### n = RT/PV
n = ((Collar6$Pressure[1]/1000)*2)/((0.08314)*(Collar6$Tair[1]+273.15))
Collar6_short$CO2_umol_m2 <- (Collar6_short$CO2*n/0.008)

#plot(x,y)
plot(Collar6_short$Rec_No,Collar6_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 6")
#lm(y~x)
fit1<-lm(Collar6_short$CO2_umol_m2~Collar6_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_6<-(summary(lm(Collar6_short$CO2_umol_m2~Collar6_short$Rec_No))$coefficients[2])
R6<-summary(lm(Collar6_short$CO2_umol_m2~Collar6_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 7
Collar7<-subset(data, subset = data$Plot_No=="7")
dim(Collar7)

plot(Collar7$Rec_No, Collar7$CO2)

Collar7_short <- Collar7[c(8:60), ]
plot(Collar7_short$Rec_No, Collar7_short$CO2)
Collar7_short[,"Rec_No"]
Collar7_short$Rec_No


##### n = RT/PV
n = ((Collar7$Pressure[1]/1000)*2)/((0.08314)*(Collar7$Tair[1]+273.15))
Collar7_short$CO2_umol_m2 <- (Collar7_short$CO2*n/0.008)

#plot(x,y)
plot(Collar7_short$Rec_No,Collar7_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 7")
#lm(y~x)
fit1<-lm(Collar7_short$CO2_umol_m2~Collar7_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_7<-(summary(lm(Collar7_short$CO2_umol_m2~Collar7_short$Rec_No))$coefficients[2])
R7<-summary(lm(Collar7_short$CO2_umol_m2~Collar7_short$Rec_No))$adj.r.squared



###########Flux calculation Collar 8
Collar8<-subset(data, subset = data$Plot_No=="8")
dim(Collar8)

plot(Collar8$Rec_No, Collar8$CO2)

Collar8_short <- Collar8[c(10:60), ]
plot(Collar8_short$Rec_No, Collar8_short$CO2)
Collar8_short[,"Rec_No"]
Collar8_short$Rec_No


##### n = RT/PV
n = ((Collar8$Pressure[1]/1000)*2)/((0.08314)*(Collar8$Tair[1]+273.15))
Collar8_short$CO2_umol_m2 <- (Collar8_short$CO2*n/0.008)

#plot(x,y)
plot(Collar8_short$Rec_No,Collar8_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 8")
#lm(y~x)
fit1<-lm(Collar8_short$CO2_umol_m2~Collar8_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_8<-(summary(lm(Collar8_short$CO2_umol_m2~Collar8_short$Rec_No))$coefficients[2])
R8<-summary(lm(Collar8_short$CO2_umol_m2~Collar8_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 9
Collar9<-subset(data, subset = data$Plot_No=="9")
dim(Collar9)

plot(Collar9$Rec_No, Collar9$CO2)

Collar9_short <- Collar9[c(8:60), ]
plot(Collar9_short$Rec_No, Collar9_short$CO2)
Collar9_short[,"Rec_No"]
Collar9_short$Rec_No


##### n = RT/PV
n = ((Collar9$Pressure[1]/1000)*2)/((0.08314)*(Collar9$Tair[1]+273.15))
Collar9_short$CO2_umol_m2 <- (Collar9_short$CO2*n/0.008)

#plot(x,y)
plot(Collar9_short$Rec_No,Collar9_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 9")
#lm(y~x)
fit1<-lm(Collar9_short$CO2_umol_m2~Collar9_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_9<-(summary(lm(Collar9_short$CO2_umol_m2~Collar9_short$Rec_No))$coefficients[2])
R9<-summary(lm(Collar9_short$CO2_umol_m2~Collar9_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 10
Collar10<-subset(data, subset = data$Plot_No=="10")
dim(Collar10)

plot(Collar10$Rec_No, Collar10$CO2)

Collar10_short <- Collar10[c(8:60), ]
plot(Collar10_short$Rec_No, Collar10_short$CO2)
Collar10_short[,"Rec_No"]
Collar10_short$Rec_No


##### n = RT/PV
n = ((Collar10$Pressure[1]/1000)*2)/((0.08314)*(Collar10$Tair[1]+273.15))
Collar10_short$CO2_umol_m2 <- (Collar10_short$CO2*n/0.008)

#plot(x,y)
plot(Collar10_short$Rec_No,Collar10_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 10")
#lm(y~x)
fit1<-lm(Collar10_short$CO2_umol_m2~Collar10_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_10<-(summary(lm(Collar10_short$CO2_umol_m2~Collar10_short$Rec_No))$coefficients[2])
R10<-summary(lm(Collar10_short$CO2_umol_m2~Collar10_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 11
Collar11<-subset(data, subset = data$Plot_No=="11")
dim(Collar11)

plot(Collar11$Rec_No, Collar11$CO2)

Collar11_short <- Collar11[c(10:60), ]
plot(Collar11_short$Rec_No, Collar11_short$CO2)
Collar11_short[,"Rec_No"]
Collar11_short$Rec_No


##### n = RT/PV
n = ((Collar11$Pressure[1]/1000)*2)/((0.08314)*(Collar11$Tair[1]+273.15))
Collar11_short$CO2_umol_m2 <- (Collar11_short$CO2*n/0.008)

#plot(x,y)
plot(Collar11_short$Rec_No,Collar11_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 11")
#lm(y~x)
fit1<-lm(Collar11_short$CO2_umol_m2~Collar11_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_11<-(summary(lm(Collar11_short$CO2_umol_m2~Collar11_short$Rec_No))$coefficients[2])
R11<-summary(lm(Collar11_short$CO2_umol_m2~Collar11_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 12
Collar12<-subset(data, subset = data$Plot_No=="12")
dim(Collar12)

plot(Collar12$Rec_No, Collar12$CO2)

Collar12_short <- Collar12[c(9:60), ]
plot(Collar12_short$Rec_No, Collar12_short$CO2)
Collar12_short[,"Rec_No"]
Collar12_short$Rec_No


##### n = RT/PV
n = ((Collar12$Pressure[1]/1000)*2)/((0.08314)*(Collar12$Tair[1]+273.15))
Collar12_short$CO2_umol_m2 <- (Collar12_short$CO2*n/0.008)

#plot(x,y)
plot(Collar12_short$Rec_No,Collar12_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 12")
#lm(y~x)
fit1<-lm(Collar12_short$CO2_umol_m2~Collar12_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_12<-(summary(lm(Collar12_short$CO2_umol_m2~Collar12_short$Rec_No))$coefficients[2])
R12<-summary(lm(Collar12_short$CO2_umol_m2~Collar12_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 13
Collar13<-subset(data, subset = data$Plot_No=="13")
dim(Collar13)

plot(Collar13$Rec_No, Collar13$CO2)

Collar13_short <- Collar13[c(10:60), ]
plot(Collar13_short$Rec_No, Collar13_short$CO2)
Collar13_short[,"Rec_No"]
Collar13_short$Rec_No


##### n = RT/PV
n = ((Collar13$Pressure[1]/1000)*2)/((0.08314)*(Collar13$Tair[1]+273.15))
Collar13_short$CO2_umol_m2 <- (Collar13_short$CO2*n/0.008)

#plot(x,y)
plot(Collar13_short$Rec_No,Collar13_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 13")
#lm(y~x)
fit1<-lm(Collar13_short$CO2_umol_m2~Collar13_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_13<-(summary(lm(Collar13_short$CO2_umol_m2~Collar13_short$Rec_No))$coefficients[2])
R13<-summary(lm(Collar13_short$CO2_umol_m2~Collar13_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 14
Collar14<-subset(data, subset = data$Plot_No=="14")
dim(Collar14)

plot(Collar14$Rec_No, Collar14$CO2)

Collar14_short <- Collar14[c(8:60), ]
plot(Collar14_short$Rec_No, Collar14_short$CO2)
Collar14_short[,"Rec_No"]
Collar14_short$Rec_No


##### n = RT/PV
n = ((Collar14$Pressure[1]/1000)*2)/((0.08314)*(Collar14$Tair[1]+273.15))
Collar14_short$CO2_umol_m2 <- (Collar14_short$CO2*n/0.008)

#plot(x,y)
plot(Collar14_short$Rec_No,Collar14_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 14")
#lm(y~x)
fit1<-lm(Collar14_short$CO2_umol_m2~Collar14_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_14<-(summary(lm(Collar14_short$CO2_umol_m2~Collar14_short$Rec_No))$coefficients[2])
R14<-summary(lm(Collar14_short$CO2_umol_m2~Collar14_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 15
Collar15<-subset(data, subset = data$Plot_No=="15")
dim(Collar15)

plot(Collar15$Rec_No, Collar15$CO2)

Collar15_short <- Collar15[c(15:60), ]
plot(Collar15_short$Rec_No, Collar15_short$CO2)
Collar15_short[,"Rec_No"]
Collar15_short$Rec_No


##### n = RT/PV
n = ((Collar15$Pressure[1]/1000)*2)/((0.08314)*(Collar15$Tair[1]+273.15))
Collar15_short$CO2_umol_m2 <- (Collar15_short$CO2*n/0.008)

#plot(x,y)
plot(Collar15_short$Rec_No,Collar15_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 15")
#lm(y~x)
fit1<-lm(Collar15_short$CO2_umol_m2~Collar15_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_15<-(summary(lm(Collar15_short$CO2_umol_m2~Collar15_short$Rec_No))$coefficients[2])
R15<-summary(lm(Collar15_short$CO2_umol_m2~Collar15_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 16
Collar16<-subset(data, subset = data$Plot_No=="16")
dim(Collar16)

plot(Collar16$Rec_No, Collar16$CO2)

Collar16_short <- Collar16[c(8:60), ]
plot(Collar16_short$Rec_No, Collar16_short$CO2)
Collar16_short[,"Rec_No"]
Collar16_short$Rec_No


##### n = RT/PV
n = ((Collar16$Pressure[1]/1000)*2)/((0.08314)*(Collar16$Tair[1]+273.15))
Collar16_short$CO2_umol_m2 <- (Collar16_short$CO2*n/0.008)

#plot(x,y)
plot(Collar16_short$Rec_No,Collar16_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 16")
#lm(y~x)
fit1<-lm(Collar16_short$CO2_umol_m2~Collar16_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_16<-(summary(lm(Collar16_short$CO2_umol_m2~Collar16_short$Rec_No))$coefficients[2])
R16<-summary(lm(Collar16_short$CO2_umol_m2~Collar16_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 17
Collar17<-subset(data, subset = data$Plot_No=="17")
dim(Collar17)

plot(Collar17$Rec_No, Collar17$CO2)

Collar17_short <- Collar17[c(8:60), ]
plot(Collar17_short$Rec_No, Collar17_short$CO2)
Collar17_short[,"Rec_No"]
Collar17_short$Rec_No


##### n = RT/PV
n = ((Collar17$Pressure[1]/1000)*2)/((0.08314)*(Collar17$Tair[1]+273.15))
Collar17_short$CO2_umol_m2 <- (Collar17_short$CO2*n/0.008)

#plot(x,y)
plot(Collar17_short$Rec_No,Collar17_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 17")
#lm(y~x)
fit1<-lm(Collar17_short$CO2_umol_m2~Collar17_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_17<-(summary(lm(Collar17_short$CO2_umol_m2~Collar17_short$Rec_No))$coefficients[2])
R17<-summary(lm(Collar17_short$CO2_umol_m2~Collar17_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 18
Collar18<-subset(data, subset = data$Plot_No=="18")
dim(Collar18)

plot(Collar18$Rec_No, Collar18$CO2)

Collar18_short <- Collar18[c(8:60), ]
plot(Collar18_short$Rec_No, Collar18_short$CO2)
Collar18_short[,"Rec_No"]
Collar18_short$Rec_No


##### n = RT/PV
n = ((Collar18$Pressure[1]/1000)*2)/((0.08314)*(Collar18$Tair[1]+273.15))
Collar18_short$CO2_umol_m2 <- (Collar18_short$CO2*n/0.008)

#plot(x,y)
plot(Collar18_short$Rec_No,Collar18_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 18")
#lm(y~x)
fit1<-lm(Collar18_short$CO2_umol_m2~Collar18_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_18<-(summary(lm(Collar18_short$CO2_umol_m2~Collar18_short$Rec_No))$coefficients[2])
R18<-summary(lm(Collar18_short$CO2_umol_m2~Collar18_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 19
Collar19<-subset(data, subset = data$Plot_No=="19")
dim(Collar19)

plot(Collar19$Rec_No, Collar19$CO2)

Collar19_short <- Collar19[c(15:60), ]
plot(Collar19_short$Rec_No, Collar19_short$CO2)
Collar19_short[,"Rec_No"]
Collar19_short$Rec_No


##### n = RT/PV
n = ((Collar19$Pressure[1]/1000)*2)/((0.08314)*(Collar19$Tair[1]+273.15))
Collar19_short$CO2_umol_m2 <- (Collar19_short$CO2*n/0.008)

#plot(x,y)
plot(Collar19_short$Rec_No,Collar19_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 19")
#lm(y~x)
fit1<-lm(Collar19_short$CO2_umol_m2~Collar19_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_19<-(summary(lm(Collar19_short$CO2_umol_m2~Collar19_short$Rec_No))$coefficients[2])
R19<-summary(lm(Collar19_short$CO2_umol_m2~Collar19_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 20
Collar20<-subset(data, subset = data$Plot_No=="20")
dim(Collar20)

plot(Collar20$Rec_No, Collar20$CO2)

Collar20_short <- Collar20[c(8:60), ]
plot(Collar20_short$Rec_No, Collar20_short$CO2)
Collar20_short[,"Rec_No"]
Collar20_short$Rec_No


##### n = RT/PV
n = ((Collar20$Pressure[1]/1000)*2)/((0.08314)*(Collar20$Tair[1]+273.15))
Collar20_short$CO2_umol_m2 <- (Collar20_short$CO2*n/0.008)

#plot(x,y)
plot(Collar20_short$Rec_No,Collar20_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 20")
#lm(y~x)
fit1<-lm(Collar20_short$CO2_umol_m2~Collar20_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_20<-(summary(lm(Collar20_short$CO2_umol_m2~Collar20_short$Rec_No))$coefficients[2])
R20<-summary(lm(Collar20_short$CO2_umol_m2~Collar20_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 21
Collar21<-subset(data, subset = data$Plot_No=="21")
dim(Collar21)

plot(Collar21$Rec_No, Collar21$CO2)

Collar21_short <- Collar21[c(12:60), ]
plot(Collar21_short$Rec_No, Collar21_short$CO2)
Collar21_short[,"Rec_No"]
Collar21_short$Rec_No


##### n = RT/PV
n = ((Collar21$Pressure[1]/1000)*2)/((0.08314)*(Collar21$Tair[1]+273.15))
Collar21_short$CO2_umol_m2 <- (Collar21_short$CO2*n/0.008)

#plot(x,y)
plot(Collar21_short$Rec_No,Collar21_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 21")
#lm(y~x)
fit1<-lm(Collar21_short$CO2_umol_m2~Collar21_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_21<-(summary(lm(Collar21_short$CO2_umol_m2~Collar21_short$Rec_No))$coefficients[2])
R21<-summary(lm(Collar21_short$CO2_umol_m2~Collar21_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 22
Collar22<-subset(data, subset = data$Plot_No=="22")
dim(Collar22)

plot(Collar22$Rec_No, Collar22$CO2)

Collar22_short <- Collar22[c(8:60), ]
plot(Collar22_short$Rec_No, Collar22_short$CO2)
Collar22_short[,"Rec_No"]
Collar22_short$Rec_No


##### n = RT/PV
n = ((Collar22$Pressure[1]/1000)*2)/((0.08314)*(Collar22$Tair[1]+273.15))
Collar22_short$CO2_umol_m2 <- (Collar22_short$CO2*n/0.008)

#plot(x,y)
plot(Collar22_short$Rec_No,Collar22_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 22")
#lm(y~x)
fit1<-lm(Collar22_short$CO2_umol_m2~Collar22_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_22<-(summary(lm(Collar22_short$CO2_umol_m2~Collar22_short$Rec_No))$coefficients[2])
R22<-summary(lm(Collar22_short$CO2_umol_m2~Collar22_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 23
Collar23<-subset(data, subset = data$Plot_No=="23")
dim(Collar23)

plot(Collar23$Rec_No, Collar23$CO2)

Collar23_short <- Collar23[c(8:60), ]
plot(Collar23_short$Rec_No, Collar23_short$CO2)
Collar23_short[,"Rec_No"]
Collar23_short$Rec_No


##### n = RT/PV
n = ((Collar23$Pressure[1]/1000)*2)/((0.08314)*(Collar23$Tair[1]+273.15))
Collar23_short$CO2_umol_m2 <- (Collar23_short$CO2*n/0.008)

#plot(x,y)
plot(Collar23_short$Rec_No,Collar23_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 23")
#lm(y~x)
fit1<-lm(Collar23_short$CO2_umol_m2~Collar23_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_23<-(summary(lm(Collar23_short$CO2_umol_m2~Collar23_short$Rec_No))$coefficients[2])
R23<-summary(lm(Collar23_short$CO2_umol_m2~Collar23_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 24
Collar24<-subset(data, subset = data$Plot_No=="24")
dim(Collar24)

plot(Collar24$Rec_No, Collar24$CO2)

Collar24_short <- Collar24[c(8:60), ]
plot(Collar24_short$Rec_No, Collar24_short$CO2)
Collar24_short[,"Rec_No"]
Collar24_short$Rec_No


##### n = RT/PV
n = ((Collar24$Pressure[1]/1000)*2)/((0.08314)*(Collar24$Tair[1]+273.15))
Collar24_short$CO2_umol_m2 <- (Collar24_short$CO2*n/0.008)

#plot(x,y)
plot(Collar24_short$Rec_No,Collar24_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 24")
#lm(y~x)
fit1<-lm(Collar24_short$CO2_umol_m2~Collar24_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_24<-(summary(lm(Collar24_short$CO2_umol_m2~Collar24_short$Rec_No))$coefficients[2])
R24<-summary(lm(Collar24_short$CO2_umol_m2~Collar24_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 25
Collar25<-subset(data, subset = data$Plot_No=="25")
dim(Collar25)

plot(Collar25$Rec_No, Collar25$CO2)

Collar25_short <- Collar25[c(20:60), ]
plot(Collar25_short$Rec_No, Collar25_short$CO2)
Collar25_short[,"Rec_No"]
Collar25_short$Rec_No


##### n = RT/PV
n = ((Collar25$Pressure[1]/1000)*2)/((0.08314)*(Collar25$Tair[1]+273.15))
Collar25_short$CO2_umol_m2 <- (Collar25_short$CO2*n/0.008)

#plot(x,y)
plot(Collar25_short$Rec_No,Collar25_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 25")
#lm(y~x)
fit1<-lm(Collar25_short$CO2_umol_m2~Collar25_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_25<-(summary(lm(Collar25_short$CO2_umol_m2~Collar25_short$Rec_No))$coefficients[2])
R25<-summary(lm(Collar25_short$CO2_umol_m2~Collar25_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 26
Collar26<-subset(data, subset = data$Plot_No=="26")
dim(Collar26)

plot(Collar26$Rec_No, Collar26$CO2)

Collar26_short <- Collar26[c(8:60), ]
plot(Collar26_short$Rec_No, Collar26_short$CO2)
Collar26_short[,"Rec_No"]
Collar26_short$Rec_No


##### n = RT/PV
n = ((Collar26$Pressure[1]/1000)*2)/((0.08314)*(Collar26$Tair[1]+273.15))
Collar26_short$CO2_umol_m2 <- (Collar26_short$CO2*n/0.008)

#plot(x,y)
plot(Collar26_short$Rec_No,Collar26_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 26")
#lm(y~x)
fit1<-lm(Collar26_short$CO2_umol_m2~Collar26_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_26<-(summary(lm(Collar26_short$CO2_umol_m2~Collar26_short$Rec_No))$coefficients[2])
R26<-summary(lm(Collar26_short$CO2_umol_m2~Collar26_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 27
Collar27<-subset(data, subset = data$Plot_No=="27")
dim(Collar27)

plot(Collar27$Rec_No, Collar27$CO2)

Collar27_short <- Collar27[c(14:60), ]
plot(Collar27_short$Rec_No, Collar27_short$CO2)
Collar27_short[,"Rec_No"]
Collar27_short$Rec_No


##### n = RT/PV
n = ((Collar27$Pressure[1]/1000)*2)/((0.08314)*(Collar27$Tair[1]+273.15))
Collar27_short$CO2_umol_m2 <- (Collar27_short$CO2*n/0.008)

#plot(x,y)
plot(Collar27_short$Rec_No,Collar27_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 27")
#lm(y~x)
fit1<-lm(Collar27_short$CO2_umol_m2~Collar27_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_27<-(summary(lm(Collar27_short$CO2_umol_m2~Collar27_short$Rec_No))$coefficients[2])
R27<-summary(lm(Collar27_short$CO2_umol_m2~Collar27_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 28
Collar28<-subset(data, subset = data$Plot_No=="28")
dim(Collar28)

plot(Collar28$Rec_No, Collar28$CO2)

Collar28_short <- Collar28[c(8:60), ]
plot(Collar28_short$Rec_No, Collar28_short$CO2)
Collar28_short[,"Rec_No"]
Collar28_short$Rec_No


##### n = RT/PV
n = ((Collar28$Pressure[1]/1000)*2)/((0.08314)*(Collar28$Tair[1]+273.15))
Collar28_short$CO2_umol_m2 <- (Collar28_short$CO2*n/0.008)

#plot(x,y)
plot(Collar28_short$Rec_No,Collar28_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 28")
#lm(y~x)
fit1<-lm(Collar28_short$CO2_umol_m2~Collar28_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_28<-(summary(lm(Collar28_short$CO2_umol_m2~Collar28_short$Rec_No))$coefficients[2])
R28<-summary(lm(Collar28_short$CO2_umol_m2~Collar28_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 29
Collar29<-subset(data, subset = data$Plot_No=="29")
dim(Collar29)

plot(Collar29$Rec_No, Collar29$CO2)

Collar29_short <- Collar29[c(8:60), ]
plot(Collar29_short$Rec_No, Collar29_short$CO2)
Collar29_short[,"Rec_No"]
Collar29_short$Rec_No


##### n = RT/PV
n = ((Collar29$Pressure[1]/1000)*2)/((0.08314)*(Collar29$Tair[1]+273.15))
Collar29_short$CO2_umol_m2 <- (Collar29_short$CO2*n/0.008)

#plot(x,y)
plot(Collar29_short$Rec_No,Collar29_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 29")
#lm(y~x)
fit1<-lm(Collar29_short$CO2_umol_m2~Collar29_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_29<-(summary(lm(Collar29_short$CO2_umol_m2~Collar29_short$Rec_No))$coefficients[2])
R29<-summary(lm(Collar29_short$CO2_umol_m2~Collar29_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 30
Collar30<-subset(data, subset = data$Plot_No=="30")
dim(Collar30)

plot(Collar30$Rec_No, Collar30$CO2)

Collar30_short <- Collar30[c(8:60), ]
plot(Collar30_short$Rec_No, Collar30_short$CO2)
Collar30_short[,"Rec_No"]
Collar30_short$Rec_No


##### n = RT/PV
n = ((Collar30$Pressure[1]/1000)*2)/((0.08314)*(Collar30$Tair[1]+273.15))
Collar30_short$CO2_umol_m2 <- (Collar30_short$CO2*n/0.008)

#plot(x,y)
plot(Collar30_short$Rec_No,Collar30_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 30")
#lm(y~x)
fit1<-lm(Collar30_short$CO2_umol_m2~Collar30_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_30<-(summary(lm(Collar30_short$CO2_umol_m2~Collar30_short$Rec_No))$coefficients[2])
R30<-summary(lm(Collar30_short$CO2_umol_m2~Collar30_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 31
Collar31<-subset(data, subset = data$Plot_No=="31")
dim(Collar31)

plot(Collar31$Rec_No, Collar31$CO2)

Collar31_short <- Collar31[c(8:60), ]
plot(Collar31_short$Rec_No, Collar31_short$CO2)
Collar31_short[,"Rec_No"]
Collar31_short$Rec_No


##### n = RT/PV
n = ((Collar31$Pressure[1]/1000)*2)/((0.08314)*(Collar31$Tair[1]+273.15))
Collar31_short$CO2_umol_m2 <- (Collar31_short$CO2*n/0.008)

#plot(x,y)
plot(Collar31_short$Rec_No,Collar31_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 31")
#lm(y~x)
fit1<-lm(Collar31_short$CO2_umol_m2~Collar31_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_31<-(summary(lm(Collar31_short$CO2_umol_m2~Collar31_short$Rec_No))$coefficients[2])
R31<-summary(lm(Collar31_short$CO2_umol_m2~Collar31_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 32
Collar32<-subset(data, subset = data$Plot_No=="32")
dim(Collar32)

plot(Collar32$Rec_No, Collar32$CO2)

Collar32_short <- Collar32[c(8:60), ]
plot(Collar32_short$Rec_No, Collar32_short$CO2)
Collar32_short[,"Rec_No"]
Collar32_short$Rec_No


##### n = RT/PV
n = ((Collar32$Pressure[1]/1000)*2)/((0.08314)*(Collar32$Tair[1]+273.15))
Collar32_short$CO2_umol_m2 <- (Collar32_short$CO2*n/0.008)

#plot(x,y)
plot(Collar32_short$Rec_No,Collar32_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 32")
#lm(y~x)
fit1<-lm(Collar32_short$CO2_umol_m2~Collar32_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_32<-(summary(lm(Collar32_short$CO2_umol_m2~Collar32_short$Rec_No))$coefficients[2])
R32<-summary(lm(Collar32_short$CO2_umol_m2~Collar32_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 33
Collar33<-subset(data, subset = data$Plot_No=="33")
dim(Collar33)

plot(Collar33$Rec_No, Collar33$CO2)

Collar33_short <- Collar33[c(8:60), ]
plot(Collar33_short$Rec_No, Collar33_short$CO2)
Collar33_short[,"Rec_No"]
Collar33_short$Rec_No


##### n = RT/PV
n = ((Collar33$Pressure[1]/1000)*2)/((0.08314)*(Collar33$Tair[1]+273.15))
Collar33_short$CO2_umol_m2 <- (Collar33_short$CO2*n/0.008)

#plot(x,y)
plot(Collar33_short$Rec_No,Collar33_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 33")
#lm(y~x)
fit1<-lm(Collar33_short$CO2_umol_m2~Collar33_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_33<-(summary(lm(Collar33_short$CO2_umol_m2~Collar33_short$Rec_No))$coefficients[2])
R33<-summary(lm(Collar33_short$CO2_umol_m2~Collar33_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 34
Collar34<-subset(data, subset = data$Plot_No=="34")
dim(Collar34)

plot(Collar34$Rec_No, Collar34$CO2)

Collar34_short <- Collar34[c(8:60), ]
plot(Collar34_short$Rec_No, Collar34_short$CO2)
Collar34_short[,"Rec_No"]
Collar34_short$Rec_No


##### n = RT/PV
n = ((Collar34$Pressure[1]/1000)*2)/((0.08314)*(Collar34$Tair[1]+273.15))
Collar34_short$CO2_umol_m2 <- (Collar34_short$CO2*n/0.008)

#plot(x,y)
plot(Collar34_short$Rec_No,Collar34_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 34")
#lm(y~x)
fit1<-lm(Collar34_short$CO2_umol_m2~Collar34_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_34<-(summary(lm(Collar34_short$CO2_umol_m2~Collar34_short$Rec_No))$coefficients[2])
R34<-summary(lm(Collar34_short$CO2_umol_m2~Collar34_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 35
Collar35<-subset(data, subset = data$Plot_No=="35")
dim(Collar35)

plot(Collar35$Rec_No, Collar35$CO2)

Collar35_short <- Collar35[c(8:60), ]
plot(Collar35_short$Rec_No, Collar35_short$CO2)
Collar35_short[,"Rec_No"]
Collar35_short$Rec_No


##### n = RT/PV
n = ((Collar35$Pressure[1]/1000)*2)/((0.08314)*(Collar35$Tair[1]+273.15))
Collar35_short$CO2_umol_m2 <- (Collar35_short$CO2*n/0.008)

#plot(x,y)
plot(Collar35_short$Rec_No,Collar35_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 35")
#lm(y~x)
fit1<-lm(Collar35_short$CO2_umol_m2~Collar35_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_35<-(summary(lm(Collar35_short$CO2_umol_m2~Collar35_short$Rec_No))$coefficients[2])
R35<-summary(lm(Collar35_short$CO2_umol_m2~Collar35_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 36
Collar36<-subset(data, subset = data$Plot_No=="36")
dim(Collar36)

plot(Collar36$Rec_No, Collar36$CO2)

Collar36_short <- Collar36[c(8:60), ]
plot(Collar36_short$Rec_No, Collar36_short$CO2)
Collar36_short[,"Rec_No"]
Collar36_short$Rec_No


##### n = RT/PV
n = ((Collar36$Pressure[1]/1000)*2)/((0.08314)*(Collar36$Tair[1]+273.15))
Collar36_short$CO2_umol_m2 <- (Collar36_short$CO2*n/0.008)

#plot(x,y)
plot(Collar36_short$Rec_No,Collar36_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 36")
#lm(y~x)
fit1<-lm(Collar36_short$CO2_umol_m2~Collar36_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_36<-(summary(lm(Collar36_short$CO2_umol_m2~Collar36_short$Rec_No))$coefficients[2])
R36<-summary(lm(Collar36_short$CO2_umol_m2~Collar36_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 37
Collar37<-subset(data, subset = data$Plot_No=="37")
dim(Collar37)

plot(Collar37$Rec_No, Collar37$CO2)

Collar37_short <- Collar37[c(8:60), ]
plot(Collar37_short$Rec_No, Collar37_short$CO2)
Collar37_short[,"Rec_No"]
Collar37_short$Rec_No


##### n = RT/PV
n = ((Collar37$Pressure[1]/1000)*2)/((0.08314)*(Collar37$Tair[1]+273.15))
Collar37_short$CO2_umol_m2 <- (Collar37_short$CO2*n/0.008)

#plot(x,y)
plot(Collar37_short$Rec_No,Collar37_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 37")
#lm(y~x)
fit1<-lm(Collar37_short$CO2_umol_m2~Collar37_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_37<-(summary(lm(Collar37_short$CO2_umol_m2~Collar37_short$Rec_No))$coefficients[2])
R37<-summary(lm(Collar37_short$CO2_umol_m2~Collar37_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 38
Collar38<-subset(data, subset = data$Plot_No=="38")
dim(Collar38)

plot(Collar38$Rec_No, Collar38$CO2)

Collar38_short <- Collar38[c(8:60), ]
plot(Collar38_short$Rec_No, Collar38_short$CO2)
Collar38_short[,"Rec_No"]
Collar38_short$Rec_No


##### n = RT/PV
n = ((Collar38$Pressure[1]/1000)*2)/((0.08314)*(Collar38$Tair[1]+273.15))
Collar38_short$CO2_umol_m2 <- (Collar38_short$CO2*n/0.008)

#plot(x,y)
plot(Collar38_short$Rec_No,Collar38_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 38")
#lm(y~x)
fit1<-lm(Collar38_short$CO2_umol_m2~Collar38_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_38<-(summary(lm(Collar38_short$CO2_umol_m2~Collar38_short$Rec_No))$coefficients[2])
R38<-summary(lm(Collar38_short$CO2_umol_m2~Collar38_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 39
Collar39<-subset(data, subset = data$Plot_No=="39")
dim(Collar39)

plot(Collar39$Rec_No, Collar39$CO2)

Collar39_short <- Collar39[c(8:60), ]
plot(Collar39_short$Rec_No, Collar39_short$CO2)
Collar39_short[,"Rec_No"]
Collar39_short$Rec_No


##### n = RT/PV
n = ((Collar39$Pressure[1]/1000)*2)/((0.08314)*(Collar39$Tair[1]+273.15))
Collar39_short$CO2_umol_m2 <- (Collar39_short$CO2*n/0.008)

#plot(x,y)
plot(Collar39_short$Rec_No,Collar39_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 39")
#lm(y~x)
fit1<-lm(Collar39_short$CO2_umol_m2~Collar39_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_39<-(summary(lm(Collar39_short$CO2_umol_m2~Collar39_short$Rec_No))$coefficients[2])
R39<-summary(lm(Collar39_short$CO2_umol_m2~Collar39_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 40
Collar40<-subset(data, subset = data$Plot_No=="40")
dim(Collar40)

plot(Collar40$Rec_No, Collar40$CO2)

Collar40_short <- Collar40[c(8:60), ]
plot(Collar40_short$Rec_No, Collar40_short$CO2)
Collar40_short[,"Rec_No"]
Collar40_short$Rec_No


##### n = RT/PV
n = ((Collar40$Pressure[1]/1000)*2)/((0.08314)*(Collar40$Tair[1]+273.15))
Collar40_short$CO2_umol_m2 <- (Collar40_short$CO2*n/0.008)

#plot(x,y)
plot(Collar40_short$Rec_No,Collar40_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 40")
#lm(y~x)
fit1<-lm(Collar40_short$CO2_umol_m2~Collar40_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_40<-(summary(lm(Collar40_short$CO2_umol_m2~Collar40_short$Rec_No))$coefficients[2])
R40<-summary(lm(Collar40_short$CO2_umol_m2~Collar40_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 41
Collar41<-subset(data, subset = data$Plot_No=="41")
dim(Collar41)

plot(Collar41$Rec_No, Collar41$CO2)

Collar41_short <- Collar41[c(8:60), ]
plot(Collar41_short$Rec_No, Collar41_short$CO2)
Collar41_short[,"Rec_No"]
Collar41_short$Rec_No


##### n = RT/PV
n = ((Collar41$Pressure[1]/1000)*2)/((0.08314)*(Collar41$Tair[1]+273.15))
Collar41_short$CO2_umol_m2 <- (Collar41_short$CO2*n/0.008)

#plot(x,y)
plot(Collar41_short$Rec_No,Collar41_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 41")
#lm(y~x)
fit1<-lm(Collar41_short$CO2_umol_m2~Collar41_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_41<-(summary(lm(Collar41_short$CO2_umol_m2~Collar41_short$Rec_No))$coefficients[2])
R41<-summary(lm(Collar41_short$CO2_umol_m2~Collar41_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 42
Collar42<-subset(data, subset = data$Plot_No=="42")
dim(Collar42)

plot(Collar42$Rec_No, Collar42$CO2)

Collar42_short <- Collar42[c(8:60), ]
plot(Collar42_short$Rec_No, Collar42_short$CO2)
Collar42_short[,"Rec_No"]
Collar42_short$Rec_No


##### n = RT/PV
n = ((Collar42$Pressure[1]/1000)*2)/((0.08314)*(Collar42$Tair[1]+273.15))
Collar42_short$CO2_umol_m2 <- (Collar42_short$CO2*n/0.008)

#plot(x,y)
plot(Collar42_short$Rec_No,Collar42_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 42")
#lm(y~x)
fit1<-lm(Collar42_short$CO2_umol_m2~Collar42_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_42<-(summary(lm(Collar42_short$CO2_umol_m2~Collar42_short$Rec_No))$coefficients[2])
R42<-summary(lm(Collar42_short$CO2_umol_m2~Collar42_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 43
Collar43<-subset(data, subset = data$Plot_No=="43")
dim(Collar43)

plot(Collar43$Rec_No, Collar43$CO2)

Collar43_short <- Collar43[c(8:60), ]
plot(Collar43_short$Rec_No, Collar43_short$CO2)
Collar43_short[,"Rec_No"]
Collar43_short$Rec_No


##### n = RT/PV
n = ((Collar43$Pressure[1]/1000)*2)/((0.08314)*(Collar43$Tair[1]+273.15))
Collar43_short$CO2_umol_m2 <- (Collar43_short$CO2*n/0.008)

#plot(x,y)
plot(Collar43_short$Rec_No,Collar43_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 43")
#lm(y~x)
fit1<-lm(Collar43_short$CO2_umol_m2~Collar43_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_43<-(summary(lm(Collar43_short$CO2_umol_m2~Collar43_short$Rec_No))$coefficients[2])
R43<-summary(lm(Collar43_short$CO2_umol_m2~Collar43_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 44
Collar44<-subset(data, subset = data$Plot_No=="44")
dim(Collar44)

plot(Collar44$Rec_No, Collar44$CO2)

Collar44_short <- Collar44[c(8:60), ]
plot(Collar44_short$Rec_No, Collar44_short$CO2)
Collar44_short[,"Rec_No"]
Collar44_short$Rec_No


##### n = RT/PV
n = ((Collar44$Pressure[1]/1000)*2)/((0.08314)*(Collar44$Tair[1]+273.15))
Collar44_short$CO2_umol_m2 <- (Collar44_short$CO2*n/0.008)

#plot(x,y)
plot(Collar44_short$Rec_No,Collar44_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 44")
#lm(y~x)
fit1<-lm(Collar44_short$CO2_umol_m2~Collar44_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_44<-(summary(lm(Collar44_short$CO2_umol_m2~Collar44_short$Rec_No))$coefficients[2])
R44<-summary(lm(Collar44_short$CO2_umol_m2~Collar44_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 45
Collar45<-subset(data, subset = data$Plot_No=="45")
dim(Collar45)

plot(Collar45$Rec_No, Collar45$CO2)

Collar45_short <- Collar45[c(8:60), ]
plot(Collar45_short$Rec_No, Collar45_short$CO2)
Collar45_short[,"Rec_No"]
Collar45_short$Rec_No


##### n = RT/PV
n = ((Collar45$Pressure[1]/1000)*2)/((0.08314)*(Collar45$Tair[1]+273.15))
Collar45_short$CO2_umol_m2 <- (Collar45_short$CO2*n/0.008)

#plot(x,y)
plot(Collar45_short$Rec_No,Collar45_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 45")
#lm(y~x)
fit1<-lm(Collar45_short$CO2_umol_m2~Collar45_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_45<-(summary(lm(Collar45_short$CO2_umol_m2~Collar45_short$Rec_No))$coefficients[2])
R45<-summary(lm(Collar45_short$CO2_umol_m2~Collar45_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 46
Collar46<-subset(data, subset = data$Plot_No=="46")
dim(Collar46)

plot(Collar46$Rec_No, Collar46$CO2)

Collar46_short <- Collar46[c(8:60), ]
plot(Collar46_short$Rec_No, Collar46_short$CO2)
Collar46_short[,"Rec_No"]
Collar46_short$Rec_No


##### n = RT/PV
n = ((Collar46$Pressure[1]/1000)*2)/((0.08314)*(Collar46$Tair[1]+273.15))
Collar46_short$CO2_umol_m2 <- (Collar46_short$CO2*n/0.008)

#plot(x,y)
plot(Collar46_short$Rec_No,Collar46_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 46")
#lm(y~x)
fit1<-lm(Collar46_short$CO2_umol_m2~Collar46_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_46<-(summary(lm(Collar46_short$CO2_umol_m2~Collar46_short$Rec_No))$coefficients[2])
R46<-summary(lm(Collar46_short$CO2_umol_m2~Collar46_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 47
Collar47<-subset(data, subset = data$Plot_No=="47")
dim(Collar47)

plot(Collar47$Rec_No, Collar47$CO2)

Collar47_short <- Collar47[c(8:60), ]
plot(Collar47_short$Rec_No, Collar47_short$CO2)
Collar47_short[,"Rec_No"]
Collar47_short$Rec_No


##### n = RT/PV
n = ((Collar47$Pressure[1]/1000)*2)/((0.08314)*(Collar47$Tair[1]+273.15))
Collar47_short$CO2_umol_m2 <- (Collar47_short$CO2*n/0.008)

#plot(x,y)
plot(Collar47_short$Rec_No,Collar47_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 47")
#lm(y~x)
fit1<-lm(Collar47_short$CO2_umol_m2~Collar47_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_47<-(summary(lm(Collar47_short$CO2_umol_m2~Collar47_short$Rec_No))$coefficients[2])
R47<-summary(lm(Collar47_short$CO2_umol_m2~Collar47_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 48
Collar48<-subset(data, subset = data$Plot_No=="48")
dim(Collar48)

plot(Collar48$Rec_No, Collar48$CO2)

Collar48_short <- Collar48[c(8:60), ]
plot(Collar48_short$Rec_No, Collar48_short$CO2)
Collar48_short[,"Rec_No"]
Collar48_short$Rec_No


##### n = RT/PV
n = ((Collar48$Pressure[1]/1000)*2)/((0.08314)*(Collar48$Tair[1]+273.15))
Collar48_short$CO2_umol_m2 <- (Collar48_short$CO2*n/0.008)

#plot(x,y)
plot(Collar48_short$Rec_No,Collar48_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 48")
#lm(y~x)
fit1<-lm(Collar48_short$CO2_umol_m2~Collar48_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_48<-(summary(lm(Collar48_short$CO2_umol_m2~Collar48_short$Rec_No))$coefficients[2])
R48<-summary(lm(Collar48_short$CO2_umol_m2~Collar48_short$Rec_No))$adj.r.squared












