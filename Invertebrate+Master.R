setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")
head(data)
dim(data)
class(data)
library(vegan)


##Earthworm Relative Abundance
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
EarthwormRelAb<-data["Lumbricidae",]/colSums(data, na.rm=T)
EarthwormRelAb<-as.matrix(EarthwormRelAb)
EarthwormRelAb[is.nan(EarthwormRelAb)]<-NA
EarthwormRelAb<-t(as.vector(EarthwormRelAb))

#Shannon
head(data)
dim(data)
t_data<-t(data)
t_data<-t_data[,c(1:20)]
head(t_data)

shannon<- diversity(t_data[,], index="shannon", MARGIN = 1, base = exp(1))
shannon
max(shannon)
barplot(shannon[1:48], ylim=c(0,2), xlab="Plots 1-48", ylab="H'", xaxt='n',
        main="Shannon Index per Plot", mgp = c(2, 1, 0), col=c(rep("lightcoral", 24), rep("palegreen3",24)))
legend("topleft", c("Lower Site", "Upper Site"), col=c("lightcoral", "palegreen3"), 
       pch=c(16,20), bty="n")

#Simpson
simpson<- diversity(t_data[,], index="simpson", MARGIN = 1, base = exp(1))
colnames(t_data)
max(simpson)
barplot(simpson, ylim=c(0,1), xlab="Plots 1-48", ylab="D", 
        main="Simpson Index per Plot", xaxt='n', mgp = c(2, 1, 0), col=c(rep("lightcoral", 24), rep("palegreen3",24)))
legend("topleft", c("Lower Site", "Upper Site"), col=c("lightcoral", "palegreen3"), 
       pch=c(16,20), bty="n")

means<-tapply(data$MicromolCO2.hr.g.soil.C, data$Site, mean, na.rm=T)
means<-c(means[1], means[3], means[2], means[4])
se<-tapply(data$MicromolCO2.hr.g.soil.C, data$Site_Date, sd, na.rm=T)/sqrt(tapply(data$MicromolCO2.hr.g.soil.C, data$Site_Date, nnzero, na.counted=F))
se<-c(se[1], se[3], se[2], se[4])
upper<-means+se
lower<-means-se

xs<-c(1,2,3,4)
plot(xs, as.vector(means), ylab = "Mean Micromol CO2/hr/g soil C", ylim=c(min(lower-10), max(upper+10)), xaxt="n", xlab="Incubation Day", xlim=c(0.5,4.5), col=c("red", "blue", "red", "blue"), pch=16)
arrows(xs, lower, xs, upper, angle=90, length=0.1, lwd=3,col=c("red", "blue", "red", "blue"), code=3 )
abline(v=2.5, lwd=3, lty=2, col="grey")
legend("topright", c("Lower", "Upper"), col=c("red", "blue"), pch=16, bty="n")
axis(1, las=1, at=c(1.5, 3.5), labels = c("Day 1", "Day 7"))

#Plot
index<-rbind(simpson,shannon)
barplot(index,beside=T, ylim=c(0,2), xlab="Plots 1-48", 
        main="Simpson and Shannon Indices per Plot", xaxt='n', mgp = c(1, 1, 0),
 col=c("honeydew2", "lightblue2"))
        
legend("topright", c("Simpson", "Shannon"), col=c("honeydew2", "lightblue2"), 
       pch=c(16,16), bty="n")

#Richness
S <- specnumber(t_data)
max(S)
barplot(S, ylim=c(0,6), xaxt='n', mgp = c(2.3, 1, 0), xlab="Plots 1-48", ylab="S", main="Species Richness per Plot", 
        col=c(rep("lightcoral", 24), rep("palegreen3",24)))
legend("topleft", c("Lower Site", "Upper Site"), col=c("lightcoral", "palegreen3"), 
       pch=c(16,20), bty="n")

#Evenness 
H<- diversity(t_data[, -(1:1)])
J <- H/log(S)
barplot(J, ylim=c(0,1), xaxt='n', mgp = c(2.3, 1, 0), 
        xlab="Plots 1-48", ylab="J", main="Species Evenness per Plot", col=c(rep("lightcoral", 24), rep("palegreen3",24)))
legend("bottomright", c("Lower Site", "Upper Site"), col=c("lightcoral", "palegreen3"), 
       pch=c(16,20), bty="n")

###################### Names
##names.arg=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18 "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
            #"31", "32", "33", "34", "35", "36", "37", "38", "39", "40","41",
            #"42", "43", "44", "45", "46","47","48")

################SMASH MASTER
setwd("~/Desktop/Summer_Science_Gill_2021")
master_data<- read.csv("Summer2021MasterData.csv")
master_data<-master_data[1:48,]
dim(master_data)
length(EarthwormRelAb)
master_data$EarthwormRelAb<-as.vector(EarthwormRelAb)
master_data$shannon<-shannon
master_data$simpson<-simpson
master_data$S<-S
master_data$J<-J
colnames(master_data)

colnames(data)
master_data$Site<-c(rep("Lower", 24), rep("Upper", 24))
means<-tapply(master_data$simpson, master_data$Site, mean, na.rm=T)
se<-tapply(master_data$simpson, master_data$Site, sd, na.rm=T)/sqrt(tapply(master_data$simpson, master_data$Site, nnzero, na.counted=F))
upper<-means+se
lower<-means-se

xs<-c(1,2)
plot(xs, as.vector(means), ylab = "Simpson Index", ylim=c(min(lower-.10), max(upper+.10)), xaxt="n", xlab="Site", xlim=c(0.5,2.5), col=c("red", "blue"), pch=16)
arrows(xs, lower, xs, upper, angle=90, length=0.1, lwd=3,col=c("red", "blue"), code=3 )
# abline(v=2.5, lwd=3, lty=2, col="grey")
legend("topright", c("Lower", "Upper"), col=c("red", "blue"), pch=16, bty="n")
legend("topleft", "p = 0.4", bty="n")
axis(1, las=1, at=c(1, 2), labels = c("Lower", "Upper"))
fit1<-lm(master_data$simpson~master_data$Site, na.rm=T)
summary(fit1)


#####Earthworm Rel Ab
barplot(master_data$EarthwormRelAb, xaxt="n", ylab = "Relative Abundance", xlab="Plots 1-48", xaxt='n', mgp = c(2, 1, 0), 
        col=c(rep("red", 24), rep("blue",24)))
legend("topright", c("Lower Site", "Upper Site"), col=c("red", "blue"), 
       pch=c(16,20), bty="n")

plot(log(master_data$CaConcentration), log(master_data$EarthwormRelAb+1), 
     xlab="Log ([Soil Ca] (ppm))", ylab="Log (Rel. Abundance Earthworms)")
fit1<-lm(log(master_data$EarthwormRelAb+1)~log(master_data$CaConcentration))
abline(fit1, col="red")
summary(fit1)
legend("topleft", "p = 0.048", bty="n")


hist(log(master_data$CaConcentration))
hist(log(master_data$EarthwormRelAb))

head(data)
data_t<-as.data.frame(t(data))
colnames(data_t)
data_t$Lumbricidae
plot(master_data$CaConcentration,data_t$Lumbricidae)
fit1<-lm(data_t$Lumbricidae~master_data$CaConcentration)
abline(fit1, col="red")
summary(fit1)

barplot(master_data$CaConcentration)
points(master_data$EarthwormRelAb, col="red")

barplot(master_data$KConcentration)
points(data_t$Lumbricidae, col="red")

barplot(master_data$NaConcentration)
points(data_t$Lumbricidae, col="red")

barplot(master_data$MgConcentration)
points(master_data$EarthwormRelAb, col="red")

######################
plot(master_data$p_soil_C, master_data$simpson, xlab = "% Soil C", ylab = "H'", 
     main = "Simpson Diversity Index vs. % Soil Carbon")
fit1<-lm(master_data$shannon~master_data$p_soil_C)
abline(fit1, col="red")
summary(fit1)

barplot(master_data$simpson, ylim=c(0,1))
points(master_data$g_soil_C)

plot(master_data$NI_MicromolCO2.hr, master_data$EarthwormRelAb)
fit1<-lm(master_data$EarthwormRelAb~master_data$NI_MicromolCO2.hr)
abline(fit1, col="red")
summary(fit1)

plot(master_data$MicromolCO2.hr.g.soil.C, master_data$shannon, 
     xlab = "Micromol CO2/hr/g soil C", ylab = "H'", main = "Shannon Diversity Index vs. Soil C Respiration")
fit1<-lm(master_data$shannon~master_data$MicromolCO2.hr.g.soil.C)
abline(fit1, col="red")
summary(fit1)

plot(master_data$MicromolCO2.hr.g.soil.C, master_data$simpson, 
     xlab = "Micromol CO2/hr/g soil C", ylab = "D", main = "Simpson Diversity Index vs. Soil C Respiration")
fit1<-lm(master_data$simpson~master_data$MicromolCO2.hr.g.soil.C)
abline(fit1, col="red")
summary(fit1)

plot(master_data$MicromolCO2.hr.g.soil.C, master_data$J, 
     xlab = "Micromol CO2/hr/g soil C", ylab = "J", main = "Species Evenness vs. Soil C Respiration")
fit1<-lm(master_data$J~master_data$MicromolCO2.hr.g.soil.C)
abline(fit1, col="red")
summary(fit1)

plot(master_data$MicromolCO2.hr.g.soil.C, master_data$S, 
     xlab = "Micromol CO2/hr/g soil C", ylab = "S", main = "Species Richness vs. Soil C Respiration")
fit1<-lm(master_data$S~master_data$MicromolCO2.hr.g.soil.C)
abline(fit1, col="red")
summary(fit1)

plot(master_data$p_soil_C, master_data$J, 
     xlab = "% Soil C", ylab = "J", main = "Species Evenness vs. % Soil C")
fit1<-lm(master_data$J~master_data$p_soil_C)
abline(fit1, col="red")
summary(fit1)

plot(master_data$p_soil_C, master_data$simpson, 
     xlab = "% Soil C", ylab = "D", main = "Simpson Diversity Index vs. % Soil C")
fit1<-lm(master_data$simpson~master_data$p_soil_C)
abline(fit1, col="red")
summary(fit1)

plot(master_data$p_soil_C, master_data$S, 
     xlab = "% Soil C", ylab = "S", main = "Species Richness vs. % Soil C")
fit1<-lm(master_data$S~master_data$p_soil_C)
abline(fit1, col="red")
summary(fit1)


plot(master_data$MicromolCO2.hr.g.soil.C, master_data$shannon, 
     xlab = "MicromolCO2/hr/g soil C", ylab = "H'", main = "Shannon Diversity Index vs. Soil C Respiration")
fit1<-lm(master_data$shannon~master_data$MicromolCO2.hr.g.soil.C)
abline(fit1, col="red")
summary(fit1)

plot(master_data$Plot, master_data$MicromolCO2.hr.g.soil.C)
fit1<-lm(master_data$Plot~master_data$MicromolCO2.hr.g.soil.C)
abline(fit1, col="red")
summary(fit1)

plot(master_data$p_soil_C, master_data$MicromolCO2_hr_gdrysoil, xlab = "% Soil C", ylab = "Micromol CO2/hr/g dry soil",
     main = "Soil Respiration vs. % Soil Carbon")
fit1<-lm(master_data$MicromolCO2_hr_gdrysoil~master_data$p_soil_C)
abline(fit1, col="red")
summary(fit1)

##Respiration across plots, with and without

barplot(master_data$NI_MicromolCO2.hr.g.soil.C, ylim=c(0,80), xlab = "Plots 1-48",
        ylab = "Micromol CO2/hr/g soil C", col=c(rep("red", 24), rep("blue",24)), mgp = c(2.5, 1, 0))
legend("topright", c("Lower Site", "Upper Site"), col=c("red", "blue"), 
       pch=c(16,20), bty="n")

barplot(master_data$WI_MicromolCO2.hr.g.soil.C, ylim=c(0,600), xlab = "Plots 1-48",
        ylab = "Micromol CO2/hr/g soil C", main = "Soil C Respiration With Invertebrates Across Plots", 
        col=c(rep("lightcoral", 24), rep("palegreen3",24)), mgp = c(2.5, 1, 0))
legend("topright", c("Lower Site", "Upper Site"), col=c("lightcoral", "palegreen3"), 
       pch=c(16,20), bty="n")

index<-rbind(master_data$WI_MicromolCO2.hr.g.soil.C,master_data$NI_MicromolCO2.hr.g.soil.C)
barplot(index,beside=T, ylim=c(0,600), xlab="Plots 1-48", ylab = "Micromol CO2/hr/g soil C", 
        xaxt='n', mgp = c(2.5, 1, 0), col=c("blue", "red"))
legend("topright", c("With Invertebrates", "Without Invertebrates"), col=c("blue", "red"), 
       pch=c(16,20), bty="n")

#Evenness/richness ~ diversity 

plot(master_data$S,master_data$shannon, xlab = "Species Richness", 
     ylab = "H'", main = "Shannon Diversity Index Increases with Species Richness")
fit1<-lm(master_data$shannon~master_data$S)
abline(fit1, col="red")
summary(fit1)

plot(master_data$S,master_data$simpson, xlab = "Species Richness", 
     ylab = "D", main = "Simpson Diversity Index Increases with Species Richness")
fit1<-lm(master_data$shannon~master_data$S)
abline(fit1, col="red")
summary(fit1)

plot(master_data$J,master_data$shannon, xlab = "Species Evenness", 
     ylab = "H'", main = "Shannon Diversity Index Increases with Species Evenness")
fit1<-lm(master_data$shannon~master_data$S)
abline(fit1, col="red")
summary(fit1)

plot(master_data$J,master_data$simpson, xlab = "Species Evenness", 
     ylab = "D", main = "Simpson Diversity Index Increases with Species Richness")
fit1<-lm(master_data$shannon~master_data$S)
abline(fit1, col="red")
summary(fit1)


###P soil C across plots

barplot(master_data$p_soil_C, ylim=c(0,30), xlab = "Plots 1-48",
        ylab = "% soil C", 
        col=c(rep("red", 24), rep("blue",24)), mgp = c(2.5, 1, 0))
legend("topright", c("Lower Site", "Upper Site"), col=c("red", "blue"), 
       pch=c(16,20), bty="n")

