setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates")
data<- read.csv("Invertebrate collection.csv")
# install.packages('Matrix)
# install.packages('dplyr')
install.packages('vegan')

library(vegan)
library(Matrix)
library(dplyr)
head(data)
length(data$Plot)
colnames(data)

as.data.frame(data %>% count(Plot))
as.data.frame(data %>% count(Plot))

sum.data<-data %>% group_by(Earthworm) %>% count(Plot)
sum.data<-as.data.frame(sum.data)

Upper<-subset(data, subset = data$U_L=="U")
dim(Upper)

Lower<-subset(data, subset = data$U_L=="L")
dim(Lower)

unique(data$Sample.Type)
length(unique(data$Sample.Type))
template1<- data.frame(array("Upper",c(length(unique(data$Sample.Type)),1)))
names(template1) <- c('Plot')
template1$Sample.Type <- c('Aranedia', 'Coleoptera', 'Collembola', 'Diptera', 
                           'Lumbricidae', 'Nematoda', 'Opiliones', 'Polyxenida',
                           'Polyzoniida')
template2<- data.frame(array("Lower",c(length(unique(data$Sample.Type)),1)))
names(template2) <- c('Plot')
template2$Sample.Type <- c('Aranedia', 'Coleoptera', 'Collembola', 'Diptera', 
                           'Lumbricidae', 'Nematoda', 'Opiliones', 'Polyxenida',
                           'Polyzoniida')
template<-rbind(template1, template2)

sum.data <- merge(template,sum.data,by.x ="Plot",by.y="Sample.Type",all.x=TRUE)
sum.data

barplot(tapply(data$Sample.Type, data$U_L, mean, na.rm=T), ylim=c(0,15))

#### Diversity
setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates")
data<- read.csv("invert2.csv")

#Shannon
shannon<- diversity(data[, -(1:1)], index="shannon", MARGIN = 1, base = exp(1))
max(shannon)
barplot(shannon[1:13], ylim=c(0,2), xlab="Plot #", ylab="H'", 
        main="Shannon Index per Plot", names.arg=c("1", "2", "3", "4", "5",
                                                   "6", "9", "14", "30","38", "41", 
                                                   "45", "47"), col="darkslateblue")
length(shannon)

#Simpson
simpson<- diversity(data[, -(1:1)], index="simpson", MARGIN = 1, base = exp(1))
max(simpson)
barplot(simpson, ylim=c(0,1), xlab="Plot #", ylab="D", main="Simpson Index per Plot",
        names.arg=c("1", "2", "3", "4", "5", "6", "9", "14", "30","38", "41", 
  "45", "47", "X"), col="lavenderblush")

#Plot
index<-rbind(simpson,shannon)
barplot(index,beside=T, ylim=c(0,2), xlab="Plot", main="Simpson and Shannon Indices per Plot", 
        col=c("honeydew2", "lightblue2"), names.arg=c("1", "2", "3", "4", "5", 
                                                "6", "9", "14", "30","38", "41", 
                                                "45", "47", "X"))
legend("topright", c("Simpson", "Shannon"), col=c("honeydew2", "lightblue2"), 
       pch=c(16,16), bty="n")

#Richness
S <- specnumber(data)
barplot(S, ylim=c(0,8), xlab="Plot #", ylab="S", main="Species Richness per Plot",
        names.arg=c("1", "2", "3", "4", "5",
                    "6", "9", "14", "30","38", "41", 
                    "45", "47", "X"), col="honeydew")

#Evenness 
H<- diversity(data[, -(1:1)])
J <- H/log(S)
barplot(J, ylim=c(0,1), xlab="Plot #", ylab="J", main="Species Evenness per Plot",
        names.arg=c("1", "2", "3", "4", "5",
                    "6", "9", "14", "30","38", "41", 
                    "45", "47", "X"), col="lightpink2")

