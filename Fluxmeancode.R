
slopes<-c(slope_1,slope_2,slope_3,slope_4,slope_5,slope_7,slope_8,slope_9,
          slope_11,slope_12,slope_13,slope_14,slope_15,slope_16,slope_17,slope_18,slope_19,
          slope_20,slope_21,slope_22,slope_23,slope_24,slope_25,slope_26,slope_27,
          slope_28,slope_29,slope_30,slope_31,slope_32,slope_33,slope_34,slope_35,
          slope_36,slope_37,slope_38,slope_39,slope_40,slope_41,slope_42,slope_43,slope_44,
          slope_45,slope_46,slope_47,slope_48)
R2<-c(R1,R2,R3,R4,R5,R7,R8,R9,
      R11,R12,R13,R14,R15,R16,R17,R18,R19,
      R20,R21,R22,R23,R24,R25,R26,R27,
      R28,R29,R30,R31,R32,R33,R34,R35,
      R36,R37,R38,R39,R40,R41,R42,R43,R44,
      R45,R46,R47,R48)

collars<-c("collar1", "collar2", "collar3", "collar4", "collar5",  
           "collar7", "collar8", "collar9","collar11", "collar12",
           "collar13", "collar14", "collar15", "collar16", "collar17", "collar18", "collar19",
           "collar20", "collar21", "collar22", "collar23", "collar24", 
           "collar25", "collar26", "collar27","collar28", "collar29", "collar30", "collar31", "collar32", "collar33", "collar34", 
           "collar35", "collar36", "collar37", "collar38", "collar39",
           "collar40", "collar41", "collar42", "collar43", "collar44", "collar45",
           "collar46", "collar47", "collar48")

CollarNumber<-c(1,2,3,4,5,7,8,9,11,12,13,
                14,15,16,17,18,19,20,21,22,23,24,
                25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48)
###Replicate<-rep(c(1,2),34)


new.data<-as.matrix(cbind(collars, slopes, R2, CollarNumber))
write.csv(new.data,"21071510_SummarySoilRespData.csv")

flux<-read.csv("21071510_SummarySoilRespData.csv")
head(flux)
flux.mean<-tapply(flux$slopes, flux$CollarNumber, mean, na.rm=T)
flux.mean<-as.data.frame(as.matrix(flux.mean))
flux.mean$CollarNumber<-c(1:46)
colnames(flux.mean)<-c("Flux_mean", "CollarNumber")

write.csv(flux.mean,"21071510_FluxMeasurement3_SummaryRespirationData_Means.csv")
