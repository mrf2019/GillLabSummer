##### Script to process soil respiration fluxes
setwd("~/Desktop/Summer_Science_Gill_2021/Flux data") # Update to match individual computer directories

install.packages("lubridate")  # need to run this line once to install lubridate R package. After you run it once, you can just use the library command
library(lubridate)

filenames <- list.files("Flux measurement 4", pattern="*.csv", full.names=TRUE) 
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

data <- read.csv("21071510_SoilRespData.csv")
dim(data)
head(data)
colnames(data)
