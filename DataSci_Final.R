setwd("/path/to/Thruster Data/train")
install.packages("lubridate")
library("lubridate")
library("dplyr")
library("plyr")
library("readr")

data_all <- list.files(path = "/path/to/Thruster Data/train", 
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              
  bind_rows                                        
data_all    

randIndex <- sample(1:dim(data_all)[1])
cutpoint <- floor(dim(data_all)[1]*1/10)
cutData <- (data_all[randIndex[1:cutpoint],])
cutData$time <- unclass(cutData$time)

time <- cutData$time[ !cutData$ton == '0']
ton <- cutData$ton[ !cutData$ton == '0']
thrust <- cutData$thrust[ !cutData$ton == '0']
mfr <- cutData$mfr[ !cutData$ton == '0']
vl <- cutData$vl[ !cutData$ton == '0']
anomaly_code <- cutData$anomaly_code[ !cutData$ton == '0']

newData <- data.frame(time, ton, thrust, mfr, vl, anomaly_code)

plot(newData$time, newData$thrust, main = "Time vs Thrust", xlab = "Time", ylab = "Thrust")
#Time does not work the way we want it to. It is based on how many seconds since Jan 1st 1970 rather than how long since the thrusters have been activated. 
#We If there was a way to go csv file by csv file and change the time to 0 when the thrusters turn on we would but that it is like basically impossible
#Instead we are going to just go csv file by csv file  for like 5 of them and graph time to thrust, get a line of best fit then average them out if possible

`SN01_21bars_offmod` <- read.csv("~/Dataset for Robertson/dataset/train/00027_027_SN01_21bars_offmod.csv")
`SN03_5bars_ssf` <- read.csv("~/Dataset for Robertson/dataset/train/00316_092_SN03_5bars_ssf.csv")
`SN01_18bars_ssf` <- read.csv("~/Dataset for Robertson/dataset/train/00003_003_SN01_18bars_ssf.csv")
`SN02_21bars_ssf` <- read.csv("~/Dataset for Robertson/dataset/train/00192_080_SN02_21bars_ssf.csv")
`SN06_18bars_onmod` <- read.csv("~/Dataset for Robertson/dataset/train/00596_035_SN06_18bars_onmod.csv")

SN01_21bars_offmod$time <- as.POSIXct(SN01_21bars_offmod$time)
SN03_5bars_ssf$time <- as.POSIXct(SN03_5bars_ssf$time)
SN01_18bars_ssf$time <- as.POSIXct(SN01_18bars_ssf$time)
SN02_21bars_ssf$time <- as.POSIXct(SN02_21bars_ssf$time)
SN06_18bars_onmod$time <- as.POSIXct(SN06_18bars_onmod$time)


SN01_21bars_offmod$time <- unclass(SN01_21bars_offmod$time)
SN03_5bars_ssf$time <- unclass(SN03_5bars_ssf$time)
SN01_18bars_ssf$time <- unclass(SN01_18bars_ssf$time)
SN02_21bars_ssf$time <- unclass(SN02_21bars_ssf$time)
SN06_18bars_onmod$time <- unclass(SN06_18bars_onmod$time)

timeCSV1 <- SN01_21bars_offmod$time[ !SN01_21bars_offmod$ton == '0']
tonCSV1 <- SN01_21bars_offmod$ton[ !SN01_21bars_offmod$ton == '0']
thrustCSV1 <- SN01_21bars_offmod$thrust[ !SN01_21bars_offmod$ton == '0']
mfrCSV1 <- SN01_21bars_offmod$mfr[ !SN01_21bars_offmod$ton == '0']
vlCSV1 <- SN01_21bars_offmod$vl[ !SN01_21bars_offmod$ton == '0']
anomaly_codeCSV1 <- SN01_21bars_offmod$anomaly_code[ !SN01_21bars_offmod$ton == '0']

CSV1 <- data.frame(timeCSV1, tonCSV1, thrustCSV1, mfrCSV1, vlCSV1, anomaly_codeCSV1)

plot(CSV1$time, CSV1$thrust, main = "Time vs Thrust Plot 1", xlab = "Time", ylab = "Thrust")
abline(lm(CSV1$thrust ~ CSV1$time), col='red')
#This is an anomaly csv - not sure why it looks like this

timeCSV2 <- SN03_5bars_ssf$time[ !SN03_5bars_ssf$ton == '0']
tonCSV2 <- SN03_5bars_ssf$ton[ !SN03_5bars_ssf$ton == '0']
thrustCSV2 <- SN03_5bars_ssf$thrust[ !SN03_5bars_ssf$ton == '0']
mfrCSV2 <- SN03_5bars_ssf$mfr[ !SN03_5bars_ssf$ton == '0']
vlCSV2 <- SN03_5bars_ssf$vl[ !SN03_5bars_ssf$ton == '0']
anomaly_codeCSV2 <- SN03_5bars_ssf$anomaly_code[ !SN03_5bars_ssf$ton == '0']

CSV2 <- data.frame(timeCSV2, tonCSV2, thrustCSV2, mfrCSV2, vlCSV2, anomaly_codeCSV2)

plot(CSV2$time, CSV2$thrust, main = "Time vs Thrust Plot 2", xlab = "Time", ylab = "Thrust")
abline(lm(CSV2$thrust ~ CSV2$time), col='red')
#This looks better - looks like vapor lock occured

timeCSV3 <- SN01_18bars_ssf$time[ !SN01_18bars_ssf$ton == '0']
tonCSV3 <- SN01_18bars_ssf$ton[ !SN01_18bars_ssf$ton == '0']
thrustCSV3 <- SN01_18bars_ssf$thrust[ !SN01_18bars_ssf$ton == '0']
mfrCSV3 <- SN01_18bars_ssf$mfr[ !SN01_18bars_ssf$ton == '0']
vlCSV3 <- SN01_18bars_ssf$vl[ !SN01_18bars_ssf$ton == '0']
anomaly_codeCSV3 <- SN01_18bars_ssf$anomaly_code[ !SN01_18bars_ssf$ton == '0']

CSV3 <- data.frame(timeCSV3, tonCSV3, thrustCSV3, mfrCSV3, vlCSV3, anomaly_codeCSV3)

plot(CSV3$time, CSV3$thrust, main = "Time vs Thrust Plot 3", xlab = "Time", ylab = "Thrust")
abline(lm(CSV3$thrust ~ CSV3$time), col='red')
#Normal looking thrust plot

timeCSV4 <- SN02_21bars_ssf$time[ !SN02_21bars_ssf$ton == '0']
tonCSV4 <- SN02_21bars_ssf$ton[ !SN02_21bars_ssf$ton == '0']
thrustCSV4 <- SN02_21bars_ssf$thrust[ !SN02_21bars_ssf$ton == '0']
mfrCSV4 <- SN02_21bars_ssf$mfr[ !SN02_21bars_ssf$ton == '0']
vlCSV4 <- SN02_21bars_ssf$vl[ !SN02_21bars_ssf$ton == '0']
anomaly_codeCSV4 <- SN02_21bars_ssf$anomaly_code[ !SN02_21bars_ssf$ton == '0']

CSV4 <- data.frame(timeCSV4, tonCSV4, thrustCSV4, mfrCSV4, vlCSV4, anomaly_codeCSV4)

plot(CSV4$time, CSV4$thrust, main = "Time vs Thrust Plot 4", xlab = "Time", ylab = "Thrust")
abline(lm(CSV4$thrust ~ CSV4$time), col='red')
#Normal looking 

timeCSV5 <- SN06_18bars_onmod$time[ !SN06_18bars_onmod$ton == '0']
tonCSV5 <- SN06_18bars_onmod$ton[ !SN06_18bars_onmod$ton == '0']
thrustCSV5 <- SN06_18bars_onmod$thrust[ !SN06_18bars_onmod$ton == '0']
mfrCSV5 <- SN06_18bars_onmod$mfr[ !SN06_18bars_onmod$ton == '0']
vlCSV5 <- SN06_18bars_onmod$vl[ !SN06_18bars_onmod$ton == '0']
anomaly_codeCSV5 <- SN06_18bars_onmod$anomaly_code[ !SN06_18bars_onmod$ton == '0']

CSV5 <- data.frame(timeCSV5, tonCSV5, thrustCSV5, mfrCSV5, vlCSV5, anomaly_codeCSV5)

plot(CSV5$time, CSV5$thrust, main = "Time vs Thrust Plot 5", xlab = "Time", ylab = "Thrust")
abline(lm(CSV5$thrust ~ CSV5$time), col='red')
#Similar to first plot kinda - anomaly?

