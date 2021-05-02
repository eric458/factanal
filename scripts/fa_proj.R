################################################################################
##
## [ PROJ ] < 255C Project >
##
################################################################################

##############################
## libraries
##############################

library(readxl)
library(psych)
library(knitr)

##############################
## directory paths
##############################



##############################
## Read in Data
##############################

hipic_data<-read_excel("./HiPIC raw data.xlsx")

##############################
## Run Descriptives Table
##############################

desc_table<-round(describe(hipic_data),2)
desc_table = subset(desc_table, select = c("vars","n","mean",
                                           "sd","median","min","max","skew",
                                           "kurtosis", "se"))

kable(desc_table, caption = "Descriptives of HiPIC Data")

##############################
## Histograms
##############################

for (i in 1:length(hipic_data)) {
  hist(sapply(hipic_data[,i], as.numeric),
       main = colnames(hipic_data[i]),
       xlab =  colnames(hipic_data[i]))
}