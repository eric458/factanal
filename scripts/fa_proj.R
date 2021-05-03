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
library(GPArotation)
library(tidyverse)

##############################
## directory paths
##############################



##############################
## Read in Data
##############################

hipic_data <- read_xlsx("HiPIC raw data.xlsx")
hipic_data <- subset(hipic_data, select=-FAMILYID)


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


################################
#Read in Correlation Matrix
################################

cormat <- cor(hipic_data, method = c("pearson"), use = "complete.obs")


################################
#Parallel Analysis
################################

fa.parallel(cormat, n.obs=195, fm="ml", fa="fa")


#########################################
#4 Factor Model with Oblimin Rotation
#########################################

efa4.oblimin <- factanal(factors=4,covmat=cormat,n.obs=195,rotation="oblimin") 
efa4.oblimin


#########################################
#5 Factor Model with Oblimin Rotation
#########################################

efa5.oblimin <- factanal(factors=5,covmat=cormat,n.obs=195,rotation="oblimin") 
efa5.oblimin


#########################################
#6 Factor Model with Oblimin Rotation
#########################################

efa6.oblimin <- factanal(factors=6,covmat=cormat,n.obs=195,rotation="oblimin") 
efa6.oblimin


#########################################
#11 Factor Model with Oblimin Rotation
#########################################

efa11.oblimin <- factanal(factors=11,covmat=cormat,n.obs=195,rotation="oblimin") 
efa11.oblimin


#########################################
#12 Factor Model with Oblimin Rotation
#########################################

efa12.oblimin <- factanal(factors=12,covmat=cormat,n.obs=195,rotation="oblimin") 
efa12.oblimin


#########################################
#13 Factor Model with Oblimin Rotation
#########################################

efa13.oblimin <- factanal(factors=13,covmat=cormat,n.obs=195,rotation="oblimin") 
efa13.oblimin


#This is a Jeff


