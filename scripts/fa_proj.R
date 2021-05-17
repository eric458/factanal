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
library(qgraph)
library(polycor)
library(lavaan)

##############################
## directory paths
##############################



##############################
## Read in Data
##############################

#Read in the data - this is for Jeffrey
#hipic_data <- read_xlsx("C:/Users/yjeff/Box/255C Final Project/HiPIC raw data.xlsx")

#this is for Jolie
hipic_data <- read_xlsx("/Users/jolie/Documents/HiPIC raw data.xlsx")

#this is for Eric
hipic_data <- read_xlsx("HiPIC raw data.xlsx")

#for all - run this to get rid of FamilyID
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
#Correlation Matrices
################################

#Pearson
cormat <- cor(hipic_data, method = c("pearson"), use = "complete.obs")

#Polychoric Using Psych Package
polycormat <- polychoric(hipic_data)

rho <- polycormat$rho

#Polychoric Using Polycor Package

#Note: I get an error from following code using the polycor package ==>  polycormat2 <- polychor(hipic_data, ML=TRUE)


#Subset of Extraversion Items
rho.extra <- rho[1:32, 1:32]

#Subset of Agreeableness (Benevolence) Items
rho.agree <- rho[33:72, 33:72]

#Subset of Conscientiousness Items
rho.con <- rho[73:104, 73:104]

#Subset of Neuroticism Items
rho.neuro <- rho[105:120, 105:120]

#Subset of Openness (Imagination) Items
rho.open <- rho[121:144, 121:144]


################################
#Parallel Analyses
################################

fa.parallel(cormat, n.obs=195, fm="ml", fa="fa")

fa.parallel(rho, n.obs=195, fm="ml", fa="fa")

fa.parallel(rho.extra, n.obs=195, fm="ml", fa="fa")
#Literature: 4 factors
#Parallel Analysis: 6 factors
#Eigenvalues above One: 4 factors

fa.parallel(rho.agree, n.obs=195, fm="ml", fa="fa")
#Literature: 5 factors
#Parallel Analysis: 6 factors
#Eigenvalues above One: 4 factors

fa.parallel(rho.con, n.obs=195, fm="ml", fa="fa")
#Literature: 4 factors
#Parallel Analysis: 6 factors
#Eigenvalues above One: 4 factors

fa.parallel(rho.neuro, n.obs=195, fm="ml", fa="fa")
#Literature: 2 factors
#Parallel Analysis: 5 factors
#Eigenvalues above One: 2 factors

fa.parallel(rho.open, n.obs=195, fm="ml", fa="fa")
#Literature: 3 factors
#Parallel Analysis: 5 factors
#Eigenvalues above One: 3 factors

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

efa12.oblimin.rho <- factanal(factors=12,covmat=rho,n.obs=195,rotation="oblimin") 
efa12.oblimin.rho

#########################################
#13 Factor Model with Oblimin Rotation
#########################################

efa13.oblimin <- factanal(factors=13,covmat=cormat,n.obs=195,rotation="oblimin") 
efa13.oblimin

efa13.oblimin.rho <- factanal(factors=13,covmat=rho,n.obs=195,rotation="oblimin") 
efa13.oblimin.rho


#########################################
#Extraversion -- 4 Factor Model
#########################################

efa4.extra <- factanal(factors=4,covmat=rho.extra,n.obs=195,rotation="oblimin") 
efa4.extra


#########################################
#Extraversion -- 6 Factor Model
#########################################

efa6.extra <- factanal(factors=6,covmat=rho.extra,n.obs=195,rotation="oblimin") 
efa6.extra


#########################################
#Agreeableness -- 5 Factor Model
#########################################

efa5.agree <- factanal(factors=5,covmat=rho.agree,n.obs=195,rotation="oblimin") 
efa5.agree


#########################################
#Agreeableness -- 6 Factor Model
#########################################

efa6.agree <- factanal(factors=6,covmat=rho.agree,n.obs=195,rotation="oblimin") 
efa6.agree


#########################################
#Agreeableness -- 4 Factor Model
#########################################

efa4.agree <- factanal(factors=4,covmat=rho.agree,n.obs=195,rotation="oblimin") 
efa4.agree


#########################################
#Conscientiousness -- 4 Factor Model
#########################################

efa4.con <- factanal(factors=4,covmat=rho.con,n.obs=195,rotation="oblimin") 
efa4.con


#########################################
#Conscientiousness -- 6 Factor Model
#########################################

efa6.con <- factanal(factors=6,covmat=rho.con,n.obs=195,rotation="oblimin") 
efa6.con


#########################################
#Neuroticism -- 5 Factor Model
#########################################

efa5.neuro <- factanal(factors=5,covmat=rho.neuro,n.obs=195,rotation="oblimin") 
efa5.neuro


#########################################
#Neuroticism -- 5 Factor Model -- Uncorrelated
#########################################

efa5.neuro.var <- factanal(factors=5,covmat=rho.neuro,n.obs=195,rotation="varimax") 
efa5.neuro.var



#########################################
#Neuroticism -- 2 Factor Model
#########################################

efa2.neuro <- factanal(factors=2,covmat=rho.neuro,n.obs=195,rotation="oblimin") 
efa2.neuro


#########################################
#Neuroticism -- 4 Factor Model
#########################################

efa4.neuro <- factanal(factors=4,covmat=rho.neuro,n.obs=195,rotation="oblimin") 
efa4.neuro


#########################################
#Openness -- 5 Factor Model
#########################################

efa5.open <- factanal(factors=5,covmat=rho.open,n.obs=195,rotation="oblimin") 
efa5.open


#########################################
#Openness -- 3 Factor Model
#########################################

efa3.open <- factanal(factors=3,covmat=rho.open,n.obs=195,rotation="oblimin") 
efa3.open







