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
hipic_data <- read_xlsx("C:/Users/yjeff/Box/255C Final Project/HiPIC raw data.xlsx")

#this is for Jolie
hipic_data <- read_xlsx("/Users/jolie/Documents/HiPIC raw data.xlsx")

#this is for Eric
hipic_data <- read_xlsx("HiPIC raw data.xlsx")

#for all - run this to get rid of FamilyID
hipic_data <- subset(hipic_data, select=-FAMILYID)

##############################
## Recode Data
##############################

#Recoding Function

recode_d<-function(item) {
  item<-item
  original<-item
  item[item == 5] <- 99
  item[item == 4] <- 88
  item[item == 3] <- 3
  item[item == 2] <- 4
  item[item == 1] <- 5
  item[item == 99] <- 1
  item[item == 88] <- 2
  print(table(item))
  print(table(original))
  return(item)
}

# Recode Variables using Dr. Wood's SPSS syntax to identify the tables
# See "itemslist" rmarkdown file

# Create list of variables names that need to be recoded
var_names<-c( #Extraversion
            "PRE_PARENT_HIPIC_1", "PRE_PARENT_HIPIC_3","PRE_PARENT_HIPIC_4",
            "PRE_PARENT_HIPIC_11","PRE_PARENT_HIPIC_12", "PRE_PARENT_HIPIC_29",
             #Agreeableness
             "PRE_PARENT_HIPIC_34", "PRE_PARENT_HIPIC_39","PRE_PARENT_HIPIC_45",
            "PRE_PARENT_HIPIC_53","PRE_PARENT_HIPIC_55", "PRE_PARENT_HIPIC_56",
            "PRE_PARENT_HIPIC_57", "PRE_PARENT_HIPIC_63",
            #Conscientiousness
            "PRE_PARENT_HIPIC_80", "PRE_PARENT_HIPIC_81","PRE_PARENT_HIPIC_83",
            "PRE_PARENT_HIPIC_84","PRE_PARENT_HIPIC_85", "PRE_PARENT_HIPIC_91",
            "PRE_PARENT_HIPIC_94", "PRE_PARENT_HIPIC_95","PRE_PARENT_HIPIC_96",
            "PRE_PARENT_HIPIC_97","PRE_PARENT_HIPIC_98", "PRE_PARENT_HIPIC_99",
            "PRE_PARENT_HIPIC_101", "PRE_PARENT_HIPIC_102","PRE_PARENT_HIPIC_104",
            #Neuroticism
            "PRE_PARENT_HIPIC_114", "PRE_PARENT_HIPIC_116","PRE_PARENT_HIPIC_118",
            #Openness
            "PRE_PARENT_HIPIC_121", "PRE_PARENT_HIPIC_123","PRE_PARENT_HIPIC_139",
            "PRE_PARENT_HIPIC_143")

#Create dataframe with only variables that need to be recoded
attach(hipic_data)
table_r_vars<-subset(hipic_data,
                     select = var_names) 
detach(hipic_data)

#Recode values and assign new recoded variable name
for (i in 1:36) {
  nam<-paste(var_names[i],"_r", sep="")
  assign(nam, recode_d(table_r_vars[i]))
}

# Combine Recoded variables into one dataframe
recoded_table<-data.frame(PRE_PARENT_HIPIC_1_r, PRE_PARENT_HIPIC_3_r,PRE_PARENT_HIPIC_4_r,
                     PRE_PARENT_HIPIC_11_r,PRE_PARENT_HIPIC_12_r, PRE_PARENT_HIPIC_29_r,
                     #Agreeableness
                     PRE_PARENT_HIPIC_34_r, PRE_PARENT_HIPIC_39_r,PRE_PARENT_HIPIC_45_r,
                     PRE_PARENT_HIPIC_53_r,PRE_PARENT_HIPIC_55_r, PRE_PARENT_HIPIC_56_r,
                     PRE_PARENT_HIPIC_57_r, PRE_PARENT_HIPIC_63_r,
                     #Conscientiousness
                     PRE_PARENT_HIPIC_80_r, PRE_PARENT_HIPIC_81_r,PRE_PARENT_HIPIC_83_r,
                     PRE_PARENT_HIPIC_84_r,PRE_PARENT_HIPIC_85_r, PRE_PARENT_HIPIC_91_r,
                     PRE_PARENT_HIPIC_94_r, PRE_PARENT_HIPIC_95_r,PRE_PARENT_HIPIC_96_r,
                     PRE_PARENT_HIPIC_97_r,PRE_PARENT_HIPIC_98_r, PRE_PARENT_HIPIC_99_r,
                     PRE_PARENT_HIPIC_101_r, PRE_PARENT_HIPIC_102_r,PRE_PARENT_HIPIC_104_r,
                     #Neuroticism
                     PRE_PARENT_HIPIC_114_r, PRE_PARENT_HIPIC_116_r,PRE_PARENT_HIPIC_118_r,
                     #Openness
                     PRE_PARENT_HIPIC_121_r, PRE_PARENT_HIPIC_123_r,PRE_PARENT_HIPIC_139_r,
                     PRE_PARENT_HIPIC_143_r)

#Update Column Variable Names
colnames(recoded_table)<-c("PRE_PARENT_HIPIC_1_r", "PRE_PARENT_HIPIC_3_r","PRE_PARENT_HIPIC_4_r",
                          "PRE_PARENT_HIPIC_11_r","PRE_PARENT_HIPIC_12_r", "PRE_PARENT_HIPIC_29_r",
                          #Agreeableness
                          "PRE_PARENT_HIPIC_34_r", "PRE_PARENT_HIPIC_39_r","PRE_PARENT_HIPIC_45_r",
                          "PRE_PARENT_HIPIC_53_r","PRE_PARENT_HIPIC_55_r", "PRE_PARENT_HIPIC_56_r",
                          "PRE_PARENT_HIPIC_57_r", "PRE_PARENT_HIPIC_63_r",
                          #Conscientiousness
                          "PRE_PARENT_HIPIC_80_r", "PRE_PARENT_HIPIC_81_r","PRE_PARENT_HIPIC_83_r",
                          "PRE_PARENT_HIPIC_84_r","PRE_PARENT_HIPIC_85_r", "PRE_PARENT_HIPIC_91_r",
                          "PRE_PARENT_HIPIC_94_r", "PRE_PARENT_HIPIC_95_r","PRE_PARENT_HIPIC_96_r",
                          "PRE_PARENT_HIPIC_97_r","PRE_PARENT_HIPIC_98_r", "PRE_PARENT_HIPIC_99_r",
                          "PRE_PARENT_HIPIC_101_r", "PRE_PARENT_HIPIC_102_r","PRE_PARENT_HIPIC_104_r",
                          #Neuroticism
                          "PRE_PARENT_HIPIC_114_r", "PRE_PARENT_HIPIC_116_r","PRE_PARENT_HIPIC_118_r",
                          #Openness
                          "PRE_PARENT_HIPIC_121_r", "PRE_PARENT_HIPIC_123_r","PRE_PARENT_HIPIC_139_r",
                          "PRE_PARENT_HIPIC_143_r")


#Append Recoded Variables into the hipic_data
hipic_datav2<- data.frame(hipic_data, recoded_table)

#check<-data.frame(hipic_datav2$PRE_PARENT_HIPIC_1, hipic_datav2$PRE_PARENT_HIPIC_1_r)

rm(list=setdiff(ls(), c("hipic_data", "hipic_datav2", "recode_d"))) 

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

rho.neuro.111 <- subset(rho.neuro, select=-PRE_PARENT_HIPIC_111)
rho.neuro.111 <- t(rho.neuro.111)
rho.neuro.111 <- subset(rho.neuro.111, select=-PRE_PARENT_HIPIC_111)

rho.neuro.114 <- subset(rho.neuro, select=-PRE_PARENT_HIPIC_114)
rho.neuro.114 <- t(rho.neuro.114)
rho.neuro.114 <- subset(rho.neuro.114, select=-PRE_PARENT_HIPIC_114)

rho.neuro.114111 <- subset(rho.neuro, select=-c(PRE_PARENT_HIPIC_114,PRE_PARENT_HIPIC_111))
rho.neuro.114111 <- t(rho.neuro.114111)
rho.neuro.114111 <- subset(rho.neuro.114111, select=-c(PRE_PARENT_HIPIC_114,PRE_PARENT_HIPIC_111))

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

fa.parallel(rho.neuro.114, n.obs=195, fm="ml", fa="fa")

fa.parallel(rho.neuro.114111, n.obs=195, fm="ml", fa="fa")

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
#Neuroticism -- 5 Factor Model -- Item 111 Removed
#########################################

efa5.neuro.111 <- factanal(factors=5,covmat=rho.neuro.111,n.obs=195,rotation="oblimin") 
efa5.neuro.111

#########################################
#Neuroticism -- 5 Factor Model -- Item 114 Removed
#########################################

efa5.neuro.114 <- factanal(factors=5,covmat=rho.neuro.114,n.obs=195,rotation="oblimin") 
efa5.neuro.114

#########################################
#Neuroticism -- 4 Factor Model -- Items 114 and 111 Removed
#########################################

efa4.neuro.114111 <- factanal(factors=4,covmat=rho.neuro.114111,n.obs=195,rotation="oblimin") 
efa4.neuro.114111


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

