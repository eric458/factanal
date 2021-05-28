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
library(ltm)

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
            "PRE_PARENT_HIPIC_113", "PRE_PARENT_HIPIC_115","PRE_PARENT_HIPIC_117",
            "PRE_PARENT_HIPIC_119", "PRE_PARENT_HIPIC_120",
             #Openness
            "PRE_PARENT_HIPIC_121", "PRE_PARENT_HIPIC_123","PRE_PARENT_HIPIC_139",
            "PRE_PARENT_HIPIC_143")

#Create dataframe with only variables that need to be recoded
attach(hipic_data)
table_r_vars<-subset(hipic_data,
                     select = var_names) 
detach(hipic_data)

#Recode values and assign new recoded variable name
for (i in 1:length(var_names)) {
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
                     PRE_PARENT_HIPIC_113_r, PRE_PARENT_HIPIC_115_r, PRE_PARENT_HIPIC_117_r,
                     PRE_PARENT_HIPIC_119_r, PRE_PARENT_HIPIC_120_r,
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
                          "PRE_PARENT_HIPIC_113_r", "PRE_PARENT_HIPIC_115_r","PRE_PARENT_HIPIC_117_r",
                          "PRE_PARENT_HIPIC_119_r", "PRE_PARENT_HIPIC_120_r",
                          #Openness
                          "PRE_PARENT_HIPIC_121_r", "PRE_PARENT_HIPIC_123_r","PRE_PARENT_HIPIC_139_r",
                          "PRE_PARENT_HIPIC_143_r")


#Append Recoded Variables into the hipic_data
hipic_datav2<- data.frame(hipic_data, recoded_table)

hipic_datav2<-subset(hipic_datav2,
               select = -c(PRE_PARENT_HIPIC_1,
                 PRE_PARENT_HIPIC_3,PRE_PARENT_HIPIC_4,
                 PRE_PARENT_HIPIC_11,PRE_PARENT_HIPIC_12, PRE_PARENT_HIPIC_29,
                 #Agreeableness
                 PRE_PARENT_HIPIC_34, PRE_PARENT_HIPIC_39,PRE_PARENT_HIPIC_45,
                 PRE_PARENT_HIPIC_53,PRE_PARENT_HIPIC_55, PRE_PARENT_HIPIC_56,
                 PRE_PARENT_HIPIC_57, PRE_PARENT_HIPIC_63,
                 #Conscientiousness
                 PRE_PARENT_HIPIC_80, PRE_PARENT_HIPIC_81,PRE_PARENT_HIPIC_83,
                 PRE_PARENT_HIPIC_84,PRE_PARENT_HIPIC_85, PRE_PARENT_HIPIC_91,
                 PRE_PARENT_HIPIC_94, PRE_PARENT_HIPIC_95,PRE_PARENT_HIPIC_96,
                 PRE_PARENT_HIPIC_97,PRE_PARENT_HIPIC_98, PRE_PARENT_HIPIC_99,
                 PRE_PARENT_HIPIC_101,PRE_PARENT_HIPIC_102,PRE_PARENT_HIPIC_104,
                 #Neuroticism
                 PRE_PARENT_HIPIC_113, PRE_PARENT_HIPIC_115,PRE_PARENT_HIPIC_117,
                 PRE_PARENT_HIPIC_119, PRE_PARENT_HIPIC_120,
                 #Openness
                 PRE_PARENT_HIPIC_121, PRE_PARENT_HIPIC_123,PRE_PARENT_HIPIC_139,
                 PRE_PARENT_HIPIC_143))
 
  

#check<-data.frame(hipic_datav2$PRE_PARENT_HIPIC_1, hipic_datav2$PRE_PARENT_HIPIC_1_r)

rm(list=setdiff(ls(), c("hipic_data", "hipic_datav2", "recode_d"))) 

#########
# Check alpha values
#########

#Original data
cronbach.alpha(hipic_data, na.rm = TRUE)

#Extraversion
cronbach.alpha(hipic_data[1:32], na.rm = TRUE)

#Agreeableness
cronbach.alpha(hipic_data[33:72], na.rm = TRUE)

#Conscientiousness
cronbach.alpha(hipic_data[73:104], na.rm = TRUE)

#Neuroticism
cronbach.alpha(hipic_data[105:120], na.rm = TRUE)

#Openess
cronbach.alpha(hipic_data[121:144], na.rm = TRUE)


#Recoded data - overall
cronbach.alpha(hipic_datav2, na.rm = TRUE)
#alpha value is 0.862

#Recoded data - Extaversion

grep("PRE_PARENT_HIPIC_2", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_32", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_1_r", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_29_r", names(hipic_datav2))

#Create Extraversion Data subset

ext_recode<-data.frame(hipic_datav2[1:26], hipic_datav2[107:112])

#Run cronbach alpha
cronbach.alpha(ext_recode,na.rm=TRUE)
  
#Recoded data - Agreeableness
grep("PRE_PARENT_HIPIC_33", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_72", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_34_r", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_63_r", names(hipic_datav2))

#Create agreeabless Data subset
agr_recode<-data.frame(hipic_datav2[27:58], hipic_datav2[113:120])

#Run cronbach alpha
cronbach.alpha(agr_recode,na.rm=TRUE)

#Recoded data - conscientiousness
grep("PRE_PARENT_HIPIC_73", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_103", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_80_r", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_104_r", names(hipic_datav2))

#Create conscientiousness Data subset
con_recode<-data.frame(hipic_datav2[59:75], hipic_datav2[121:135])

#Run cronbach alpha
cronbach.alpha(con_recode,na.rm=TRUE)

#Recoded data - Neuroticism
grep("PRE_PARENT_HIPIC_105", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_118", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_113_r", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_120_r", names(hipic_datav2))

#Create Neuroticism Data subset
neu_recode<-data.frame(hipic_datav2[76:86], hipic_datav2[136:140])

#Run cronbach alpha
cronbach.alpha(neu_recode,na.rm=TRUE)

#Recoded data - Openness
grep("PRE_PARENT_HIPIC_122", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_144", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_121_r", names(hipic_datav2))
grep("PRE_PARENT_HIPIC_143_r", names(hipic_datav2))

#Create Neuroticism Data subset
open_recode<-data.frame(hipic_datav2[87:106], hipic_datav2[141:144])

#Run cronbach alpha
cronbach.alpha(open_recode,na.rm=TRUE)


##############################
## Run Descriptives Table
##############################

desc_table<-round(describe(hipic_datav2),2)
desc_table = subset(desc_table, select = c("vars","n","mean",
                                           "sd","median","min","max","skew",
                                           "kurtosis", "se"))

kable(desc_table, caption = "Descriptives of HiPIC Data")


##############################
## Histograms
##############################

for (i in 1:length(hipic_datav2)) {
  hist(sapply(hipic_datav2[,i], as.numeric),
       main = colnames(hipic_datav2[i]),
       xlab =  colnames(hipic_datav2[i]))
}


################################
#Correlation Matrices
################################

#Pearson
cormat <- cor(hipic_datav2, method = c("pearson"), use = "complete.obs")

#Polychoric Using Psych Package
polycormat <- polychoric(hipic_datav2)

#Ext
Ext_cor<- polychoric(ext_recode)

rho.ext <- Ext_cor$rho

#Agr
agr_cor<- polychoric(agr_recode)

rho.agr <- agr_cor$rho

#Con
con_cor<- polychoric(con_recode)

rho.con <- con_cor$rho

#Neu
neu_cor<- polychoric(neu_recode)

rho.neu <- neu_cor$rho

#Open
opn_cor<- polychoric(open_recode)

rho.opn <- opn_cor$rho

#################
#Polychoric Using Polycor Package

#Note: I get an error from following code using the polycor package ==>  polycormat2 <- polychor(hipic_data, ML=TRUE)

#Subset of Extraversion Items
#rho.extra <- rho[1:32, 1:32]

#Subset of Agreeableness (Benevolence) Items
#rho.agree <- rho[33:72, 33:72]

#Subset of Conscientiousness Items
#rho.con <- rho[73:104, 73:104]

#Subset of Neuroticism Items
#rho.neuro <- rho[105:120, 105:120]

#rho.neuro.111 <- subset(rho.neuro, select=-PRE_PARENT_HIPIC_111)
#rho.neuro.111 <- t(rho.neuro.111)
#rho.neuro.111 <- subset(rho.neuro.111, select=-PRE_PARENT_HIPIC_111)

#rho.neuro.114 <- subset(rho.neuro, select=-PRE_PARENT_HIPIC_114)
#rho.neuro.114 <- t(rho.neuro.114)
#rho.neuro.114 <- subset(rho.neuro.114, select=-PRE_PARENT_HIPIC_114)

#rho.neuro.114111 <- subset(rho.neuro, select=-c(PRE_PARENT_HIPIC_114,PRE_PARENT_HIPIC_111))
#rho.neuro.114111 <- t(rho.neuro.114111)
#rho.neuro.114111 <- subset(rho.neuro.114111, select=-c(PRE_PARENT_HIPIC_114,PRE_PARENT_HIPIC_111))

#Subset of Openness (Imagination) Items
#rho.open <- rho[121:144, 121:144]

################################
#Parallel Analyses
################################

fa.parallel(cormat, n.obs=195, fm="ml", fa="fa")

fa.parallel(rho, n.obs=195, fm="ml", fa="fa")

fa.parallel(rho.ext, n.obs=195, fm="ml", fa="fa")
#Literature: 4 factors
#Parallel Analysis: 6 factors
#Eigenvalues above One: 4 factors

fa.parallel(rho.agr, n.obs=195, fm="ml", fa="fa")
#Literature: 5 factors
#Parallel Analysis: 6 factors
#Eigenvalues above One: 4 factors

fa.parallel(rho.con, n.obs=195, fm="ml", fa="fa")
#Literature: 4 factors
#Parallel Analysis: 7 factors
#Eigenvalues above One: 4 factors

fa.parallel(rho.neu, n.obs=195, fm="ml", fa="fa")
#Literature: 2 factors
#Parallel Analysis: 5 factors
#Eigenvalues above One: 2 factors

#fa.parallel(rho.neuro.114, n.obs=195, fm="ml", fa="fa")

#fa.parallel(rho.neuro.114111, n.obs=195, fm="ml", fa="fa")

fa.parallel(rho.opn, n.obs=195, fm="ml", fa="fa")
#Literature: 3 factors
#Parallel Analysis: 5 factors
#Eigenvalues above One: 3 factors


#########################################
#Extraversion -- 4 Factor Model
#########################################

#EFA
efa4.extra <- factanal(factors=4,covmat=rho.ext,n.obs=195,rotation="oblimin") 
efa4.extra

#CFA
cfa4.extra.model <- 'f1 =~ PRE_PARENT_HIPIC_8 +PRE_PARENT_HIPIC_9 +
                            PRE_PARENT_HIPIC_10 +PRE_PARENT_HIPIC_15 +
                            PRE_PARENT_HIPIC_16 +PRE_PARENT_HIPIC_1_r +
                            PRE_PARENT_HIPIC_4_r + PRE_PARENT_HIPIC_11_r +
                            PRE_PARENT_HIPIC_12_r + PRE_PARENT_HIPIC_5 +
                            PRE_PARENT_HIPIC_14 + PRE_PARENT_HIPIC_13 +
                            PRE_PARENT_HIPIC_6
                      f2 =~ PRE_PARENT_HIPIC_17 +PRE_PARENT_HIPIC_19 +
                            PRE_PARENT_HIPIC_20 +PRE_PARENT_HIPIC_21 +
                            PRE_PARENT_HIPIC_22 +PRE_PARENT_HIPIC_24 +
                            PRE_PARENT_HIPIC_26 +PRE_PARENT_HIPIC_18
                      f3 =~ PRE_PARENT_HIPIC_25 +PRE_PARENT_HIPIC_27 +
                            PRE_PARENT_HIPIC_28 +PRE_PARENT_HIPIC_30 +
                            PRE_PARENT_HIPIC_13 +PRE_PARENT_HIPIC_26 +
                            PRE_PARENT_HIPIC_31 +PRE_PARENT_HIPIC_29_r
                      f4 =~ PRE_PARENT_HIPIC_2 +PRE_PARENT_HIPIC_7 +
                            PRE_PARENT_HIPIC_5 +PRE_PARENT_HIPIC_6'

#Note: I (Jeffrey) didn't include all the extraversion items 
#(per what we decided in the google doc), should I have made a smaller 
# covariance matrix to run this CFA?
cfa4.extra.model.fit <-cfa(cfa4.extra.model, sample.cov=rho.ext,
                           sample.nobs=195, std.lv=TRUE)

summary(cfa4.extra.model.fit, fit.measures=TRUE, standardized=TRUE)



#########################################
#Agreeableness -- 5 Factor Model
#########################################

efa5.agree <- factanal(factors=5,covmat=rho.agr,n.obs=195,rotation="oblimin") 
efa5.agree

#CFA
cfa5.agree.model <- 'f1 =~ PRE_PARENT_HIPIC_33 +PRE_PARENT_HIPIC_40 +
                            PRE_PARENT_HIPIC_41 +PRE_PARENT_HIPIC_42 +
                            PRE_PARENT_HIPIC_43 +PRE_PARENT_HIPIC_44 +
                            PRE_PARENT_HIPIC_46 + PRE_PARENT_HIPIC_47 +
                            PRE_PARENT_HIPIC_48 
                      f2 =~ PRE_PARENT_HIPIC_65 +PRE_PARENT_HIPIC_66 +
                            PRE_PARENT_HIPIC_67 +PRE_PARENT_HIPIC_68 +
                            PRE_PARENT_HIPIC_69 +PRE_PARENT_HIPIC_70 +
                            PRE_PARENT_HIPIC_71 +PRE_PARENT_HIPIC_72
                      f3 =~ PRE_PARENT_HIPIC_39_r +PRE_PARENT_HIPIC_45_r +
                            PRE_PARENT_HIPIC_49 +PRE_PARENT_HIPIC_50 +
                            PRE_PARENT_HIPIC_51 +PRE_PARENT_HIPIC_52 +
                            PRE_PARENT_HIPIC_53_r +PRE_PARENT_HIPIC_54+
                            PRE_PARENT_HIPIC_65
                      f4 =~ PRE_PARENT_HIPIC_56_r +PRE_PARENT_HIPIC_57_r +
                            PRE_PARENT_HIPIC_58 +PRE_PARENT_HIPIC_62+
                            PRE_PARENT_HIPIC_64 +PRE_PARENT_HIPIC_63_r+
                            PRE_PARENT_HIPIC_59 +PRE_PARENT_HIPIC_60+
                            PRE_PARENT_HIPIC_61
                      f5 =~ PRE_PARENT_HIPIC_35 +PRE_PARENT_HIPIC_36 +
                            PRE_PARENT_HIPIC_37 +PRE_PARENT_HIPIC_38+
                            PRE_PARENT_HIPIC_55_r +PRE_PARENT_HIPIC_59'

#Note: Did not include item 34

cfa5.agree.model.fit <-cfa(cfa5.agree.model, sample.cov=rho.agr,
                           sample.nobs=195, std.lv=TRUE)

summary(cfa5.agree.model.fit, fit.measures=TRUE, standardized=TRUE)


#########################################
#Agreeableness -- 4 Factor Model
#########################################

efa4.agree <- factanal(factors=4,covmat=rho.agr,n.obs=195,rotation="oblimin") 
efa4.agree

#CFA
cfa4.agree.model <- 'f1 =~ PRE_PARENT_HIPIC_33 +PRE_PARENT_HIPIC_40 +
                            PRE_PARENT_HIPIC_41 +PRE_PARENT_HIPIC_42 +
                            PRE_PARENT_HIPIC_43 +PRE_PARENT_HIPIC_44 +
                            PRE_PARENT_HIPIC_46 + PRE_PARENT_HIPIC_47 +
                            PRE_PARENT_HIPIC_48 
                      f2 =~ PRE_PARENT_HIPIC_66 +
                            PRE_PARENT_HIPIC_67 +PRE_PARENT_HIPIC_68 +
                            PRE_PARENT_HIPIC_69 +PRE_PARENT_HIPIC_70 +
                            PRE_PARENT_HIPIC_71 +PRE_PARENT_HIPIC_72
                      f3 =~ PRE_PARENT_HIPIC_35 +PRE_PARENT_HIPIC_36 +
                            PRE_PARENT_HIPIC_37 +PRE_PARENT_HIPIC_38 +
                            PRE_PARENT_HIPIC_58 +PRE_PARENT_HIPIC_59 +
                            PRE_PARENT_HIPIC_60 +PRE_PARENT_HIPIC_61+
                            PRE_PARENT_HIPIC_62 + PRE_PARENT_HIPIC_64
                      f4 =~ PRE_PARENT_HIPIC_45_r +PRE_PARENT_HIPIC_49 +
                            PRE_PARENT_HIPIC_50 +PRE_PARENT_HIPIC_51+
                            PRE_PARENT_HIPIC_52 +PRE_PARENT_HIPIC_54'

#Unsure about the cross loaded items section
cfa4.agree.model.fit <-cfa(cfa4.agree.model, sample.cov=rho.agr,
                           sample.nobs=195, std.lv=TRUE)

summary(cfa4.agree.model.fit, fit.measures=TRUE, standardized=TRUE)


#########################################
#Conscientiousness -- 4 Factor Model
#########################################

efa4.con <- factanal(factors=4,covmat=rho.con,n.obs=195,rotation="oblimin")
efa4.con

efa7.con <- factanal(factors=7,covmat=rho.con,n.obs=195,rotation="oblimin")
efa7.con

efa4.con.v2 <- fa(r=rho.con, nfactors=4,n.obs=195,rotate="oblimin")
efa4.con.v2

efa7.con.v2 <- fa(r=rho.con, nfactors=7,n.obs=195,rotate="oblimin")
efa7.con.v2

#CFA
cfa4.con.model <- 'f1 =~ PRE_PARENT_HIPIC_73 +PRE_PARENT_HIPIC_74 +
PRE_PARENT_HIPIC_75 +PRE_PARENT_HIPIC_76 +
PRE_PARENT_HIPIC_77 +PRE_PARENT_HIPIC_78 +
PRE_PARENT_HIPIC_79 + PRE_PARENT_HIPIC_80_r
f2 =~ PRE_PARENT_HIPIC_81_r +PRE_PARENT_HIPIC_82 +
PRE_PARENT_HIPIC_83_r +PRE_PARENT_HIPIC_84_r +
PRE_PARENT_HIPIC_85_r +PRE_PARENT_HIPIC_86 +
PRE_PARENT_HIPIC_87 +PRE_PARENT_HIPIC_88
f3 =~ PRE_PARENT_HIPIC_89 +PRE_PARENT_HIPIC_90 +
PRE_PARENT_HIPIC_91_r +PRE_PARENT_HIPIC_92 +PRE_PARENT_HIPIC_93 + 
PRE_PARENT_HIPIC_94_r +PRE_PARENT_HIPIC_95_r +PRE_PARENT_HIPIC_96_r
f4 =~ PRE_PARENT_HIPIC_97_r +PRE_PARENT_HIPIC_98_r +
PRE_PARENT_HIPIC_99_r +PRE_PARENT_HIPIC_100 +PRE_PARENT_HIPIC_101_r + 
PRE_PARENT_HIPIC_102_r +PRE_PARENT_HIPIC_103 +PRE_PARENT_HIPIC_104_r'

#Updated correlation matrix and removed 2 items 80 & 102
rho.con.v2 <- subset(rho.con, select=-c(PRE_PARENT_HIPIC_80_r,PRE_PARENT_HIPIC_102_r))
rho.con.v2 <- t(rho.con.v2)
rho.con.v2 <- subset(rho.con.v2, select=-c(PRE_PARENT_HIPIC_80_r,PRE_PARENT_HIPIC_102_r))

#Note: I (Jeffrey) didn't include all the extraversion items 
#(per what we decided in the google doc), should I have made a smaller 
# covariance matrix to run this CFA?
cfa4.con.model.fit <-cfa(cfa4.con.model, sample.cov=rho.con,
                           sample.nobs=195, std.lv=TRUE)

summary(cfa4.con.model.fit, fit.measures=TRUE, standardized=TRUE)


 #Using Conscientiousness subdomains from established research article Tackett




#########################################
#Neuroticism -- 2 Factor Model
#########################################

efa2.neuro <- factanal(factors=2,covmat=rho.neu,n.obs=195,rotation="oblimin") 
efa2.neuro


#########################################
#Neuroticism -- CFA -- 3 Factor Model -- Items 114 and 111 Removed
#########################################

cfa3.neuro.model <- 'f1 =~ PRE_PARENT_HIPIC_106 +PRE_PARENT_HIPIC_108 +PRE_PARENT_HIPIC_109+
                            PRE_PARENT_HIPIC_110
                      f2 =~ PRE_PARENT_HIPIC_105 +PRE_PARENT_HIPIC_107 +PRE_PARENT_HIPIC_116+
                            PRE_PARENT_HIPIC_118 + PRE_PARENT_HIPIC_112
                      f3 =~ PRE_PARENT_HIPIC_113 +PRE_PARENT_HIPIC_115 +PRE_PARENT_HIPIC_117+
                            PRE_PARENT_HIPIC_119 + PRE_PARENT_HIPIC_120' 

cfa3.neuro.model.fit <- cfa(cfa3.neuro.model, sample.cov=rho.neuro.114111, sample.nobs=195, std.lv=TRUE)
summary(cfa3.neuro.model.fit, fit.measures=TRUE, standardized=TRUE)



#########################################
#Openness -- 3 Factor Model 
#########################################

efa3.open <- factanal(factors=3,covmat=rho.opn,n.obs=195,rotation="oblimin") 
efa3.open




