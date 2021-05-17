# fit-indices.R
# examine null (baseline) model
# manually compute fit statistics

# install lavaan packages (if not already installed on computer)
#install.packages("lavaan") # run if not already installed on computer
library(lavaan) # load lavaan

############################################
## define the sample correlation matrix R ##
############################################

# source: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ability.cov.html

S <- ability.cov$cov # the sample covariance matrix
R <- cov2cor(S) # the sample correlation matrix
n <- ability.cov$n.obs # sample size
p <- nrow(R) # number of observed variables

##########################
## fit a 1-factor model ##
##########################

# specify and fit the model
cfa1 <- 'f1 =~ general + picture + blocks + maze + reading + vocab'
fit1 <- cfa(cfa1,sample.cov=R,std.lv=TRUE,sample.nobs=n)

# view the output; note that the summary output includes both...
# "Model Test User Model:" (which shows the chi-square test for the 1-factor model) and
# "Model Test Baseline Model:" (which shows the chi-square test for the null model)
summary(fit1,fit.measures=TRUE) # view summary (will show in console window)
inspect(fit1,what="std") # view standardized solution (will show in console window)

# compute the chi-square statistic for the "User Model" (1-factor model)
# note that there are some differences in naming of matrices in lavaan vs. McCallum
m1 <- 1 # number of factors in the fitted model
Lambda1 <- inspect(fit1,what="std")$lambda # the factor loadings
Phi1 <- inspect(fit1,what="std")$psi # the factor correlation matrix
Psi1 <- matrix(0,p,p) # create matrix for unique factor variances
diag(Psi1) <- diag(inspect(fit1,what="std")$theta) # add unique factor variances to diagonal
Rho1 <- Lambda1 %*% Phi1 %*% t(Lambda1) + Psi1 # the model-implied correlations
F1 <- log(det(Rho1)) - log(det(R)) + sum(diag((R-Rho1)%*%solve(Rho1)))
chisq1 <- n*F1 #chi-square statistic
df1 <- ((p-m1)^2-(p+m1))/2 # degrees of freedom
pvalue1 <- 1-pchisq(q=chisq1,df=df1) # p-value for the chi-square test statistic
chisq1;df1;pvalue1 # view results (will show in console window)

###############################
## the null (baseline) model ##
###############################

# the "null" or "baseline" model is a "zero-factor model" or "independence model"
# in this model, the observed variables are assumed to be mutually independent (uncorrelated)
# thus, the implied correlation matrix Rho0 for the null model is an identity matrix
Rho0 <- matrix(0,p,p) # create an object "Rho0" to save the model-implied correlation matrix
diag(Rho0) <- 1 # add ones to the diagonal (so Rho0 is now an identify matrix)

# now, compare the model-implied correlation matrix Rho0 to the sample correlation matrix R
R # print the sample correlation matrix (will show in console window)
Rho0 # print the implied correlation matrix for the null model (will show in console window)
R-Rho0 # print the residual correlation matrix (will show in console window)

# compute the chi-square statistic for the null model
m0 <- 0 # number of factors
F0 <- log(det(Rho0)) - log(det(R)) + sum(diag((R-Rho0)%*%solve(Rho0)))
chisq0 <- n*F0 #chi-square statistic (this is different formula from that used in factanal function)
df0 <- ((p-m0)^2-(p+m0))/2 # degrees of freedom
pvalue0 <- 1-pchisq(q=chisq0,df=df0) # p-value for the chi-square test statistic
chisq0;df0;pvalue0 # view results (will show in console window)

# incidentally, we can force lavaan to fit the null model
# for example, we could fit a one-factor model but fix all the factor loadings to zero
# view the output; note that the "User Model" and "Baseline Model" are now the same
cfa0 <- 'f1 =~ 0*general + 0*picture + 0*blocks + 0*maze + 0*reading + 0*vocab'
fit0 <- cfa(cfa0,sample.cov=R,std.lv=TRUE,sample.nobs=n)
summary(fit0,fit.measures=TRUE) # view summary (will show in console window)
inspect(fit0,what="std") # view standardized solution (will show in console window)

####################################################
## compute the fit indices for the 1-factor model ##
####################################################

# manually compute the indices printed in the summary output

# comparative fit indices based on chi-square statistics
CFI <- ((chisq0-df0)-(chisq1-df1))/(chisq0-df0)
TLI <- (chisq0/df0-chisq1/df1)/(chisq0/df0-1)

# indices based on likelihood
# note that lavaan uses -2 times the log-likelihood instead of the chi-square statistic
L1 <-fitmeasures(fit1)["logl"] # log-likelihood
k1 <- (p*(p+1)/2-df1) # number of parameters estimated
AIC <- -2*L1+2*k1 # Akaike Information Criterion (AIC)
BIC <- -2*L1+log(n)*k1 # Bayesian Information Criterion (BIC)
SABIC <- -2*L1+log((n+2)/24)*k1 # Sample-size Adjusted Bayesian Information Criterion (SABIC)

# Root Mean Square Error of Approximation (RMSEA)
RMSEA <- sqrt((chisq1-df1)/(n*df1))

# Standardized Root Mean Square Residual (SRMR)
nmr <- 0 # numerator for the SRMR
for (i in 2:p) { # loop over rows
  for (j in 1:(i-1)) { # loop over columns
    nmr <- nmr+((R[i,j]-Rho1[i,j])/(R[i,i]*R[j,j]))^2
    } # end loop over columns
  } # end loop over rows
SRMR <- sqrt(nmr/(p*(p+1)/2))

# view all the indices (will show in console window)
CFI
TLI
AIC
BIC
SABIC
RMSEA
SRMR

# compare to those reported by lavaan
summary(fit1,fit.measures=TRUE) # view summary (will show in console window)
