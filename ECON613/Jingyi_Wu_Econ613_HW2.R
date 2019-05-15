##Exercise 1
set.seed(007) # set seed
X1 = data.frame(X1 = runif(10000,1,3)) #draw from uniform dist.
X2 = data.frame(X2 = rgamma(10000,3,2)) #draw from gamma dist.
X3 = data.frame(X3 = rbinom(10000,1,0.3)) #draw from binomial dist.
eps = data.frame(eps = rnorm(10000,2,1)) #draw from normal dist.
Y = as.matrix(0.5 + 1.2*X1 - 0.9*X2 +0.1*X3 + eps) #create variable Y
colnames(Y)=c("Y")
mean_Y = mean(Y[,1]) #Calculate mean of Y
ydum = data.frame(ydum = as.numeric(Y > mean_Y)) #create variable ydum
ydum = as.matrix(ydum)

##Exercise 2 OLS
#1 Correlation
corrY_X1 = cor(Y, X1) #Calculate correlation between Y and X1
corrY_X1 
#        X1
# Y 0.48255
#it's very different from 1.2

#2 OLS
intercept = as.data.frame(1)
intercept = data.frame(intercept = intercept[rep(1:nrow(intercept),each=10000),]) #create a 1*10000 vector of 1
reg_data = cbind(Y,intercept,X1,X2,X3) #combine info
reg_data1 = cbind(ydum,intercept,X1,X2,X3)
X = as.matrix(cbind(intercept,X1,X2,X3))
XtX_inv = solve(t(X) %*% X) #Calculate (X'X)^(-1)
beta_ols = XtX_inv%*%t(X)%*%Y #Calculate beta of ols
epshat = Y - X%*%beta_ols #Calculate residual
res_squared = t(as.matrix(epshat)) %*% (as.matrix(epshat)) #Calculate e'e
s_squared = as.numeric(res_squared/9996) #Calculate s^2
cov_coef = s_squared * XtX_inv #calculate covariance matrix
sd_ols = sqrt(cbind(cov_coef[1,1],cov_coef[2,2],cov_coef[3,3],cov_coef[4,4])) #calculate standard error
rownames(sd_ols)=c("sd_ols")
colnames(sd_ols)=c("sd_intercept","sd_X1","sd_X2","sd_X3")
sd_ols
#        sd_intercept      sd_X1      sd_X2      sd_X3
# sd_ols   0.04038722 0.01732722 0.01168405 0.02169208

#3 Bootstrap
r1 = 49 #rep=49
beta_boot1 = matrix(c(0,0,0,0),nrow=1)
beta_boot1 = matrix(beta_boot1[rep(1:nrow(beta_boot1),each=r1),],nrow=r1) #create a 4*49 matrix for betas
for (b in 1:r1) {
  i = sample(1:10000, size = 10000, replace = TRUE) #subsampling
  Yb1 = as.matrix(reg_data[i,1])
  X1 = reg_data[i,3]
  X2 = reg_data[i,4]
  X3 = reg_data[i,5]
  Xb1 = as.matrix(cbind(intercept,X1,X2,X3))
  beta_boot1[b,] = solve(t(Xb1)%*%Xb1)%*%t(Xb1)%*%Yb1
}
sd_boot1 = cbind(sd(beta_boot1[,1]),sd(beta_boot1[,2]),sd(beta_boot1[,3]),sd(beta_boot1[,4])) #report the standard error 
rownames(sd_boot1)=c("sd_bootstrap1")
colnames(sd_boot1)=c("sd_intercept","sd_X1","sd_X2","sd_X3")
sd_boot1
#did not show the output here since the sample() will sample differently every time, so the results will be slightly different

r2 = 499 #rep=499
beta_boot2 = matrix(c(0,0,0,0),nrow=1)
beta_boot2 = matrix(beta_boot2[rep(1:nrow(beta_boot2),each=r2),],nrow=r2) #create a 4*49 matrix for betas
for (b in 1:r2) {
  i = sample(1:10000, size = 10000, replace = TRUE) #subsampling
  Yb2 = as.matrix(reg_data[i,1])
  X1 = reg_data[i,3]
  X2 = reg_data[i,4]
  X3 = reg_data[i,5]
  Xb2 = as.matrix(cbind(intercept,X1,X2,X3))
  beta_boot2[b,] = solve(t(Xb2)%*%Xb2)%*%t(Xb2)%*%Yb2
}
sd_boot2 = cbind(sd(beta_boot2[,1]),sd(beta_boot2[,2]),sd(beta_boot2[,3]),sd(beta_boot2[,4])) #report the standard error 
rownames(sd_boot2)=c("sd_bootstrap2")
colnames(sd_boot2)=c("sd_intercept","sd_X1","sd_X2","sd_X3")
sd_boot2
#did not show the output here since the sample() will sample differently every time, so the results will be slightly different

##Exercise 3 Numerical Optimization
#likelihood function
pro_ll = function(b) #define the probit likelihood function
{
  f = sum(ydum*log(pnorm(X%*%b)))+sum((1-ydum)*log(1-pnorm(X%*%b)))
  return(f)
}
#implement the steepest ascent optimization
bk = matrix(c(0.5,0.5,0.5,0.5),ncol=1) #set default value of beta
difference = 1 #set default value of difference
while (difference>0.0000001) {
    pro_ll1 = pro_ll(bk) #calculate log likelihood of bk
    ak = 0.00001 #set step
    eps0 = 0.000001 #set epsilon for derivatives
    bk_a = matrix(bk,4,4) 
    e = diag(eps0,4,4)
    bk_b = bk_a + e
    dk1 = (pro_ll(bk_b[,1])-pro_ll(bk_a[,1]))/eps0 #calculate partical derivatives(gradient) for each x variable
    dk2 = (pro_ll(bk_b[,2])-pro_ll(bk_a[,2]))/eps0
    dk3 = (pro_ll(bk_b[,3])-pro_ll(bk_a[,3]))/eps0
    dk4 = (pro_ll(bk_b[,4])-pro_ll(bk_a[,4]))/eps0
    dk = rbind(dk1,dk2,dk3,dk4)
    c = ak*dk
    bk = bk + c #update rules of bk+1
    pro_ll2 = pro_ll(bk) #calculate log likelihood of bk+1
    difference = (pro_ll1-pro_ll2)/pro_ll1 #calculate increasing rate of log likelihood (for stopping rule)
}
beta_probit_ll = data.frame(beta_probit = bk,row.names = c("intercept","X1","X2","X3"))
beta_probit_ll
#           beta_probit
# intercept -1.07790115
# X1         1.19347159
# X2        -0.89016682
# X3         0.06062339
##check difference between true parameters
#the true b is (0.5,1.2,-0.9,0.1)
#The coefficients for X1 and X2 is quite similar to the true parameters

#Exercise 4 Discrete Choice
#for probit model
pro_ll_1 = function(b,x,y) #define the minus probit likelihood function
{
  f = -sum(y*log(pnorm(x%*%b)))-sum((1-y)*log(1-pnorm(x%*%b)))
  return(f)
}
bk = matrix(c(0.5,0.5,0.5,0.5),ncol=1) #set default beta
beta_probit = optim(bk,pro_ll_1,x=X, y=ydum,method = "BFGS")$par #return the beta for probit model
beta_probit
#             [,1]
# [1,] -1.10207605
# [2,]  1.20344182
# [3,] -0.88846005
# [4,]  0.06290443
## Interpretation
#if X1 increase, the probability of y success is likely to increase.
#if X2 increase, the probability of y success is likely to decrease.
#if X3 increase, the probability of y success is likely to increase.

#check and see significance level using glm function
probit = glm(ydum~ 0+X, data = reg_data1, family = binomial(link = "probit"))
beta_probit = as.matrix(probit$coefficients,ncol=1)
summary(probit)
# Call:
#   glm(formula = ydum ~ 0 + X, family = binomial(link = "probit"), 
#       data = reg_data1)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.7789  -0.8262   0.2237   0.7970   3.0566  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# Xintercept   -1.10247    0.05702 -19.333   <2e-16 ***
#   XX1         1.20363    0.02773  43.410   <2e-16 ***
#   XX2        -0.88850    0.02147 -41.386   <2e-16 ***
#   XX3         0.06301    0.03146   2.003   0.0452 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 13862.9  on 10000  degrees of freedom
# Residual deviance:  9873.2  on  9996  degrees of freedom
# AIC: 9881.2
# 
# Number of Fisher Scoring iterations: 5
##Interpretation for significance
#all the coefficients are significant, and coefficients for X1 and X2 are more significant.
#the results using optim() is quite similar to using glm()

#for logit model
logit_func = function(x){ #define the logistic function
  exp(x)/(1+exp(x))
}
log_ll = function(b,x,y) #define the minus logit likelihood function
{
  f = -sum(y*log(logit_func(x%*%b)))-sum((1-y)*log(1-logit_func(x%*%b)))
  return(f)
}
beta_logit = optim(bk,log_ll,x=X, y=ydum,method = "BFGS")$par #return the beta for logit model
beta_logit
#            [,1]
# [1,] -1.8588665
# [2,]  2.0409342
# [3,] -1.5172659
# [4,]  0.1056161
## Interpretation
#if X1 increase, the probability of y success is likely to increase.
#if X2 increase, the probability of y success is likely to decrease.
#if X3 increase, the probability of y success is likely to increase.

#check and see significance level using glm function
logit = glm(ydum~ 0+X, data = reg_data1, family = binomial(link = "logit"))
beta_logit = as.matrix(logit$coefficients,ncol=1)
summary(logit)
#Call:
#   glm(formula = ydum ~ 0 + X, family = binomial(link = "logit"), 
#       data = reg_data1)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6399  -0.8082   0.2643   0.7833   2.8365  
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# Xintercept   -1.85821    0.09679 -19.198   <2e-16 ***
#   XX1         2.04048    0.04979  40.978   <2e-16 ***
#   XX2        -1.51703    0.03891 -38.992   <2e-16 ***
#   XX3         0.10534    0.05368   1.962   0.0497 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 13862.9  on 10000  degrees of freedom
# Residual deviance:  9877.4  on  9996  degrees of freedom
# AIC: 9885.4
# 
# Number of Fisher Scoring iterations: 5
##Interpretation for significance
#all the coefficients are significant, and coefficients for X1 and X2 are more significant.
#the results using optim() is quite similar to using glm()

#for linear probability model
beta_lp = XtX_inv%*%t(X)%*%ydum
beta_lp
#                  ydum
# intercept  0.14046325
# X1         0.35664501
# X2        -0.23440275
# X3         0.01666967
##Interpretation
#if X1 increase 1 unit, the probability of y success is likely to increase 0.356645, holding other xi fixed.
#if X2 increase 1 unit, the probability of y success is likely to decrease 0.234403, holding other xi fixed.
#the difference in probability of success when x3=1 and x3=0 is 0.016670, holding other xi fixed.

#check and see significance level using lm function
lp = lm(ydum~0 + X) #linear probability model
beta_lp = data.frame(beta_lp = lp$coef, row.names = c("intercept","X1","X2","X3"))
summary(lp)
# Call:
#   lm(formula = ydum ~ 0 + X)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.07859 -0.34971  0.04563  0.32746  1.16292 
# 
# Coefficients:
#           Estimate Std. Error t value Pr(>|t|)    
# Xintercept    0.140463   0.016574   8.475   <2e-16 ***
#   XX1         0.356645   0.007111  50.156   <2e-16 ***
#   XX2        -0.234403   0.004795 -48.886   <2e-16 ***
#   XX3         0.016670   0.008902   1.873   0.0612 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4102 on 9996 degrees of freedom
# Multiple R-squared:  0.6693,	Adjusted R-squared:  0.6691 
# F-statistic:  5057 on 4 and 9996 DF,  p-value: < 2.2e-16
##Interpretation
#the coefficients for X1 and X2 are significant at high significance level.
#the results using formula is quite similar to using lm()

#Exercise 5
#Calculate marginal effect of probit model （at average level)
mfx_probit = dnorm(mean(X%*%as.matrix(beta_probit)))*as.matrix(beta_probit)
colnames(mfx_probit)=c("marginal_effects_probit")
mfx_probit
#      marginal_effects_probit
# [1,]             -0.43981871
# [2,]              0.48017560
# [3,]             -0.35445764
# [4,]              0.02513593

#Calculate marginal effect of logit model (at average level)
logit_dfunc = function(x){
  exp(x)/((1+exp(x))^2)
}
mfx_logit = logit_dfunc(mean(X%*%as.matrix(beta_logit)))*as.matrix(beta_logit)
colnames(mfx_logit)=c("marginal_effects_logit")
mfx_logit
#      marginal_effects_logit
# [1,]            -0.46453397
# [2,]             0.51009822
# [3,]            -0.37924242
# [4,]             0.02633436

#Calculate probit standard deviations of marginal effects using the delta method
var_cov_probit = vcov(probit) #extract variance-covariance matrix
eps1 = 0.000001 #set epsilon for derivatives
m1b1 = (dnorm(mean(X%*%matrix(c(beta_probit[1,]+eps1,beta_probit[2,],beta_probit[3,],beta_probit[4,]),ncol=1)))*(beta_probit[1,]+eps1)-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[1,])/eps1 #calculate partical derivatives for each ME
m1b2 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,]+eps1,beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[1,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[1,])/eps1 
m1b3 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,]+eps1,beta_probit[4,]),ncol=1)))*beta_probit[1,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[1,])/eps1 
m1b4 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,],beta_probit[4,]+eps1),ncol=1)))*beta_probit[1,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[1,])/eps1
m2b1 = (dnorm(mean(X%*%matrix(c(beta_probit[1,]+eps1,beta_probit[2,],beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[2,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[2,])/eps1 
m2b2 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,]+eps1,beta_probit[3,],beta_probit[4,]),ncol=1)))*(beta_probit[2,]+eps1)-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[2,])/eps1 
m2b3 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,]+eps1,beta_probit[4,]),ncol=1)))*beta_probit[2,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[2,])/eps1 
m2b4 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,],beta_probit[4,]+eps1),ncol=1)))*beta_probit[2,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[2,])/eps1 
m3b1 = (dnorm(mean(X%*%matrix(c(beta_probit[1,]+eps1,beta_probit[2,],beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[3,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[3,])/eps1 
m3b2 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,]+eps1,beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[3,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[3,])/eps1 
m3b3 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,]+eps1,beta_probit[4,]),ncol=1)))*(beta_probit[3,]+eps1)-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[3,])/eps1 
m3b4 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,],beta_probit[4,]+eps1),ncol=1)))*beta_probit[3,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[3,])/eps1 
m4b1 = (dnorm(mean(X%*%matrix(c(beta_probit[1,]+eps1,beta_probit[2,],beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[4,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[4,])/eps1 
m4b2 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,]+eps1,beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[4,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[4,])/eps1 
m4b3 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,]+eps1,beta_probit[4,]),ncol=1)))*beta_probit[4,]-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[4,])/eps1 
m4b4 = (dnorm(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,],beta_probit[4,]+eps1),ncol=1)))*(beta_probit[4,]+eps1)-dnorm(mean(X%*%as.matrix(beta_probit)))*beta_probit[4,])/eps1 
jacobian_1 = cbind(m1b1,m1b2,m1b3,m1b4)
jacobian_2 = cbind(m2b1,m2b2,m2b3,m2b4)
jacobian_3 = cbind(m3b1,m3b2,m3b3,m3b4)
jacobian_4 = cbind(m4b1,m4b2,m4b3,m4b4)
jacobian_prob = rbind(jacobian_1,jacobian_2,jacobian_3,jacobian_4)
cov_mfx_probit_delta = t(jacobian_prob)%*%var_cov_probit%*%jacobian_prob
sd_mfx_probit_delta = sqrt(cbind(cov_mfx_probit_delta[1,1],cov_mfx_probit_delta[2,2],cov_mfx_probit_delta[3,3],cov_mfx_probit_delta[4,4]))
rownames(sd_mfx_probit_delta)=c("sd_mfx_probit_delta")
colnames(sd_mfx_probit_delta)=c("sd_intercept","sd_X1","sd_X2","sd_X3")
sd_mfx_probit_delta
#                     sd_intercept      sd_X1       sd_X2      sd_X3
# sd_mfx_probit_delta    0.0226088 0.01135678 0.008534172 0.01255505

#Calculate logit standard deviations of marginal effects using the delta method
var_cov_logit = vcov(logit)
eps1 = 0.000001 #set epsilon for derivatives
m1b11 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,]+eps1,beta_probit[2,],beta_probit[3,],beta_probit[4,]),ncol=1)))*(beta_probit[1,]+eps1)-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[1,])/eps1 #calculate partical derivatives for each ME
m1b22 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,]+eps1,beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[1,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[1,])/eps1 
m1b33 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,]+eps1,beta_probit[4,]),ncol=1)))*beta_probit[1,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[1,])/eps1 
m1b44 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,],beta_probit[4,]+eps1),ncol=1)))*beta_probit[1,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[1,])/eps1
m2b11 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,]+eps1,beta_probit[2,],beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[2,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[2,])/eps1 
m2b22 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,]+eps1,beta_probit[3,],beta_probit[4,]),ncol=1)))*(beta_probit[2,]+eps1)-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[2,])/eps1 
m2b33 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,]+eps1,beta_probit[4,]),ncol=1)))*beta_probit[2,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[2,])/eps1 
m2b44 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,],beta_probit[4,]+eps1),ncol=1)))*beta_probit[2,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[2,])/eps1 
m3b11 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,]+eps1,beta_probit[2,],beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[3,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[3,])/eps1 
m3b22 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,]+eps1,beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[3,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[3,])/eps1 
m3b33 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,]+eps1,beta_probit[4,]),ncol=1)))*(beta_probit[3,]+eps1)-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[3,])/eps1 
m3b44 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,],beta_probit[4,]+eps1),ncol=1)))*beta_probit[3,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[3,])/eps1 
m4b11 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,]+eps1,beta_probit[2,],beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[4,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[4,])/eps1 
m4b22 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,]+eps1,beta_probit[3,],beta_probit[4,]),ncol=1)))*beta_probit[4,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[4,])/eps1 
m4b33 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,]+eps1,beta_probit[4,]),ncol=1)))*beta_probit[4,]-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[4,])/eps1 
m4b44 = (logit_dfunc(mean(X%*%matrix(c(beta_probit[1,],beta_probit[2,],beta_probit[3,],beta_probit[4,]+eps1),ncol=1)))*(beta_probit[4,]+eps1)-logit_dfunc(mean(X%*%as.matrix(beta_probit)))*beta_probit[4,])/eps1 
jacobian_11 = cbind(m1b11,m1b22,m1b33,m1b44)
jacobian_22 = cbind(m2b11,m2b22,m2b33,m2b44)
jacobian_33 = cbind(m3b11,m3b22,m3b33,m3b44)
jacobian_44 = cbind(m4b11,m4b22,m4b33,m4b44)
jacobian_log = rbind(jacobian_11,jacobian_22,jacobian_33,jacobian_44)
cov_mfx_logit_delta = t(jacobian_log)%*%var_cov_logit%*%jacobian_log
sd_mfx_logit_delta = sqrt(cbind(cov_mfx_logit_delta[1,1],cov_mfx_logit_delta[2,2],cov_mfx_logit_delta[3,3],cov_mfx_logit_delta[4,4]))
rownames(sd_mfx_logit_delta)=c("sd_mfx_logit_delta")
colnames(sd_mfx_logit_delta)=c("sd_intercept","sd_X1","sd_X2","sd_X3")
sd_mfx_logit_delta
#                    sd_intercept      sd_X1       sd_X2     sd_X3
# sd_mfx_logit_delta   0.02412133 0.01261186 0.009696985 0.0134242

#Calculate probit standard deviations of marginal effects using bootstrap (average level)
r3 = 499 #rep=499
bk = matrix(c(0.5,0.5,0.5,0.5),ncol=1) #set default value of beta
mfx_probit_boot3 = matrix(c(0,0,0,0),nrow=1)
mfx_probit_boot3 = matrix(mfx_probit_boot3[rep(1:nrow(mfx_probit_boot3),each=r3),],nrow=r3) #create a default 4*49 matrix
for (b in 1:r3) {
  i = sample(1:10000, size = 10000, replace = TRUE) #subsampling
  Yd1 = as.matrix(reg_data1[i,1])
  X1 = reg_data1[i,3]
  X2 = reg_data1[i,4]
  X3 = reg_data1[i,5]
  Xp = as.matrix(cbind(intercept,X1,X2,X3))
  beta_probit = optim(bk,pro_ll_1,x=Xp, y=Yd1,method = "BFGS")$par
  mfx_probit_boot3[b,] = dnorm(mean(Xp%*%as.matrix(beta_probit)))*as.matrix(beta_probit)
}
sd_mfx_probit_boot3 = cbind(sd(mfx_probit_boot3[,1]),sd(mfx_probit_boot3[,2]),sd(mfx_probit_boot3[,3]),sd(mfx_probit_boot3[,4])) #report the standard error 
rownames(sd_mfx_probit_boot3)=c("sd_mfx_probit_bootstrap")
colnames(sd_mfx_probit_boot3)=c("sd_intercept","sd_X1","sd_X2","sd_X3")
sd_mfx_probit_boot3
#did not show the output here since the sample() will sample differently every time, so the results will be slightly different

#Calculate logit standard deviations of marginal effects using bootstrap (average level)
r4 = 499 #rep=499
mfx_logit_boot3 = matrix(c(0,0,0,0),nrow=1)
mfx_logit_boot3 = matrix(mfx_logit_boot3[rep(1:nrow(mfx_logit_boot3),each=r4),],nrow=r4) #create a default 4*49 matrix
for (b in 1:r4) {
  i = sample(1:10000, size = 10000, replace = TRUE) #subsampling
  Yd2 = as.matrix(reg_data1[i,1])
  X1 = reg_data1[i,3]
  X2 = reg_data1[i,4]
  X3 = reg_data1[i,5]
  Xl = as.matrix(cbind(intercept,X1,X2,X3))
  beta_logit = optim(bk,log_ll,x=Xl, y=Yd2,method = "BFGS")$par
  mfx_logit_boot3[b,] = logit_dfunc(mean(Xl%*%as.matrix(beta_logit)))*as.matrix(beta_logit)
}
sd_mfx_logit_boot3 = cbind(sd(mfx_logit_boot3[,1]),sd(mfx_logit_boot3[,2]),sd(mfx_logit_boot3[,3]),sd(mfx_logit_boot3[,4])) #report the standard error 
rownames(sd_mfx_logit_boot3)=c("sd_mfx_logit_bootstrap")
colnames(sd_mfx_logit_boot3)=c("sd_intercept","sd_X1","sd_X2","sd_X3")
sd_mfx_logit_boot3
#did not show the output here since the sample() will sample differently every time, so the results will be slightly different