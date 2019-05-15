dat = read.csv("~/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/4/Koop-Tobias.csv")

#=========================================================================
#Exercise 1 Data
#=========================================================================
sam_id = sample(1:2178,5)
sam_wage1 = dat[which(dat[,1] == sam_id[1]),][,c(5,3)]
plot(sam_wage1,type = "l")
sam_wage2 = dat[which(dat[,1] == sam_id[2]),][,c(5,3)]
plot(sam_wage2,type = "l")
sam_wage3 = dat[which(dat[,1] == sam_id[3]),][,c(5,3)]
plot(sam_wage3,type = "l")
sam_wage4 = dat[which(dat[,1] == sam_id[4]),][,c(5,3)]
plot(sam_wage4,type = "l")
sam_wage5 = dat[which(dat[,1] == sam_id[5]),][,c(5,3)]
plot(sam_wage5,type = "l")

#=========================================================================
#Exercise 2 Random Effects
#=========================================================================
library(nlme) 
logwage = as.matrix(dat[,3]) #extract data
educ= as.matrix(dat[,2])
potexpr = as.matrix(dat[,4])
model1 = gls(logwage ~ 1 + educ + potexpr) #use gls estimation
summary(model1)

#=========================================================================
#Exercise 3 Fixed Effects
#=========================================================================
#between estimator
meanlogwage = aggregate(cbind(LOGWAGE,EDUC,POTEXPER) ~ PERSONID, data = dat, mean) #calculate mean
colnames(meanlogwage) = c("PERSONID","MEAN_LOGWAGE","MEAN_EDUC","MEAN_POTEPER")
model2 = lm(meanlogwage[,2] ~ meanlogwage[,3] + meanlogwage[,4])
summary(model2)

#within estimator
dat1 = merge(dat,meanlogwage)
d_logwage = dat1[,3]-dat1[,11] #decompose
d_educ = dat1[,2]-dat1[,12]
d_potexper = dat1[,4]-dat1[,13]
model3 = lm(d_logwage ~ d_educ + d_potexper - 1)
summary(model3)

#first difference estimator
dat2 = dat[,c(1,3,2,4)]
personid1 = matrix(c(0,dat2[1:17918,1]),ncol=1)
dat2[,1] = dat2[,1]-personid1
logwage1 = matrix(c(0,logwage[1:17918]),ncol=1)
dat2[,2] = dat2[,2]-logwage1
educ1 = matrix(c(0,educ[1:17918,]),ncol=1) 
dat2[,3] = dat2[,3]-educ1
potexpr1 = matrix(c(0,potexpr[1:17918]),ncol=1) 
dat2[,4] = dat2[,4]-potexpr1
personid = dat2[,1]
personid[personid==1] = NA
dat2[,1] = personid
dat2 = na.omit(dat2)
model4 = lm(dat2[,2] ~ dat2[,3] + dat2[,4])
summary(model4)

#=========================================================================
#Exercise 4 Fixed Effects
#=========================================================================
#the likelihood and estimate for the individual fixed effect parameters
sam_id2 = sample(1:2178,100)
dat3 = dat[dat[,1]%in%sam_id2,]
pro_ll_1 = function(b,x,y) #define the minus probit likelihood function
{
  f = -sum(y*log(pnorm(x%*%b)))-sum((1-y)*log(1-pnorm(x%*%b)))
  return(f)
}
Y = as.matrix(dat3[,3])
X = as.matrix(dat3[,c(2,4)])
beta_ml = optim(c(0,0),pro_ll_1,x=X, y=Y)$par #return the beta

#regression of estimated individual fixed effets on the invariant variables
mean = aggregate(cbind(LOGWAGE,EDUC,POTEXPER) ~ PERSONID, data = dat3, mean)
alpha = matrix(mean[,2] - (mean[,3]*beta_ml[1]+mean[,4]*beta_ml[2]),ncol=1) #estimate individual fixed effect
dat4 = dat3[!duplicated(dat3$PERSONID),]
X2 = as.matrix(dat4[,6:10])
model5 = lm(alpha ~ X2)
summary(model5)

#Reasons for alternative method to compute standard errors
#1.the errors are potentially serially correlated (i.e., correlated over t for given i) 
#2.heteroskedastic

#Use bootstrap
rm(mean)
boot = matrix(0,nrow=49,ncol=6)
for (b in 1:49) {
  i = matrix(sample(unique(dat3[,1]),100,replace=TRUE),ncol=1) #subsampling
  d = matrix(0,ncol=10,nrow=1)
  for (c in 1:100){
    d = rbind(d,as.matrix(dat3[dat3[,1]%in%i[c,],]))
  }
  dat5 = as.data.frame(d[2:nrow(d),])
  Y_boot = as.matrix(dat5[,3])
  X_boot = as.matrix(dat5[,c(2,4)])
  beta_boot = optim(c(0,0),pro_ll_1,x=X_boot, y=Y_boot)$par #return the beta
  mean_boot = aggregate(cbind(LOGWAGE,EDUC,POTEXPER) ~ PERSONID, data = dat5, mean)
  alpha_boot = matrix(mean_boot[,2] - (mean_boot[,3]*beta_boot[1]+mean_boot[,4]*beta_boot[2]),ncol=1)
  dat6 = dat5[!duplicated(dat5$PERSONID),]
  X2_boot = as.matrix(dat6[,6:10])
  model6 = lm(alpha_boot ~ X2_boot)$coef
  boot[b,] = model6
}
sd_boot = cbind(sd(boot[,1]),sd(boot[,2]),sd(boot[,3]),sd(boot[,4]),sd(boot[,5]),sd(boot[,6]))
colnames(sd_boot) = c("Intercept","ABILITY","MOTHERED","FATHERED","BRKNHOME","SIBLINGS")
rownames(sd_boot) = c("corrected_sd_bootstrap")
sd_boot

