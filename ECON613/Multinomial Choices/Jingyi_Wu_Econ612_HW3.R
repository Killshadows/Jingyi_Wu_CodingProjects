#import data
demos = read.csv("~/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/3/demos.csv")
product = read.csv("~/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/3/product.csv")

#==============================================================================
#Exercise 1 Data Description
#==============================================================================
#Average and dispersion in product characteristics
meanprice = as.data.frame(apply(as.matrix(product[,4:13]), 2, mean))
print(meanprice)
varprice = as.data.frame(apply(as.matrix(product[,4:13]), 2, var))
print(varprice)

#Market share, and market share by product characteristics
mschoice = as.data.frame(table(product[,3])/dim(product)[1]) #calculate market share by product
rownames(mschoice) = c("Pk_Stk","BB_Stk","Fl_Stk ","Hse_Stk","Gen_Stk","Imp_Stk","SS_Tub","Pk_Tub","Fl_Tub","Hse_Tub")
colnames(mschoice) = c("choice","market share")
print(mschoice)
mschoice_brand = as.data.frame(rbind(mschoice[1,2]+mschoice[8,2],mschoice[2,2],mschoice[3,2]+mschoice[9,2],mschoice[4,2]+mschoice[10,2],mschoice[5,2],mschoice[6,2],mschoice[7,2])) #calculate market share by brand
rownames(mschoice_brand) = c("Pk","BB","Fl ","Hse","Gen","Imp","SS")
colnames(mschoice_brand) = c("market share by brand")
print(mschoice_brand)
mschoice_type = as.data.frame(rbind(mschoice[1,2]+mschoice[2,2]+mschoice[3,2]+mschoice[4,2]+mschoice[5,2]+mschoice[6,2],mschoice[7,2]+mschoice[8,2]+mschoice[9,2]+mschoice[10,2])) #calculate market share by type
rownames(mschoice_type) = c("stick","tub")
colnames(mschoice_type) = c("market share by type")
print(mschoice_type)

#Mapping between observed attributes and choices
map = as.data.frame(product[,2:3])
map = merge(map, demos, by.map = hhid, by.demos = hhid, all = FALSE, sort=TRUE) #mapping income and choice by id
map = map[,c(1,2,4)]
Pk_Stk = data.frame(Pk_Stk = as.numeric(map$choice == 1)) #mapping choices
BB_Stk = data.frame(BB_Stk = as.numeric(map$choice == 2))
Fl_Stk = data.frame(Fl_Stk = as.numeric(map$choice == 3))
Hse_Stk = data.frame(Hse_Stk = as.numeric(map$choice == 4))
Gen_Stk = data.frame(Gen_Stk = as.numeric(map$choice == 5))
Imp_Stk = data.frame(Imp_Stk = as.numeric(map$choice == 6))
SS_Tub = data.frame(SS_Tub = as.numeric(map$choice == 7))
Pk_Tub = data.frame(Pk_Tub = as.numeric(map$choice == 8))
Fl_Tub = data.frame(Fl_Tub = as.numeric(map$choice == 9))
Hse_Tub = data.frame(Hse_Tub = as.numeric(map$choice == 10))
map = cbind(map,Pk_Stk,BB_Stk,Fl_Stk,Hse_Stk,Gen_Stk,Imp_Stk,SS_Tub,Pk_Tub,Fl_Tub,Hse_Tub)
decompose = aggregate(cbind(Pk_Stk,BB_Stk,Fl_Stk,Hse_Stk,Gen_Stk,Imp_Stk,SS_Tub,Pk_Tub,Fl_Tub,Hse_Tub) ~ Income, data = map, sum) #sum each product by income level
decompose = cbind(decompose[,1],prop.table(as.matrix(decompose[,2:11]),margin = 1)) #calculate market share
colnames(decompose) = c("Income","Pk_Stk","BB_Stk","Fl_Stk ","Hse_Stk","Gen_Stk","Imp_Stk","SS_Tub","Pk_Tub","Fl_Tub","Hse_Tub")
print(decompose)

#==============================================================================
##Exercise 2 First Model
#==============================================================================
#use a conditional logit model
dep = as.data.frame(product[,4:13]) #extract dependent variable
depbar = as.data.frame(rep(as.data.frame(dep[,1]),each = 10)) #replicate for 10 times
dep = as.matrix(dep - depbar) #substract first column
dum = as.matrix(map[,4:13]) #generate dummy matrix
clogit = function(theta){
  alpha = matrix(c(0,theta[2:10]),nrow=1)
  alpha = alpha[rep(1:nrow(alpha),times=4470),] #create alpha matrix
  v = dep*as.numeric(theta[1])+alpha #create utility matrix
  v_exp = as.matrix(exp(v)) #take exp
  p = prop.table(v_exp,margin=1) #create market share
  likelihood = sum(log(p)*dum) #calculate likelihood
  return(-likelihood)
}
para1 = as.matrix(optim(c(0,0,0,0,0,0,0,0,0,0),clogit,method = "BFGS")$par) #optim likelihood to get estimate parameters
rownames(para1) = c("price","intercept2","intercept3","intercept4","intercept5","intercept6","intercept7","intercept8","intercept9","intercept10")
print(para1)
#Interpretation:
#The coefficient on price is -6.6566340 (negative)
#It means that if the price goes up, people are less likely to buy margarine

#==============================================================================
#Exercise 3 Second Model
#==============================================================================
income = as.data.frame(rep(as.data.frame(map[,3]),each=10)) #replicate income column for 10 times
mlogit = function(gamma){
  beta = matrix(c(0,gamma[1:9]),nrow=1)
  beta = beta[rep(1:nrow(beta),times=4470),] #create beta matrix
  alpha = matrix(c(0,gamma[10:18]),nrow=1)
  alpha = alpha[rep(1:nrow(alpha),times=4470),] #create alpha matrix
  v = income*beta+alpha #calculate utility matrix
  v_exp = as.matrix(exp(v))
  p = prop.table(v_exp,margin=1) #calculate market share matrix
  likelihood = sum(log(p)*dum) #calculate likelihood
  return(-likelihood)
}
para2 = as.matrix(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),mlogit,method = "BFGS")$par) #optim likelihood to get estimate parameters
rownames(para2) = c("income2","income3","income4","income5","income6","income7","income8","income9","income10","intercept2","intercept3","intercept4","intercept5","intercept6","intercept7","intercept8","intercept9","intercept10")
print(para2)
#Interpretation:
#The coefficient on income2 is -0.003156338 (negative)
#It means that if the income goes up, people are less likely to buy product2 comparing to product1
#The coefficient on income2 is 0.014507166 (positive)
#It means that if the income goes up, people are more likely to buy product3 comparing to product1

#==============================================================================
#Exercise 4 Marginal Effects
#==============================================================================
#calculate marginal effects for model1 
alpha1 = matrix(c(0,para1[2:10]),nrow=1) #substitute estimated para to get market share matrix p
alpha1 = alpha1[rep(1:nrow(alpha1),times=4470),]
v_exp = as.matrix(exp(dep*as.numeric(para1[1])+alpha1)) 
p = prop.table(v_exp,margin=1) 
p1 = as.data.frame(rep(as.data.frame(p[,1]),each=10)) #replicate to do elementory wise product
p2 = as.data.frame(rep(as.data.frame(p[,2]),each=10))
p3 = as.data.frame(rep(as.data.frame(p[,3]),each=10))
p4 = as.data.frame(rep(as.data.frame(p[,4]),each=10))
p5 = as.data.frame(rep(as.data.frame(p[,5]),each=10))
p6 = as.data.frame(rep(as.data.frame(p[,6]),each=10))
p7 = as.data.frame(rep(as.data.frame(p[,7]),each=10))
p8 = as.data.frame(rep(as.data.frame(p[,8]),each=10))
p9 = as.data.frame(rep(as.data.frame(p[,9]),each=10))
p10 = as.data.frame(rep(as.data.frame(p[,10]),each=10))
dum_p = dum - p
ave_me1 = as.matrix(apply((p1*dum_p)*para1[1], 2, mean)) #calculate average marginal effect for product 1
ave_me2 = as.matrix(apply((p2*dum_p)*para1[1], 2, mean))
ave_me3 = as.matrix(apply((p3*dum_p)*para1[1], 2, mean))
ave_me4 = as.matrix(apply((p4*dum_p)*para1[1], 2, mean))
ave_me5 = as.matrix(apply((p5*dum_p)*para1[1], 2, mean))
ave_me6 = as.matrix(apply((p6*dum_p)*para1[1], 2, mean))
ave_me7 = as.matrix(apply((p7*dum_p)*para1[1], 2, mean))
ave_me8 = as.matrix(apply((p8*dum_p)*para1[1], 2, mean))
ave_me9 = as.matrix(apply((p9*dum_p)*para1[1], 2, mean))
ave_me10 = as.matrix(apply((p10*dum_p)*para1[1], 2, mean))
ave_me_model1 = cbind(ave_me1,ave_me2,ave_me3,ave_me4,ave_me5,ave_me6,ave_me7,ave_me8,ave_me9,ave_me10)
rownames(ave_me_model1) = c("price_product1","price_product2","price_product3","price_product4","price_product5","price_product6","price_product7","price_product8","price_product9","price_product10")
colnames(ave_me_model1) = c("demand_product1","demand_product2","demand_product3","demand_product4","demand_product5","demand_product6","demand_product7","demand_product8","demand_product9","demand_product10")
#Interpretation:
#The average marginal effect of price of product1 on demand of product1 is -0.005445123
#It means that if the price of product 1 increase 1 unit, people will on average decrease their probability purchasing product1 by 0.005445123
#The average marginal effect of price of product1 on demand of product2 is 0.0094415942
#It means that if the price of product 1 increase 1 unit, people will on average increase their probability purchasing product2 by 0.0094415942

#calculate marginal effects for model2
beta0 = matrix(c(0,para2[1:9]),nrow=1) #substitute estimated para to get market share matrix p22
beta = beta0[rep(1:nrow(beta0),times=4470),] 
alpha2 = matrix(c(0,para2[10:18]),nrow=1)
alpha2 = alpha2[rep(1:nrow(alpha2),times=4470),] 
v2 = income*beta+alpha2 
v_exp2 = as.matrix(exp(v2))
p22 = prop.table(v_exp2,margin=1)
betabar = as.matrix(apply(p22*beta, 1, sum)) #calculate average beta matrix for i from 1 to 4470
ave_me21 = mean(as.matrix(p22[,1]*(beta0[,1]-betabar))) #calculate average marginal effect of income on product 1
ave_me22 = mean(as.matrix(p22[,2]*(beta0[,2]-betabar)))
ave_me23 = mean(as.matrix(p22[,3]*(beta0[,3]-betabar)))
ave_me24 = mean(as.matrix(p22[,4]*(beta0[,4]-betabar)))
ave_me25 = mean(as.matrix(p22[,5]*(beta0[,5]-betabar)))
ave_me26 = mean(as.matrix(p22[,6]*(beta0[,6]-betabar)))
ave_me27 = mean(as.matrix(p22[,7]*(beta0[,7]-betabar)))
ave_me28 = mean(as.matrix(p22[,8]*(beta0[,8]-betabar)))
ave_me29 = mean(as.matrix(p22[,9]*(beta0[,9]-betabar)))
ave_me210 = mean(as.matrix(p22[,10]*(beta0[,10]-betabar)))
ave_me_model2 = matrix(c(ave_me21,ave_me22,ave_me23,ave_me24,ave_me25,ave_me26,ave_me27,ave_me28,ave_me29,ave_me210),ncol=1)
rownames(ave_me_model2) = c("demand_product1","demand_product2","demand_product3","demand_product4","demand_product5","demand_product6","demand_product7","demand_product8","demand_product9","demand_product10")
colnames(ave_me_model2) = c("income")
#Interpretation:
#The average marginal effect of income on demand of product1 is -0.0010504137
#It means that if income increase by 1 unit, people will on average decrease their probability purchasing product1 by 0.0010504137

#==============================================================================
#Exercise 5 Marginal Effects
#==============================================================================
#Mixed Logit model
mixlogit1 = function(delta){
  alpha = matrix(c(0,delta[11:19]),nrow=1)
  alpha = alpha[rep(1:nrow(alpha),times=4470),] #create alpha matrix
  beta = matrix(c(0,delta[2:10]),nrow=1) 
  beta = beta[rep(1:nrow(beta),times=4470),] #create multinomial beta matrix
  v = dep*as.numeric(delta[1])+income*beta+alpha #create utility matrix
  v_exp = as.matrix(exp(v))
  p = prop.table(v_exp,margin=1) #calculate market share matrix
  likelihood = sum(log(p)*dum) #calculate likelihood
  return(-likelihood)
}
para3 = as.matrix(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),mixlogit1,method = "BFGS")$par) #extract estimated para for unsubset model
rownames(para3) = c("price","income2","income3","income4","income5","income6","income7","income8","income9","income10","intercept2","intercept3","intercept4","intercept5","intercept6","intercept7","intercept8","intercept9","intercept10")

#Subset mixed logit model (Remove choice 10)
map[map==10] = NA #remove choice 10 from income data
map2 = na.omit(map)[,3:12]
income2 = as.data.frame(rep(as.data.frame(map2[,1]),each=9))
dum2 = map2[,2:10] #remove choice 10 from dummy matrix
dep2 = as.data.frame(product[,3:13]) #remove choice 10 from price matrix
dep2[dep2==10] = NA
dep2 = na.omit(dep2)[,2:10]
depbar2 = as.data.frame(rep(as.data.frame(dep2[,1]),each = 9)) #replicate for 10 times
dep2 = as.matrix(dep2 - depbar2) #substract first column
mixlogit2 = function(delta){
  alpha = matrix(c(0,delta[10:17]),nrow=1)
  alpha = alpha[rep(1:nrow(alpha),times=4437),] #create alpha matrix
  beta = matrix(c(0,delta[2:9]),nrow=1)
  beta = beta[rep(1:nrow(beta),times=4437),] #create beta matrix
  v = dep2*as.numeric(delta[1])+income2*beta+alpha #create utility matrix
  v_exp = as.matrix(exp(v))
  p = prop.table(v_exp,margin=1) #calculate market share matrix
  likelihood = sum(log(p)*dum2) #calculate likelihood
  return(-likelihood)
}
para4 = as.matrix(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),mixlogit2,method = "BFGS")$par) #extract estimated para for subset model
rownames(para4) = c("price","income2","income3","income4","income5","income6","income7","income8","income9","intercept2","intercept3","intercept4","intercept5","intercept6","intercept7","intercept8","intercept9")
Lr_f = -mixlogit2(para3[c(1:9,11:18),]) #calculate likelihood for unsubset model
Lr_r = -mixlogit2(para4) #calculate likelihood for subset model
MTT = -2*(Lr_f-Lr_r) #calculate MTT test statistics
#Conclusion:
#MTT = 0.00627
#Critical Value of chi2(9) at 5% significance level = 16.92
#MTT < CV
#There is not a significant different, so IIA is not violated 
#Probably because the choice I removed (choice 10) is of little marketshare - 0.74%, so removing it will not casue big influence on the market




