clear;  
close all; 
clc;

%% Exercise 1
%(a) see function for detail

%(b) see function for detail

%(c)
L = 20; %set Lag = 20

SP = csvread('^GSPC.csv', 1, 0);
P_SP = SP(:, 6); 
ret_SP = log(P_SP(2:end)./P_SP(1:end-1)); %calculate log return
ret_SP_sqr = ret_SP.^2; %calculate log return squared

lb_SP = LBtest(ret_SP,L); %return lbtest for SP return
p_SP = lb_SP.p(2:end)'; %return sample correlation for SP return
lb_SP_sqr = LBtest(ret_SP_sqr,L); %return lbtest for SP return squared
p_SP_sqr = lb_SP_sqr.p(2:end)'; %return sample correlationfor SP return squared

plot(p_SP,'p:','MarkerSize',10) %plotting
hold on 
plot(p_SP_sqr,'o:','MarkerSize',10)
hold off

%same for Exchange rate
Ex = csvread('Exc.csv', 1, 0);
P_Ex = Ex(:, 5);
ret_Ex = log(P_Ex(2:end)./P_Ex(1:end-1));
ret_Ex_sqr = ret_Ex.^2;

lb_Ex = LBtest(ret_Ex,L);
p_Ex = lb_Ex.p(2:end)';
lb_Ex_sqr = LBtest(ret_Ex_sqr,L);
p_Ex_sqr = lb_Ex_sqr.p(2:end)';

plot(p_Ex,'p:','MarkerSize',10)
hold on 
plot(p_Ex_sqr,'o:','MarkerSize',10)
hold off

%same for 3-month T-bill
Tbill = csvread('Tbill.csv', 1, 0);
P_Tbill = Tbill(:, 2);
P_Tbill = 100./(1+P_Tbill).^0.25;
ret_Tbill = log(P_Tbill(2:end)./P_Tbill(1:end-1));
ret_Tbill_sqr = ret_Tbill.^2;

lb_Tbill = LBtest(ret_Tbill,L);
p_Tbill = lb_Tbill.p(2:end)';
lb_Tbill_sqr = LBtest(ret_Tbill_sqr,L);
p_Tbill_sqr = lb_Tbill_sqr.p(2:end)';

plot(p_Tbill,'p:','MarkerSize',10)
hold on 
plot(p_Tbill_sqr,'o:','MarkerSize',10)
hold off

%% Exercise 2
%(a) see funtion for detail

%(b)
[X,FVAL,EXITFLAG] = fminunc(@x3,2)

%(c)
A = -1;
b = -1;
[X,FVAL,EXITFLAG] = fmincon('x3', 3, A, b)

%(d) see function for detail

%(e)
[X,FVAL,EXITFLAG] = fminunc(@x4,0)

%(f)
[X,FVAL,EXITFLAG] = fminunc(@x4,1)

%(g)
fplot(@x4,[-2.5,2.5]);

%% Exercise 3
%(a) see function for detail

%(b)
%Generate data set(sample) from a normal distribution with mean 1 and standard deviation 2.
rng(1);
R = 1 + 2. *randn(1000,1);
mean_R = mean(R) %calculate sample mean
var_R = var(R) %calculate sample variance

X = R; %insert sample
x0 = [0,1]; %set starting value
A = [0,-1]; %set constraint
b = 0;
Aeq = []; %set non-equal constraint
beq = [];
theta = fmincon(@(theta) LL_normal(theta,X) , x0, A, b, Aeq, beq) %maxmize Likelihood

%% Exercise 4
%see function for detail
SP = csvread('^GSPC.csv', 1, 0); %import data
P_SP = SP(:, 6); 
ret_SP = log(P_SP(2:end)./P_SP(1:end-1));
demeaned_ret_SP = ret_SP - mean(ret_SP);

data = demeaned_ret_SP;
theta = [0.1; 0.4; 0.5];
sigma2 = garch_variance(theta,data)';
plot(sigma2);

%% Exercise 5

SP500 = csvread('SP_from10to17.csv', 1, 1);
SP500 = 100*SP500(:, 7);

results = nwest(SP500, ones(length(SP500),1), 5);
resid = SP500 - results.yhat;

addpath('/Users/killshadows/Desktop/DUKE/COURSES/SPRING2019/ECON623/TA Session/3/mfe-toolbox-master/univariate')
addpath('/Users/killshadows/Desktop/DUKE/COURSES/SPRING2019/ECON623/TA Session/3/mfe-toolbox-master/distributions')

parameters = tarch(resid,1,0,1);

sigmasqr = NaN(length(resid)+1, 1);
sigmasqr(1) = var(resid);

omega = parameters(1);
alpha = parameters(2);
beta = parameters(3);

for i = 2:length(resid)+1
    
    sigmasqr(i) = omega + alpha*resid(i-1)^2 + beta*sigmasqr(i-1);

end

annual_sd = sqrt(252)*sigmasqr;
plot(annual_sd);


