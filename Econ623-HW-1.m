clear;  
close all; 
clc;


%% Exercise 1 

%Import a time series of daily prices on two financial assets (S&P500 and APPLE)
SP = csvread('^GSPC.csv', 1, 0);
AAPL = csvread('AAPL.csv', 1, 0);

% (a)
%Calculate the continuously compounded returns of S&P500
P_SP = SP(:, 6); 
ret_SP = log(P_SP(2:end)./P_SP(1:end-1));
 
%Calculate the continuously compounded returns of APPLE
P_AAPL = AAPL(:, 6);
ret_AAPL = log(P_AAPL(2:end)./P_AAPL(1:end-1))
 
% Add xlabel and ylabel 
figure(1)
plot(ret_SP)
title('Continuously compounded returns, SP')
xlim([1 length(ret_SP)]) % Set the x axis limit
 
% Add xlabel and ylabel 
figure(2)
plot(ret_AAPL)
title('Continuously compounded returns, AAPL')
xlim([1 length(ret_AAPL)]) % Set the x axis limit

% (b)
% Calculate mean, median, ... for two financial assets series data
[MeanOut, MedianOut, MaxOut, MinOut, StDOut, SkewOut, KurtOut, JB_t, JB_p] = sum_stats(P_SP)
sum_SP = [MeanOut, MedianOut, MaxOut, MinOut, StDOut, SkewOut, KurtOut, JB_t, JB_p]'
 
[MeanOut, MedianOut, MaxOut, MinOut, StDOut, SkewOut, KurtOut, JB_t, JB_p] = sum_stats(P_AAPL)
sum_AAPL = [MeanOut, MedianOut, MaxOut, MinOut, StDOut, SkewOut, KurtOut, JB_t, JB_p]'
 
% Save data into one table
sum_stats = table(sum_SP, sum_AAPL)
sum_stats.Properties.RowNames = {'Mean' 'Median' 'Max' 'Min' 'StandardDeviation' 'Skewness' 'Kurtosis' 'JB_t' 'JB_p'};

%% Exercise 2 

% (a)
% Construct the regression
Intercept = ones(size(ret_AAPL(1:end-1), 1), 1);
X = [Intercept ret_AAPL(1:end-1)];
Y = ret_SP(2:end);
 
results = ols(Y,X);
 
% Report the estimated parameters, their standard errors, their t-statistics, and the R2 and
% R2 adj from this regression
Beta = results.beta
StandardError = results.bstd 
Tstatistics = results.tstat
Rsquared = results.rsqr
Rbarsquared = results.rbar

% (b)
%Calculate Tstatistics
tstat_b = (results.beta(2) - 1)/results.bstd(2)
%Calculate Critical Value for 5% significance level
Critical_values = [norminv(0.025) norminv(0.975)]
%Test if absolute value of Tstatistics is less than the Critical Value
abs(tstat_b) < norminv(0.975) 

% (c)
% Construct the regression
Indep = [ret_AAPL(3:end-1) ret_AAPL(2:end-2) ret_AAPL(1:end-3)];
Intercept = ones(size(Indep, 1), 1);
X = [Intercept Indep];
Y = ret_SP(4:end);
 
results_c = ols(Y,X);
 
% Report the estimated parameters, their standard errors, their t-statistics, and the R2 and
% R2 adj from this regression
Beta_c = results_c.beta
StandardError_c = results_c.bstd 
Tstatistics_c = results_c.tstat
Rsquared_c = results_c.rsqr
Rbarsquared_c = results_c.rbar

% (d)
% Calculate needed value for chi^2 test
Betas_c = results_c.beta(2:end);
XXinv = (X'*X)\eye(4);
CovarianceMatrix = XXinv*var(results_c.resid);
CovarianceMatrixBetas = CovarianceMatrix(2:end, 2:end); % Drop the constant
R = [1 0 0
     0 1 0
     0 0 1];
%Calculate test statistics
 TestValue_d = (R*Betas_c - [0; 0; 0])'*((R'*CovarianceMatrixBetas*R)\(R*Betas_c - [0; 0; 0]))
%Calculate Critical Value for 5% significance level
CriticalValue_d = chi2inv(1-0.05,3) 
%Test if test statistics is less than the Critical Value
TestValue_d < chi2inv(1-0.05,3)

% (e)
Betas_d = results_c.beta(2:end);
XXinv = (X'*X)\eye(4);
CovarianceMatrix = XXinv*var(results_c.resid);
CovarianceMatrixBetas = CovarianceMatrix(2:end, 2:end); % Drop the constant
R = [1 -1 0
     1 0 -1];
%Calculate test statistics
TestValue_d = (R*Betas_d - [0; 0])'*((R*CovarianceMatrixBetas*R')\(R*Betas_d - [0; 0]))
%Calculate Critical Value for 5% significance level
CriticalValue_d = chi2inv(1-0.05,2) 
%Test if test statistics is less than the Critical Value
TestValue_d < chi2inv(1-0.05,2)

%% Excercise 3

% (a)
% see function

% (b)
%Assign value to theta and T, and plot the result
theta = [0 0.8 1];
T = 250;
Y = AR1(theta, T);
plot(Y)
title('Simulated Time Series')

% (c)
%Calculate sample autocorrelation
autocorrY = autocorr(Y, 20); 
autocorrY_1 = autocorrY(2:21,:);

%Calculate theoretical autocorrelation
for i = 1:20
    phi1 = 0.8;
    autocorrY_2(i) = phi1^(i);
end

%Plotting
plot(autocorrY_1)
hold on
plot(autocorrY_2)
hold off
title('Sample and Theoretical Autocorrelation')
xlabel('lag(i)')
ylabel('Autocorrelation')


%% Exercise 4

% (a)
% See function
[X,FVAL,EXITFLAG] = fminunc(@x3,2)
% (b)
%Assign value to theta and T, and plot the result
theta = [0 0.8 -1 1];
T = 250;
X = AR1simU(theta, T);
plot(X)
title('Simulated Time Series')

% (d)
% (i) Assign value to theta and T, and compute the sample mean
theta = [0 0.8 -1 1];
T = 100000;
X1 = AR1simU(theta, T);
mean(X1)

% (ii) Assign value to theta and T, and compute the sample mean
theta = [0 0.8 -2 1];
T = 100000;
X2 = AR1simU(theta, T);
mean(X2)

% (iii) Assign value to theta and T, and compute the sample mean
theta = [1 0.8 -5 2];
T = 100000;
X3 = AR1simU(theta, T);
mean(X3)







