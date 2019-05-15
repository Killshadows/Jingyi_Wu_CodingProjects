log using "/Users/killshadows/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/5/Jingyi_Wu_Econ613_HW#5.smcl
**===========================================================
**HW2
**===========================================================
clear

**Exercise 1 
*generate data
set seed 100 
set obs 10000
gen x1 = runiform(1,3)
gen x2 = rgamma(3,2)
gen x3 = rbinomial(1,0.3)
gen eps = rnormal(2,1)
gen y = 0.5+1.2*x1-0.9*x2+0.1*x3+eps
egen a = mean(y)
gen ydum = y
replace ydum = 1 if ydum > a
replace ydum = 0 if y <= a

**Exercise 2 
*calculate correlation
correlate y x1 
*OLS regression
regress y x1 x2 x3
*bootstrap
bootstrap, reps(49): regress y x1 x2 x3
bootstrap, reps(499): regress y x1 x2 x3

**Exercise 4 & 5
*probit model
probit ydum x1 x2 x3
*margin effect for probit model
margins, dydx(*) atmeans
*logit model
logit ydum x1 x2 x3
*margin effect for logit model
margins, dydx(*) atmeans

**i don't know how to do bootstrap for marginal effect.....

**===========================================================
**HW3
**===========================================================
clear all
set more off, perm
set scrollbufsize 2000000

*Exercise 1
insheet using /Users/killshadows/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/3/product.csv
sort hhid
save data, replace
clear
insheet using /Users/killshadows/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/3/demos.csv
save datanew, replace
sort hhid
merge hhid using data.dta
*average and dispersion in product characteristics
summarize ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub
*market share
tabulate choice
tabulate income choice

**Exercise 2&3&4&5
clear
*merge two datasets
insheet using /Users/killshadows/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/3/product.csv
sort hhid
save data, replace
clear
insheet using /Users/killshadows/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/3/demos.csv
save datanew, replace
sort hhid
merge hhid using data.dta

*drop irrelavent data
drop fs3_4 fs5 fam_size college whtcollar retired hhid v1
*generate unique id for conditional logit 
gen n = 4410
gen v1 = _n
*rename for later call
rename (ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub)(c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
*reshape data for conditional logit
reshape long c, i(v1) j(price)
*generate dummy variable for choice
gen dum = cond(price == choice, 1, 0)

*do conditional logit model
asclogit dum c, case(v1) alternatives(price)
*store estimation
est sto c_logit
*calculate marginal effect
estat mfx

*do multinomial logit model
asclogit dum, case(v1) alternatives(price) casevar(income)
*store estimation
est sto m_logit
*calculate marginal effect
estat mfx

*do mix logit model
asmixlogit dum, random(c) casevar(income) alternatives(price) case(v1)
*store estimation (of all alternative)
est sto all

*randomly drop one alternative (in this case drop the last alternative)
drop if choice == 10
drop if price == 10

*do mix logit model after removing one alternative
asmixlogit dum, random(c) casevar(income) alternatives(price) case(v1)
*store estimation (of partial alternative)
est sto partial

*do hauman test for IIA property
hausman partial all, alleqs constant

**===========================================================
**HW4
**===========================================================
**Exercise 1
clear
insheet using /Users/killshadows/Desktop/DUKE/COURSES/SPRING2019/ECON613/Assignments/4/Koop-Tobias.csv
*convert data into panel data
xtset personid timetrnd
bysort personid: gen t = _n
*draw plots of 5 random individuals' panel dimension
plot logwage timetrnd if personid == 5
plot logwage timetrnd if personid == 15
plot logwage timetrnd if personid == 115
plot logwage timetrnd if personid == 1115
plot logwage timetrnd if personid == 1555

**Exercise 2
*do random effet model
xtreg logwage educ potexper, re

**Exercise 3
*do fixed effect model
*between estimator
xtreg logwage educ potexper,be

*within estimator
xtreg logwage educ potexper,fe

*first time difference estimator
gen d_wage = logwage - logwage[_n-1]
gen d_id = personid - personid[_n-1]
gen d_edu = educ - educ[_n-1]
gen d_exp = potexper - potexper[_n-1]
drop if d_id == 1
drop if d_id == .
reg d_wage d_edu d_exp
