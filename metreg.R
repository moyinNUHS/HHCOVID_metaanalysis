# R code for running stan metaregression 
# Studies in order are Aiello 2012, Simmerman 2011, Larson 2010, Nicholson 2014, Suess 2012, Pandejpong 2012
# use -99 for NA (as NA not allowed in Stan)
study_names<-c("Aiello 2012", "Simmerman 2011", "Larson 2010", "Nicholson 2014", "Suess 2012", "Pandejpong 2012")

baseline_hand_washing_freq<-4 # assumed number of handwashes  with soap and to use when not reported (based on Simmerman)


#https://blogs.scientificamerican.com/plugged-in/the-benefits-of-a-bar-of-soap-that-is/
#https://www.mcgill.ca/oss/article/health/liquid-or-bar-soapy-tales
# gives 0.35 g of solid soap per handwash
#note that in the Nicholson study part of the interventino was giving soap to households
# and they reported 45g of soap per week in control arm 
#45g per week per household#= 6.4 g per day
#household size =5.8 control , 5.7 in intervention
#1.1 g per person per day 
#so 3.2 handwashes per day 
# and 16.8 in interventino arm
Nicholson_hw_per_day_control<-((45/7)/5.8)/.35
Nicholson_hw_per_day_arm2<-((235/7)/5.7)/.35

#For Seuss - little bit unclear on hand hygiene frequency though some data

assumed_mask_hrs<-2 # assumed number of mask hours per day in mask arm if not reported

num_arms<-c(3,3,3,2,3,3)
index_case_based_recruitment<-c(0,1,0,0,1,0)
multiple_outcomes_per_person<-c(0,0,1,1,0, 1)
follow_up_time_days<-c(42,21,389,287,8,84)

num_individuals_control_arm<-c(370,302,904,4812,82,540)
num_individuals_arm2<-c(392,292,946,4863,69, 452)
num_individuals_arm3<-c(349,291, 938,-99, 67, 449)

person_days_at_risk_control_arm<-c(-99, -99, 46526*7, 1289616, -99, 538 *84)
person_days_at_risk_arm2<-c(-99,-99,48731*7, 1302311,-99,452*84)
person_days_at_risk_arm3<-c(-99,-99,50676*7, -99, -99, 447*84)

case_data_control_arm<-c(51,26,1646,20526, 14, 644)
case_data_arm2<-c(46,50,1416,18432, 6, 438 )
case_data_arm3<-c(31,51, 1972,-99,6, 501)

hhfreq_control<-c(5.93+1.51,3.9, baseline_hand_washing_freq,3.2, 1+baseline_hand_washing_freq )
hhfreq_arm2<-c(5.81+1.29, 4.7,baseline_hand_washing_freq+5.7, 16.8, 4, 6 + baseline_hand_washing_freq )
hhfreq_arm3<-c(5.58+4.49,4.9,baseline_hand_washing_freq+5.5, -99,-99,4, 3+baseline_hand_washing_freq)
maskhrs_control<-c(0,0,0,0,0,0)
maskhrs_arm2<-c(5.04,0,0,0,4.2,0)
maskhrs_arm3<-c(5.08,211/60,assumed_mask_hrs,0,3.6,0)


hh_intervention_arm2<-c(0,1,1,1,0,1)
hh_intervention_arm3<-c(1,1,1,0,1,1)
mask_intervention_arm2<-c(1,0,0,0,1,0)
mask_intervention_arm3<-c(1,1,1,0,1,0)



#case_data<-array(data=c(10, 12, 6, 7,0,0),dim = c(2,3))
case_data<-array(data=c(case_data_control_arm,case_data_arm2,case_data_arm3),dim = c(6,3))
# denom_data<-array(data=c(100, 101, 99, 102,0,0),dim = c(2,3))
denom_data<-array(data=c(num_individuals_control_arm,num_individuals_arm2,num_individuals_arm3),dim = c(6,3))
hhfreq <- array(data=c(hhfreq_control,hhfreq_arm2,hhfreq_arm3),dim = c(6, 3))

maskhrs <- array(data=c(maskhrs_control,maskhrs_arm2,maskhrs_arm3),dim = c(6,3))



hh_trial_data <- list(
  T = 6,
  arms = num_arms,
  cases = case_data,
  denoms = denom_data,
  hhfreq=hhfreq,
  maskhrs=maskhrs,
  binaryoutcome=!multiple_outcomes_per_person,
  followupdays=follow_up_time_days,
  pdaysatrisk_arm1=person_days_at_risk_control_arm,
  pdaysatrisk_arm2=person_days_at_risk_arm2,
  pdaysatrisk_arm3=person_days_at_risk_arm3
  
)

library(rstan)
fit0.3 <- stan(
  file = "metareg0.3.stan",  # Stan program
  data = hh_trial_data,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 100000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 1000,             # no progress shown
  control = list(max_treedepth = 15,adapt_delta=0.8)
)

#plot(fit1, pars=c("b","c", "pi1_1","pi1_2","pi1_3","pi2_1","pi2_2","pi2_3" ))
#plot(fit1, pars=c("a0", "b","c", "a[1]","a[2]","a[3]","a[4]","a[5]","a[6]" ))

#plot(fit1, pars=c("b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]" ))
plot(fit0.3, pars=c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]" ))
plot(fit0.3, pars=c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","c[1]","c[2]","c[3]","c[4]","c[5]" ))



fit0.4 <- stan(
  file = "metareg0.4.stan",  # Stan program
  data = hh_trial_data,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 100000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 1000,             # no progress shown
  control = list(max_treedepth = 15,adapt_delta=0.8)
)


