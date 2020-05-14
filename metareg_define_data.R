# ========================================== #
# Define trial data for running STAN models
# ========================================== #

##############
# Studies included in the meta-analysis 
# ordered in - Aiello 2012, Simmerman 2011, Larson 2010, Nicholson 2014, Suess 2012, Pandejpong 2012
study_names <- c("Aiello 2012", "Simmerman 2011", "Larson 2010", "Nicholson 2014", "Suess 2012", "Pandejpong 2012")
study_names_short <- c("Aiello","Simmerman","Larson","Nicholson","Suess","Pandjejong")

##############
# Assumptions made in hand hygiene frequency and hours of mask use when not directly reported in the trials 
# For further details refer to study details in the archive folder 
baseline_hand_washing_freq <- 4 # baseline number of handwashes in control groups or in addition to 
#                                 reported hand hygiene frequencies when intervention is alcohol hand 
#                                 rub (based on Simmerman)
assumed_mask_hrs <- 2           # number of mask hours per day if number of masks instead of number of hours 
#                                 of mask use reported 
soap_per_handwash <- 0.35       # Nicholson - reported volume of bar soap use, assume 0.35g per wash
#                                 Based on: https://blogs.scientificamerican.com/plugged-in/the-benefits-of-a-bar-of-soap-that-is/
#                                           https://www.mcgill.ca/oss/article/health/liquid-or-bar-soapy-tales
ml_per_wash <- 2                # Larson - reported volume of alcohol hand rub use, assume 2ml per wash 
no_value <- -99                 # use -99 for NA (as NA not allowed in Stan)

##############
# Data from each arm in each trial 

# Aiello 2012: alcohol or soap as hand hygiene intervention 
Aiello_hw_per_day_control <- 5.93 + 1.51 
Aiello_hw_per_day_arm2 <- 5.81 + 1.29     # mask arm (Hand washing events + Hand sanitzer events)
Aiello_hw_per_day_arm3 <- 5.58 + 4.49     # mask + hand hygiene arm (Hand washing events + Hand sanitzer events)

Aiello_maskhrs_per_day_control <- 0       # assume no mask worn in control group 
Aiello_maskhrs_per_day_arm2 <- 5.04       # mask arm
Aiello_maskhrs_per_day_arm3 <- 5.08       # mask + hand hygiene arm

# Simmerman 2011: soap as hand hygiene intervention 
Simmerman_hw_per_day_control <- 3.9       
Simmerman_hw_per_day_arm2 <- 4.7          # hand hygiene arm  
Simmerman_hw_per_day_arm3 <- 4.9          # mask + hand hygiene arm

Simmerman_maskhrs_per_day_control <- 0    # assume no mask worn in control group 
Simmerman_maskhrs_per_day_arm2 <- 0       # assume no mask worn in hand hygiene group 
Simmerman_maskhrs_per_day_arm3 <- 211/60  # mask + hand hygiene arm (211 min)

# Larson 2010: alcohol hand rub as hand hygiene intervention 
Larson_hw_per_day_control <- baseline_hand_washing_freq                       # assumed baseline as this is not reported 
Larson_hw_per_day_arm2 <- (343.8/30)/ml_per_wash + baseline_hand_washing_freq # hand hygiene arm: 343.8ml alcohol used per month 
Larson_hw_per_day_arm3 <- (329.6/30)/ml_per_wash + baseline_hand_washing_freq # mask + hand hygiene arm: 329.6ml alcohol used per month 

Larson_maskhrs_per_day_control <- 0               # assume no mask worn in control group 
Larson_maskhrs_per_day_arm2 <- 0                  # assume no mask worn in hand hygiene group 
Larson_maskhrs_per_day_arm3 <- assumed_mask_hrs   # masks only worn for those who developed symptoms;
#                                                   only half (22/44) of the households with an ILI reported using masks 
#                                                   within 48 hours of episode onset. Those who used masks at all reported 
#                                                   a mean of only two masks/ day/ILI episode (range: 0–9)
#                                                   Hence, 2 masks per day in half of the families 
#                                                   --> 2 hours per mask * 2 masks / 2 = 2hours 

# Nicholson 2014: giving soap bars to households as hand hygiene intervention
Nicholson_hw_per_day_control <- ((45/7)/5.8)/soap_per_handwash # reported 45g of soap per week in control arm 
#                                                                (mean faimly size 5.8)
Nicholson_hw_per_day_arm2 <- ((235/7)/5.7)/soap_per_handwash   # reported 235g of soap per week in hand hygiene arm (mean faimly size 5.7)
#                                                                (mean faimly size 5.7)
Nicholson_hw_per_day_arm3 <- no_value                          # only 2 arms in this study, hence this is given -99 as Stan does not take NA

Nicholson_maskhrs_per_day_control <- 0                         # assume no mask worn in this study as it is not part of any interventions
Nicholson_maskhrs_per_day_arm2 <- 0
Nicholson_maskhrs_per_day_arm3 <- 0

# Suess 2012: alcohol hand rub as hand hygiene intervention
# For Suess - little bit unclear on hand hygiene frequency though some data
Suess_hw_per_day_control <- 1 + baseline_hand_washing_freq
Suess_hw_per_day_arm2 <- 4 + baseline_hand_washing_freq
Suess_hw_per_day_arm3 <- 4 + baseline_hand_washing_freq

Suess_maskhrs_per_day_control <- 0
Suess_maskhrs_per_day_arm2 <- 4.2
Suess_maskhrs_per_day_arm3 <- 3.6

# Pandjejong 2012: alcohol hand rub as hand hygiene intervention
Pandjejong_hw_per_day_control <- 1 + baseline_hand_washing_freq  # control group washed hands once before lunch 
Pandjejong_hw_per_day_arm2 <- 6 + baseline_hand_washing_freq     # arm 2 washed hands once every hour during school hours 
Pandjejong_hw_per_day_arm3 <- 3 + baseline_hand_washing_freq     # arm 3 washed hands once every 2 hours during school hours 

Pandjejong_maskhrs_per_day_control <- 0                          # assume no mask worn in this study as it is not part of any interventions
Pandjejong_maskhrs_per_day_arm2 <- 0
Pandjejong_maskhrs_per_day_arm3 <- 0

# Other data
num_arms<-c(3, 3, 3, 2, 3, 3)                         # numbers of arms per trial 
# index_case_based_recruitment <- c(0, 1, 0, 0, 1, 0)  # if a particular trial enrolled an index patient with respiratory infection 
# multiple_outcomes_per_person <- c(0, 0, 1, 1, 0, 1) # if the trial reported multiple outcomes per participant 
follow_up_time_days <- c(42, 21, 389, 287, 8, 84)     # period of follow up for each trial 

hh_intervention_arm2 <- c(0, 1, 1, 1, 0, 1)           #clearly specify the intervention for each arm 
hh_intervention_arm3 <- c(1, 1, 1, 0, 1, 1)
mask_intervention_arm2 <- c(1, 0, 0, 0, 1, 0)
mask_intervention_arm3 <- c(1, 1, 1, 0, 1, 0)

num_individuals_control_arm <- c(370, 302, 904, 4812, 82, 540) #number of participants in each arm 
num_individuals_arm2 <- c(392, 292, 946, 4863, 69, 452)
num_individuals_arm3 <- c(349, 291, 938, no_value, 67, 449)

person_days_at_risk_control_arm <- c(no_value, no_value, 46526*7, 1289616, no_value, 538 *84) # person at risk for each arm is calculated 
person_days_at_risk_arm2 <- c(no_value, no_value, 48731*7, 1302311, no_value, 452*84)         # with number of participants * follow up
person_days_at_risk_arm3 <- c(no_value, no_value, 50676*7, no_value, no_value, 447*84)        # time if not explicitly reported

case_data_control_arm <- c(51, 26, 1646, 20526, 14, 644) # number of outcomes (upper respiratory tract infection symptoms) reported
case_data_arm2 <- c(46, 50, 1416, 18432, 6, 438 )        # in each arm of each trial 
case_data_arm3 <- c(31, 51, 1972, no_value, 6, 501)

##############
# Combine above data into list for stan 
hhfreq_control <- sapply(paste0(study_names_short,"_hw_per_day_control"), function(x) eval(parse(text = x)))
hhfreq_arm2 <- sapply(paste0(study_names_short,"_hw_per_day_arm2"), function(x) eval(parse(text = x)))
hhfreq_arm3 <- sapply(paste0(study_names_short,"_hw_per_day_arm3"), function(x) eval(parse(text = x)))

maskhrs_control <- sapply(paste0(study_names_short,"_maskhrs_per_day_control"), function(x) eval(parse(text = x)))
maskhrs_arm2 <- sapply(paste0(study_names_short,"_maskhrs_per_day_arm2"), function(x) eval(parse(text = x)))
maskhrs_arm3 <- sapply(paste0(study_names_short,"_maskhrs_per_day_arm3"), function(x) eval(parse(text = x)))

case_data <- array(data=c(case_data_control_arm, case_data_arm2, case_data_arm3),dim = c(6,3))
denom_data <- array(data=c(num_individuals_control_arm, num_individuals_arm2, num_individuals_arm3), dim = c(6,3))

(hhfreq <- array(data=c(hhfreq_control, hhfreq_arm2, hhfreq_arm3), dim = c(6, 3)))
(maskhrs <- array(data=c(maskhrs_control, maskhrs_arm2, maskhrs_arm3), dim = c(6,3)))

hh_trial_data <- list(
  T = 6,
  cases = case_data,
  denoms = denom_data,
  followupdays = follow_up_time_days,
  hhfreq = hhfreq,
  maskhrs = maskhrs,
  pdaysatrisk_arm1 = person_days_at_risk_control_arm,
  pdaysatrisk_arm2 = person_days_at_risk_arm2,
  pdaysatrisk_arm3 = person_days_at_risk_arm3
)