# ========================================== #
# R code for running stan metaregression 
# ========================================== #
# Studies in order are Aiello 2012, Simmerman 2011, Larson 2010, Nicholson 2014, Suess 2012, Pandejpong 2012
# use -99 for NA (as NA not allowed in Stan)
study_names<-c("Aiello 2012", "Simmerman 2011", "Larson 2010", "Nicholson 2014", "Suess 2012", "Pandejpong 2012")
study_names_short <- c("Aiello","Simmerman","Larson","Nicholson","Suess","Pandjejong")

# Assumed baseline numbers
baseline_hand_washing_freq<-4 # assumed number of handwashes  with soap and to use when not reported (based on Simmerman)
assumed_mask_hrs<-2 # assumed number of mask hours per day in mask arm if not reported
no_value<- -99

# arm2 = mask or hand hygiene intervention 
# arm3 = combination of mask or hand hygiene

# Aiello 2012: HH (alcohol + soap) + mask
Aiello_hw_per_day_control <- 5.93+1.51
Aiello_hw_per_day_arm2 <- 5.81+1.29
Aiello_hw_per_day_arm3 <- 5.58+4.49

Aiello_maskhrs_per_day_control <- 0
Aiello_maskhrs_per_day_arm2 <- 5.04
Aiello_maskhrs_per_day_arm3 <- 5.08

# Simmerman 2011: HH (soap)
Simmerman_hw_per_day_control <- 3.9
Simmerman_hw_per_day_arm2 <- 4.7
Simmerman_hw_per_day_arm3 <- 4.9

Simmerman_maskhrs_per_day_control <- 0
Simmerman_maskhrs_per_day_arm2 <- 0
Simmerman_maskhrs_per_day_arm3 <- 211/60

# Larson 2010: HH (alcohol hand rub)
Larson_hw_per_day_control <- baseline_hand_washing_freq
Larson_hw_per_day_arm2 <- 5.7 + baseline_hand_washing_freq
Larson_hw_per_day_arm3 <- 5.5 + baseline_hand_washing_freq

Larson_maskhrs_per_day_control <- 0
Larson_maskhrs_per_day_arm2 <- 0
Larson_maskhrs_per_day_arm3 <- 2

# Nicholson 2014: HH (soap bars)
{
  # Calculation for frequency of hand washes per day for Nicholson study
  # The following sources
  # https://blogs.scientificamerican.com/plugged-in/the-benefits-of-a-bar-of-soap-that-is/
  # https://www.mcgill.ca/oss/article/health/liquid-or-bar-soapy-tales
  # give 0.35 g of solid soap per handwash. 
  # Note that in the Nicholson study part of the intervention was giving soap to households
  # and they reported 45g of soap per week in control arm and 235g in intervention arm
  # 45g per week per household = 6.4g per day (in control)
  # 235g per week per househol = 33.6g per day (in intervention)
  # household size = 5.8 in control, 5.7 in intervention
  # --> 6.4/5.8 = 1.1g per person per day (in control)
  # --> 33.6/5.7 = 5.9g per person per day (in intervention)
  # Hand washes per day: 
  # 1.1/0.35 = 3.2 handwashes per day (control)
  # 5.9/0.35 = 16.8 in intervention arm
}
Nicholson_hw_per_day_control <- ((45/7)/5.8)/.35
Nicholson_hw_per_day_arm2 <- ((235/7)/5.7)/.35
Nicholson_hw_per_day_arm3 <- no_value

Nicholson_maskhrs_per_day_control <- 0
Nicholson_maskhrs_per_day_arm2 <- 0
Nicholson_maskhrs_per_day_arm3 <- 0

# Suess 2012: HH (alcohol hand rub)
# For Suess - little bit unclear on hand hygiene frequency though some data
Suess_hw_per_day_control <- 1 + baseline_hand_washing_freq
Suess_hw_per_day_arm2 <- 4 + baseline_hand_washing_freq
Suess_hw_per_day_arm3 <- 4 + baseline_hand_washing_freq

Suess_maskhrs_per_day_control <- 0
Suess_maskhrs_per_day_arm2 <- 4.2
Suess_maskhrs_per_day_arm3 <- 3.6

# Pandjejong 2012: HH (alcohol hand rub)
Pandjejong_hw_per_day_control <- 1 + baseline_hand_washing_freq
Pandjejong_hw_per_day_arm2 <- 6 + baseline_hand_washing_freq
Pandjejong_hw_per_day_arm3 <- 3 + baseline_hand_washing_freq

Pandjejong_maskhrs_per_day_control<-0
Pandjejong_maskhrs_per_day_arm2<-0
Pandjejong_maskhrs_per_day_arm3<-0

# Other data
num_arms<-c(3,3,3,2,3,3)
index_case_based_recruitment<-c(0,1,0,0,1,0)
multiple_outcomes_per_person<-c(0,0,1,1,0, 1)
follow_up_time_days<-c(42, 21, 389, 287, 8, 84)

hh_intervention_arm2<-c(0, 1, 1, 1, 0, 1)
hh_intervention_arm3<-c(1, 1, 1, 0, 1, 1)
mask_intervention_arm2<-c(1, 0, 0, 0, 1, 0)
mask_intervention_arm3<-c(1, 1, 1, 0, 1, 0)

num_individuals_control_arm<-c(370, 302, 904, 4812, 82, 540)
num_individuals_arm2<-c(392, 292, 946, 4863, 69, 452)
num_individuals_arm3<-c(349, 291, 938, no_value, 67, 449)

person_days_at_risk_control_arm<-c(no_value, no_value, 46526*7, 1289616, no_value, 538 *84)
person_days_at_risk_arm2<-c(no_value, no_value, 48731*7, 1302311, no_value, 452*84)
person_days_at_risk_arm3<-c(no_value, no_value, 50676*7, no_value, no_value, 447*84)

case_data_control_arm<-c(51, 26, 1646, 20526, 14, 644)
case_data_arm2<-c(46, 50, 1416, 18432, 6, 438 )
case_data_arm3<-c(31, 51, 1972, no_value, 6, 501)

hhfreq_control <- sapply(paste0(study_names_short,"_hw_per_day_control"), function(x) eval(parse(text=x)))
hhfreq_arm2 <- sapply(paste0(study_names_short,"_hw_per_day_arm2"), function(x) eval(parse(text=x)))
hhfreq_arm3 <- sapply(paste0(study_names_short,"_hw_per_day_arm3"), function(x) eval(parse(text=x)))
# hhfreq_control<-c(5.93+1.51, 3.9, baseline_hand_washing_freq, Nicholson_hw_per_day_control, 1, 1+baseline_hand_washing_freq )
# hhfreq_arm2<-c(5.81+1.29, 4.7, baseline_hand_washing_freq+5.7, Nicholson_hw_per_day_arm2, 4, 6+baseline_hand_washing_freq )
# hhfreq_arm3<-c(5.58+4.49, 4.9, baseline_hand_washing_freq+5.5, no_value, 4, 3+baseline_hand_washing_freq)

maskhrs_control <- sapply(paste0(study_names_short,"_maskhrs_per_day_control"), function(x) eval(parse(text=x)))
maskhrs_arm2 <- sapply(paste0(study_names_short,"_maskhrs_per_day_arm2"), function(x) eval(parse(text=x)))
maskhrs_arm3 <- sapply(paste0(study_names_short,"_maskhrs_per_day_arm3"), function(x) eval(parse(text=x)))
# maskhrs_control<-c(0,0,0,0,0,0)
# maskhrs_arm2<-c(5.04,0,0,0,4.2,0)
# maskhrs_arm3<-c(5.08,211/60,assumed_mask_hrs,0,3.6,0)

case_data<-array(data=c(case_data_control_arm,case_data_arm2,case_data_arm3),dim = c(6,3))
denom_data<-array(data=c(num_individuals_control_arm,num_individuals_arm2,num_individuals_arm3),dim = c(6,3))

(hhfreq <- array(data=c(hhfreq_control,hhfreq_arm2,hhfreq_arm3),dim = c(6, 3)))
(maskhrs <- array(data=c(maskhrs_control,maskhrs_arm2,maskhrs_arm3),dim = c(6,3)))


# List for stan
hh_trial_data <- list(
  T = 6,
  # arms = num_arms,
  # binaryoutcome=!multiple_outcomes_per_person,
  cases = case_data,
  denoms = denom_data,
  followupdays=follow_up_time_days,
  hhfreq=hhfreq,
  maskhrs=maskhrs,
  pdaysatrisk_arm1=person_days_at_risk_control_arm,
  pdaysatrisk_arm2=person_days_at_risk_arm2,
  pdaysatrisk_arm3=person_days_at_risk_arm3
)