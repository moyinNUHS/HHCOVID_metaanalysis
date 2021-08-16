# ======================================================================================== #
# Runs STAN models based on data in metareg_define_data.R and model in metareg0.9b.stan
# ======================================================================================= #

library(rstan)
source('metareg_define_data.R')

fit0.9_sens2 <- stan(
  file = "metareg0.9b.stan",   # Input model version here 
  data = hh_trial_data,        # named list of data defined in metareg_define_data.R
  chains = 4,                  # number of Markov chains
  warmup = 1000,               # number of warmup iterations per chain
  iter = 50000,               # total number of iterations per chain
  cores = 4,                   # number of cores (use one per chain)
  refresh = 1000,              # no of runs at which progress is shown
  control = list(max_treedepth = 15, adapt_delta=0.99)
)

mod_fit = fit0.9_sens2 # change this to the model ran
suffix = 'sens2'

#####################
# Save
write.csv(summary(mod_fit)$summary, file = paste0('runs/', paste0("summary_", suffix,".csv")))
save(mod_fit, file = paste0('runs/', "fit0.9b_", suffix, ".Rdata")) 

#####################
# Check quality 

# Rhat and chain mixing
# parameter names 
params.trial = c("a0", "c0", "b0", 
                 "a[1]","a[2]","a[3]","a[4]","a[5]","a[6]",
                 "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]",
                 "c[1]","c[2]","c[3]","c[4]","c[5]", 
                 "RR_masks", "RR_handwashing")
p1 = NULL
for(i in 1:20) p1<-c(p1, paste0("predicted_mean[", i, "]") ) # means of the parameters 
params = c(params.trial, p1)

summary(mod_fit, pars = params)$summary
traceplot(mod_fit, pars = c("RR_masks", "RR_handwashing"))


