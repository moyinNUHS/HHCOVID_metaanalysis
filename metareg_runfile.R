#this file runs the data in stan
library(rstan)
source('metareg_define_data.R')

fit0.5 <- stan(
  file = "metareg0.5.stan",  # Input model version here 
  data = hh_trial_data,      # named list of data defined in metareg_define_data.R
  chains = 4,                # number of Markov chains
  warmup = 1000,             # number of warmup iterations per chain
  iter = 500000,             # total number of iterations per chain
  cores = 4,                 # number of cores (could use one per chain)
  refresh = 1000,            # no of runs at which progress is shown
  control = list(max_treedepth = 15, adapt_delta=0.99)
)

params = c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","c[1]","c[2]","c[3]","c[4]","c[5]","RR_handwashing", "RR_masks")

# Plot parameters for hand washing and masks
plot(fit0.5, pars = params)
# Print relative risk for handwashing and masks
print(fit0.5, pars=c("RR_handwashing", "RR_masks"))

summary(fit0.5, pars = params)$summary
traceplot(fit0.5, pars = params)

# Save output parameters
write.table(summary(fit1)$summary, file="summary.csv",sep=",")

save(fit0.5, file='../../../../Desktop/model0.5HHCOVID.Rdata')

