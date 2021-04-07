# =========================================================================== #
# Runs STAN models based on data in metareg_define_data.R and metareg0.9b.stan
# =========================================================================== #

library(rstan)
source('metareg_define_data.R')

fit0.9_main <- stan(
  file = "metareg0.9b.stan",   # Input model version here 
  data = hh_trial_data,       # named list of data defined in metareg_define_data.R
  chains = 4,                 # number of Markov chains
  warmup = 1000,              # number of warmup iterations per chain
  iter = 200000,              # total number of iterations per chain
  cores = 4,                  # number of cores (use one per chain)
  refresh = 1000,             # no of runs at which progress is shown
  control = list(max_treedepth = 15, adapt_delta=0.99)
)

#####################
# Check quality 
mod_fit = fit0.9_main # change this to the model ran 

# Rhat and chain mixing
summary(mod_fit, pars = params)$summary
traceplot(mod_fit, pars = params)

# Print relative risk for handwashing and masks
print(mod_fit, pars=c("RR_handwashing", "RR_masks"))

# Plot parameters for hand washing and masks
suffix <- "2chains_08052020_LarsonLower" # for saving files
pdf(paste0("plot_",suffix,".pdf"), width=12,height=9)
# parameters to extract from the stanfit object 
params = c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","c[1]","c[2]","c[3]","c[4]","c[5]","RR_handwashing", "RR_masks")
plot(mod_fit, pars = params)
dev.off()

# Save output parameters
write.table(summary(mod_fit)$summary, file=paste0("summary_", suffix,".csv"),sep=",")
save(mod_fit, file=paste0("model0.5HHCOVID_", suffix, ".Rdata")) 


