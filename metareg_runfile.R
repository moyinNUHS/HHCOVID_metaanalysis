#this file runs the data in stan
library(rstan)
source('metareg_define_data.R')

fit0.5 <- stan(
  file = "metareg0.5.stan",  # Input model version here 
  data = hh_trial_data,      # named list of data defined in metareg_define_data.R
  chains = 2,                # number of Markov chains
  warmup = 1000,             # number of warmup iterations per chain
  iter = 200000,             # total number of iterations per chain
  cores = 2,                 # number of cores (could use one per chain)
  refresh = 1000,            # no of runs at which progress is shown
  control = list(max_treedepth = 15, adapt_delta=0.99)
)

params = c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","c[1]","c[2]","c[3]","c[4]","c[5]","RR_handwashing", "RR_masks")

# For saving files
suffix <- "2chains_08052020_LarsonLower"

# Plot parameters for hand washing and masks
pdf(paste0("plot_",suffix,".pdf"),width=12,height=9)
plot(fit0.5, pars = params)
dev.off()
# Print relative risk for handwashing and masks
print(fit0.5, pars=c("RR_handwashing", "RR_masks"))

summary(fit0.5, pars = params)$summary
traceplot(fit0.5, pars = params)

# Save output parameters
write.table(summary(fit0.5)$summary, file=paste0("summary_",suffix,".csv"),sep=",")
save(fit0.5, file=paste0("model0.5HHCOVID_", suffix, ".Rdata")) 


