source("metareg_define_data.R")    # define data 
require(rstan)

fit_trial4 <- stan(
  file = "metareg_1trial.stan",  # Stan program
  data = hh_trial_data,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 100000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 1000,             # no progress shown
  control = list(max_treedepth = 15, adapt_delta=0.8)
)
summary(fit_trial4)
plot(fit_trial4, pars=c("c","b"))

save.image('../../../../Desktop/trial4.metareg.Rdata')
