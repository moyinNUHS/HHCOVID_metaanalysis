#this file runs the data in stan

library(rstan)

fit0.5 <- stan(
  file = "metareg0.5.stan",  # Stan program
  data = hh_trial_data,    # named list of data defined in metareg_define_data.R
  chains = 2,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 100000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 1000,             # no progress shown
  control = list(max_treedepth = 15,adapt_delta=0.8)
)


plot(fit0.5, pars=c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]" ))
plot(fit0.5, pars=c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","c[1]","c[2]","c[3]","c[4]","c[5]" ))

