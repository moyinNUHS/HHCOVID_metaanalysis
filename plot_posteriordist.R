# ========================================== #
# Plot posterior distributions model output 
# ========================================== #

rm(list=ls()) # clear environment
library(ggplot2); library(rstan) # load required libraries 

##############
# Get data for plot

# outputs from stan model 
fit0.9_main = readRDS("runs/fit0.9_5k.RDS") # main analysis 
fit0.9_sens1 = readRDS("runs/fit0.9_5k.RDS") # main analysis 
fit0.9_sens2 = readRDS("runs/fit0.9_5k.RDS") # main analysis 

# parameter names 
params.trial = c("a0", "c0", "b0", 
                 "a[1]","a[2]","a[3]","a[4]","a[5]","a[6]",
                 "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]",
                 "c[1]","c[2]","c[3]","c[4]","c[5]", 
                 "OR_masks", "OR_handwashing")
p1 = NULL
for(i in 1:20) p1<-c(p1, paste0("predicted_mean[", i, "]") ) # means of the parameters 
params = c(params.trial, p1)

# posterior values from stan model output 
post = rstan::extract(fit0.9_main, pars = params)


##############
# Produce plot 



# save plot
ggsave(paste0('../../../../Desktop/dose_response_COVIDHH.jpeg'), units = 'cm', width = 30, height= 15)

