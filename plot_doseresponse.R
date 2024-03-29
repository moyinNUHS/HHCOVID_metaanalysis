# ========================================== #
# Plot dose-response curve from model output 
# ========================================== #

rm(list = ls()) # clear environment
library(ggplot2); library(rstan) # load required libraries 

##############
# Get data for plot

# outputs from stan model - main analysis (loads object mod_fit)
load("runs/fit0.9b_main.Rdata")

# parameter names 
source('metareg_define_data.R') # Get hand hygiene frequency from data 
params.trial = c("a0", "c0", "b0", 
                 "a[1]","a[2]","a[3]","a[4]","a[5]","a[6]",
                 "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]",
                 "c[1]","c[2]","c[3]","c[4]","c[5]", 
                 "RR_masks", "RR_handwashing")
p1 = NULL
for(i in 1:20) p1<-c(p1, paste0("predicted_mean[", i, "]") ) # means of the parameters 
params = c(params.trial, p1)

# posterior values from stan model output 
d = rstan::extract(mod_fit, pars = params)

##############
# Prepare posterior values into dataframe for ggplot 

# posterior values for each arm (for bubbles)
df.trial = data.frame(dp.Aiello.cont= exp(d$`a[1]` + d$`b[1]`*hhfreq[1,1]),
                      dp.Aiello.mask = exp(d$`a[1]` + d$`b[1]`*hhfreq[1,2]),
                      dp.Aiello.hhmask= exp(d$`a[1]` + d$`b[1]`*hhfreq[1,3]),
                      dp.Simmerman.cont = exp(d$`a[2]` + d$`b[2]`*hhfreq[2,1]),
                      dp.Simmerman.hh = exp(d$`a[2]` + d$`b[2]`*hhfreq[2,2]),
                      dp.Simmerman.hhmask = exp(d$`a[2]` + d$`b[2]`*hhfreq[2,3]),
                      dp.Larson.cont = exp(d$`a[3]` + d$`b[3]`*hhfreq[3,1]),
                      dp.Larson.hh = exp(d$`a[3]` + d$`b[3]`*hhfreq[3,2]),
                      dp.Larson.hhmask = exp(d$`a[3]` + d$`b[3]`*hhfreq[3,3]),
                      dp.Nicholson.cont = exp(d$`a[4]` + d$`b[4]`*hhfreq[4,1]),
                      dp.Nicholson.hh = exp(d$`a[4]` + d$`b[4]`*hhfreq[4,2]),
                      dp.Suess.cont = exp(d$`a[5]` + d$`b[5]`*hhfreq[5,1]),
                      dp.Suess.mask = exp(d$`a[5]` + d$`b[5]`*hhfreq[5,2]),
                      dp.Suess.hhmask = exp(d$`a[5]` + d$`b[5]`*hhfreq[5,3]),
                      dp.Pandejpong.cont = exp(d$`a[6]` + d$`b[6]`*hhfreq[6,1]),
                      dp.Pandejpong.2h = exp(d$`a[6]` + d$`b[6]`*hhfreq[6,2]),
                      dp.Pandejpong.1h = exp(d$`a[6]` + d$`b[6]`*hhfreq[6,3]))
df.trial = as.data.frame(t(sapply(df.trial, quantile, probs = c(0.1, 0.5, 0.9)))) #plot 80% credible intervals 
hhfreq.counts = c(unlist(t(hhfreq))) # take the hand hygiene values for each arm 
hhfreq.counts = hhfreq.counts[-which(hhfreq.counts == -99)] #remove the arm 3 value from Nicholson (coded as -99 as stan does not take NA)
df.trial$hhfreq = hhfreq.counts
df.trial$trial.names = c(rep('Aiello', 3),  # trial names (to determine color of the bubbles)
                         rep('Simmerman', 3),
                         rep('Larson', 3),
                         rep('Nicholson', 2),
                         rep('Suess', 3),
                         rep('Pandejpong', 3))
df.trial$trial.arms = as.factor(c('Control', 'Mask', 'HH and mask', # trial names (to determine borders of the bubbles)
                                  'Control', 'HH', 'HH and mask', 
                                  'Control', 'HH', 'HH and mask', 
                                  'Control', 'HH', 
                                  'Control', 'Mask', 'HH and mask',
                                  'Control', 'HH', 'HH'))
levels(df.trial$trial.arms) = c('Control', 'HH', 'Mask', 'HH and mask')

# posterior values for predicted data (for ribbons)
df.predicted = as.data.frame(matrix(unlist(d[grep('predicted', names(d))]), 
                                    byrow = F, nrow = lengths(d)))
colnames(df.predicted) = names(d)[grep('predicted', names(d))]
quants = c(0.1, 0.25, 0.5, 0.75, 0.9) # plot 50% and 80% credible intervals
d.plot.predict = as.data.frame(t(sapply(df.predicted, quantile, probs = quants)))
colnames(d.plot.predict) = paste0("pred_mean", quants)
d.plot.predict$hhfreq = 0:(length(p1)-1)

##############
# Produce plot 

# colors for plots 
cols = c("#52154ECC", "#0072B2CC", "#FFA500CC", "#EF5D60CC", "#83B692CC", "#7070D0CC")
ribcol = c(alpha('grey', 0.3),
           alpha('grey', 0.6))
# plot
ggplot() + 
  geom_ribbon(aes(x = hhfreq, ymin = pred_mean0.1, ymax = pred_mean0.9), data = d.plot.predict, fill = ribcol[1])+
  geom_ribbon(aes(x = hhfreq, ymin = pred_mean0.25, ymax = pred_mean0.75), data = d.plot.predict, fill = ribcol[2])+
  geom_point(aes(x = hhfreq, y = `50%`, size = 1/(`90%` - `10%`), color = trial.names), data = df.trial) +
  geom_line(aes(x = hhfreq, y = pred_mean0.5), data = d.plot.predict, color='grey40') +
  scale_y_continuous(limits = c(0, 0.02)) + 
  scale_x_continuous(limits = c(0, 19)) + 
  scale_size(guide = 'none') +
  ylab('Daily probability of infection') + 
  xlab('Hand hygiene frequency per day') + 
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(), 
        text = element_text(size = 15)) +
  guides(color = guide_legend(nrow = 1))

# save plot
ggsave('~/Documents/nBox/COVID/Hand_hygiene_covid/graphics/dose_response_COVIDHH.jpg', units = 'cm', width = 30, height= 15)

