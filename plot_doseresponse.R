rm(list=ls())

##plot dose response fig 3 
library(ggplot2)
library(rstan)
library(bayesplot)

#get outputs from stan model 
fit0.9 = readRDS("fit0.9_5k.RDS")

#get parameter names 
source('metareg_define_data.R') #get hand hygiene frequency from data 
params.trial = c("a0", "c0", "b0", 
                 "a[1]","a[2]","a[3]","a[4]","a[5]","a[6]",
                 "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]",
                 "c[1]","c[2]","c[3]","c[4]","c[5]", 
                 "OR_masks", "OR_handwashing")
p1 = NULL
for(i in 1:20) p1<-c(p1, paste0("predicted_mean[",i,"]") )
p2 = NULL
for(i in 1:length(p1)) p2<-c(p2, paste0("predicted_mean_new_trial[",i,"]") )

params = c(params.trial, p1, p2)

#get posterior values 
d = rstan::extract(fit0.9, pars = params)

# put posterior values into dataframe
### posterior for each trial and arms 
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
sample = 5000
df = dplyr::sample_n(df.trial, sample) #sample some values so the plot doesn't take forever
arm.names = colnames(df)
hhfreq.counts = c(unlist(t(hhfreq))) #take the hand hygiene values for each arm 
hhfreq.counts = hhfreq.counts[-which(hhfreq.counts == -99)] #remove the value from Nicholson 
colnames(df) = hhfreq.counts
df$iter = 1:nrow(df)
df = apply(df, 2, as.character)

d.plot.trial = reshape2::melt(df, measure.var = 'iter')
d.plot.trial = d.plot.trial[-which(d.plot.trial$Var2=='iter'),]
d.plot.trial$hhfreq = as.numeric(as.character(d.plot.trial$Var2))
d.plot.trial$value = as.numeric(as.character(d.plot.trial$value))
d.plot.trial$iter = as.factor(d.plot.trial$Var1)
d.plot.trial$trial.arms = rep(arm.names, each = sample)
d.plot.trial$trial.names = NA
d.plot.trial$trial.names[grep('Aiello', d.plot.trial$trial.arms)] = 'Aiello'
d.plot.trial$trial.names[grep('Simmerman', d.plot.trial$trial.arms)] = 'Simmerman'
d.plot.trial$trial.names[grep('Larson', d.plot.trial$trial.arms)] = 'Larson'
d.plot.trial$trial.names[grep('Nicholson', d.plot.trial$trial.arms)] = 'Nicholson'
d.plot.trial$trial.names[grep('Suess', d.plot.trial$trial.arms)] = 'Suess'
d.plot.trial$trial.names[grep('Pandejpong', d.plot.trial$trial.arms)] = 'Pandejpong'
str(d.plot.trial)

# get preducted data for ribbons 
df.predicted = as.data.frame(matrix(unlist(d[grep('predicted', names(d))]), 
                                    byrow = F, nrow = lengths(d)))
colnames(df.predicted) = names(d)[grep('predicted', names(d))]
quants = c(0.025, 0.1, 0.25, 0.5, 0.75,0.9, 0.975)
d.plot.predict = as.data.frame(t(sapply(df.predicted, quantile, probs = quants)))
d.plot.predict1 = d.plot.predict[1:length(p1),]
d.plot.predict2 = d.plot.predict[(length(p1)+1):nrow(d.plot.predict),]
colnames(d.plot.predict1)<- paste0("pred_mean", quants)
colnames(d.plot.predict2)<- paste0("pred_mean_new", quants)
d.plot.predict = cbind(d.plot.predict1, d.plot.predict2)
d.plot.predict$hhfreq = 0:(length(p1)-1)

# colors for plots 
alph = 0.8
cols = c(alpha('#52154E', alph), 
         alpha('#0072B2A0', alph),
         alpha('orange', alph),
         alpha('#EF5D60', alph),
         alpha('#83B692', alph),
         alpha('#7070D0', alph))
ribcol = c(alpha('grey', alph*0.3),
           alpha('grey', alph*0.6))
##plot
ggplot() + 
  geom_ribbon(aes(x = hhfreq, ymin = pred_mean0.1, ymax = pred_mean0.9), data = d.plot.predict, fill = ribcol[1])+
  geom_ribbon(aes(x = hhfreq, ymin = pred_mean0.25, ymax = pred_mean0.75), data = d.plot.predict, fill = ribcol[2])+
  geom_violin(aes(x=hhfreq, y=value, group = trial.arms, fill = trial.names), color = NA, data = d.plot.trial, width = 2) +
  geom_line(aes(x=hhfreq, y = pred_mean0.5), data = d.plot.predict, color='grey40') +
  scale_fill_manual(values = cols) +
  scale_y_continuous(limits = c(0, 0.04))+ 
  scale_x_continuous(limits = c(0, 18))+ 
  ylab('Daily probability of infection') + 
  xlab('Hand hygiene frequency per day') + 
  theme_minimal()+
  theme(legend.position = 'bottom', 
        legend.title = element_blank())+
  guides(fill = guide_legend(nrow = 1))

ggsave(paste0('../../../../Desktop/dose_response_COVIDHH.jpeg'), units = 'cm', width = 30, height= 15)

