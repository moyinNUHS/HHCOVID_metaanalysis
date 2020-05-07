##plot dose response 
library(ggplot2)
library(rstan)
library(bayesplot)

load('../../../../Downloads/model0.5HHCOVID_1.Rdata')

source('metareg_define_data.R')
params = c("a0", "c0", "b0", 
           "a[1]","a[2]","a[3]","a[4]","a[5]","a[6]",
           "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]",
           "c[1]","c[2]","c[3]","c[4]","c[5]",
           "RR_handwashing", "RR_masks")


d = rstan::extract(fit0.5, pars = params)
df.raw = data.frame( dp.Aiello.cont= exp(d$`a[1]` + d$`b[1]`*hhfreq[1,1]),
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
sample = 150000
df = dplyr::sample_n(df.raw, sample) #sample some values so the plot doesn't take forever
arm.names = colnames(df)
hhfreq.counts = c(unlist(t(hhfreq))) #take the hand hygiene values for each arm 
hhfreq.counts = hhfreq.counts[-which(hhfreq.counts == -99)] #remove the value from Nicholson 
colnames(df) = hhfreq.counts
df$iter = 1:nrow(df)
df = apply(df, 2, as.character)

d.plot = reshape2::melt(df, measure.var = 'iter')
d.plot = d.plot[-which(d.plot$Var2=='iter'),]
d.plot$hhfreq = as.numeric(as.character(d.plot$Var2))
d.plot$value = as.numeric(as.character(d.plot$value))
d.plot$iter = as.factor(d.plot$Var1)
d.plot$trial.arms = rep(arm.names, each = sample)
d.plot$trial.names = NA
d.plot$trial.names[grep('Aiello', d.plot$trial.arms)] = 'Aiello'
d.plot$trial.names[grep('Simmerman', d.plot$trial.arms)] = 'Simmerman'
d.plot$trial.names[grep('Larson', d.plot$trial.arms)] = 'Larson'
d.plot$trial.names[grep('Nicholson', d.plot$trial.arms)] = 'Nicholson'
d.plot$trial.names[grep('Suess', d.plot$trial.arms)] = 'Suess'
d.plot$trial.names[grep('Pandejpong', d.plot$trial.arms)] = 'Pandejpong'
str(d.plot)

means = apply(df.raw, 2, mean)
df.mean = data.frame(means = means, 
                     hhfreq = hhfreq.counts)

hh = 1:16
prediction.mean =  quantile(d$RR_handwashing, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
predictions = lapply(prediction.mean, function(x) {1 - (x^hh)})
predictions.df = as.data.frame(matrix(unlist(predictions), ncol = length(prediction.mean), byrow = F))
colnames(predictions.df) = c('10 %', '25 %', '50 %', '75 %', '90 %')
predictions.df$hhfreq = hh
predictions.plot= reshape2::melt(predictions.df, id.var = 'hhfreq')

cols = c(alpha('#52154E', 0.5), 
         alpha('#0072B2A0', 0.5),
         alpha('orange', 0.5),
         alpha('#EF5D60', 0.5),
         alpha('#83B692', 0.5),
         alpha('#7070D0', 0.5))
ribcol = c(alpha('grey', 0.2),
           alpha('grey', 0.5))
##boxplot
ggplot() + 
  #geom_ribbon(aes(x = hhfreq, ymin = `10 %`, ymax = `90 %`), data = predictions.df, fill = ribcol[1])+
  #geom_ribbon(aes(x = hhfreq, ymin = `25 %`, ymax = `75 %`), data = predictions.df, fill = ribcol[2])+
  geom_violin(aes(x=hhfreq, y=value, group = trial.arms, fill = trial.names), color = NA, data = d.plot, width = 2) +
  scale_color_manual(values = cols) +
  scale_y_continuous(limits = c(0, 0.04))+
  #geom_smooth(method = 'lm', color = '#303030') + 
  ylab('Daily probability of infection') + 
  xlab('Hand hygiene frequency per day') + 
  theme_minimal()+
  theme(legend.position = 'bottom', 
        legend.title = element_blank())+
  guides(fill = guide_legend(nrow = 1))

##ridge plot 
library(ggridges)
ggplot(d.plot, aes(y=hhfreq, x=value)) + 
  geom_density_ridges(outlier.shape = NA, aes(group = trial.arms, fill = trial.names), color = 'white') +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(0,0.04))+
  xlab('Daily probability of infection') + 
  ylab('Hand hygiene frequency per day') + 
  coord_flip()+
  theme_minimal()+
  theme(legend.position = 'bottom', legend.title = element_blank())

ggsave(paste0('../../../../Desktop/dose_response_COVIDHH.jpeg'), units = 'cm', width = 30, height= 15)


####Mask 
df.raw.mask = data.frame( dp.Aiello.cont= exp(d$`a[1]` + d$`c[1]`*maskhrs[1,1]),
                          dp.Aiello.mask = exp(d$`a[1]` + d$`c[1]`*maskhrs[1,2]),
                          dp.Aiello.hhmask= exp(d$`a[1]` + d$`c[1]`*maskhrs[1,3]),
                          dp.Simmerman.cont = exp(d$`a[2]` + d$`c[2]`*maskhrs[2,1]),
                          dp.Simmerman.hh = exp(d$`a[2]` + d$`c[2]`*maskhrs[2,2]),
                          dp.Simmerman.hhmask = exp(d$`a[2]` + d$`c[2]`*maskhrs[2,3]),
                          dp.Larson.cont = exp(d$`a[3]` + d$`c[3]`*maskhrs[3,1]),
                          dp.Larson.hh = exp(d$`a[3]` + d$`c[3]`*maskhrs[3,2]),
                          dp.Larson.hhmask = exp(d$`a[3]` + d$`c[3]`*maskhrs[3,3]),
                          dp.Nicholson.cont = exp(d$`a[4]` + d$`c[4]`*maskhrs[4,1]),
                          dp.Nicholson.hh = exp(d$`a[4]` + d$`c[4]`*maskhrs[4,2]),
                          dp.Suess.cont = exp(d$`a[5]` + d$`c[5]`*maskhrs[5,1]),
                          dp.Suess.mask = exp(d$`a[5]` + d$`c[5]`*maskhrs[5,2]),
                          dp.Suess.hhmask = exp(d$`a[5]` + d$`c[5]`*maskhrs[5,3]))
sample = 2000
df = dplyr::sample_n(df.raw.mask, sample) #sample some values so the plot doesn't take forever
arm.names = colnames(df)
maskhr.counts = c(unlist(t(maskhrs[1:5,]))) #take the hand hygiene values for each arm 
maskhr.counts = maskhr.counts[-12] #remove the value from Nicholson 
colnames(df) = maskhr.counts
df$iter = 1:nrow(df)
df = apply(df, 2, as.character)

d.plot = reshape2::melt(df, measure.var = 'iter')
d.plot = d.plot[-which(d.plot$Var2=='iter'),]
d.plot$maskhr = as.numeric(as.character(d.plot$Var2))
d.plot$value = as.numeric(as.character(d.plot$value))
d.plot$iter = as.factor(d.plot$Var1)
d.plot$trial.arms = rep(arm.names, each = sample)
d.plot$trial.names = NA
d.plot$trial.names[grep('Aiello', d.plot$trial.arms)] = 'Aiello'
d.plot$trial.names[grep('Simmerman', d.plot$trial.arms)] = 'Simmerman'
d.plot$trial.names[grep('Larson', d.plot$trial.arms)] = 'Larson'
d.plot$trial.names[grep('Nicholson', d.plot$trial.arms)] = 'Nicholson'
d.plot$trial.names[grep('Suess', d.plot$trial.arms)] = 'Suess'
str(d.plot)

cols = c(alpha('#52154E', 0.7), 
         alpha('#0072B2A0', 0.7),
         alpha('orange', 0.7),
         alpha('#EF5D60', 0.7),
         alpha('#83B692', 0.7))
##boxplot
ggplot(d.plot, aes(x=maskhr, y=value)) + 
  geom_boxplot(outlier.shape = NA, aes(group = trial.arms, color = trial.names), width = 0.6) +
  scale_color_manual(values = cols) +
  scale_y_continuous(limits = c(0,0.045))+
  #geom_smooth(method = 'lm', color = '#303030') + 
  ylab('Daily probability of infection') + 
  xlab('Mask hour per day') + 
  theme_minimal()+
  theme(legend.position = 'bottom', legend.title = element_blank())+
  guides(colour = guide_legend(nrow = 1))

ggsave(paste0('../../../../Desktop/dose_response_COVIDmask.jpeg'), units = 'cm', width = 20, height= 10)
