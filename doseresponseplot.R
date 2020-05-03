##plot dose response 
library(ggplot2)
library(rstan)
library(bayesplot)

params = c("a0", "c0", "b0", 
           "a[1]","a[2]","a[3]","a[4]","a[5]","a[6]",
           "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]",
           "c[1]","c[2]","c[3]","c[4]","c[5]",
           "RR_handwashing", "RR_masks")
load('../../../../Downloads/model0.5HHCOVID_2.Rdata')
d = rstan::extract(fit0.5, pars = params)

source('metareg_define_data.R')

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
df = dplyr::sample_n(df.raw, 1000)
hhfreq.counts = c(unlist(hhfreq))
hhfreq.counts = hhfreq.counts[-which(hhfreq.counts == -99)]
colnames(df) = hhfreq.counts
df$iter = 1:nrow(df)

d.plot = data.table::melt(df, id.var = 'iter')
d.plot$variable = as.numeric(as.character(d.plot$variable))
d.plot$value = as.numeric(d.plot$value)
str(d.plot)

color = alpha("grey", 0.5)

ggplot(d.plot, aes(x=variable, y=value)) + 
  geom_point(color = color ) +
  geom_smooth(method='lm', formula= y~x)+ 
  ylab('Daily probability of infection') + 
  xlab('Hand hygiene frequency per day') + 
  theme_minimal()+
  theme(legend.position = 'none')

ggsave(paste0('../../../../Desktop/dose_response_COVIDHH.jpeg'), units = 'cm', width = 35, height=25)


