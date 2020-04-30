load(file = '../../../../Desktop/model0.5HHCOVID_100k.Rdata')
library(ggplot2); library(reshape); library(bayesplot); library(ggpubr); library(rstan)

study_names = c("Aiello 2012", "Simmerman 2011", "Larson 2010", "Nicholson 2014", "Suess 2012", "Pandejpong 2012")
params = c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","c[1]","c[2]","c[3]","c[4]","c[5]" )
params.hh = params[grep('b', params)]
params.mask = params[grep('c', params)]

overallextract = rstan::extract(fit0.5, pars= params)#gives posterior values
posterior = as.data.frame(matrix(unlist(overallextract), byrow = F, ncol = length(params)))
colnames(posterior) = params

post.hh = posterior[, grep('b',colnames(posterior))]
post.mask = posterior[, grep('c',colnames(posterior))]

hh = mcmc_areas(post.hh, pars =  params.hh, prob = 0.8, prob_outer = 0.99) + 
  scale_y_discrete(limits = rev(params.hh), 
                   labels = rev(c('Overall mean', study_names))) +
  scale_x_continuous(limits = c(-2, 2))+
  labs(title = 'Hand hygiene')+
  xlab('')+
  theme_minimal()+
  theme(text = element_text(size=20))

mask = mcmc_areas(post.mask, pars =  params.mask, prob = 0.8, prob_outer = 0.99) + 
  scale_y_discrete(limits = rev(c(params.mask,'')), 
                   labels = rev(c('Overall mean', study_names[-6], ''))) +
  scale_x_continuous(limits = c(-2, 2))+
  labs(title = 'Face mask')+
  xlab('')+
  theme_minimal()+
  theme(text = element_text(size=20))

ggarrange(hh, mask)

ggsave(filename = paste0('../../../../Desktop/metareg_posterior.jpeg'), width = 50, height = 20, units = 'cm')

#### plot 80%CrI
quants = c(0.025, 0.1, 0.5, 0.9, 0.975)
plotdf = as.data.frame(t(sapply(plot.df, quantile, probs = quants)))
plotdf$para = rownames(plotdf)

empty = as.data.frame(matrix(c(rep(NA, ncol(plotdf)-1), 'holder', 
                               rep(NA, ncol(plotdf)-1),  'c.holder',
                               rep(NA, ncol(plotdf)-1), 'b.holder'), byrow = T, ncol = ncol(plotdf)))
colnames(empty) = colnames(plotdf)
empty[,1:5] = sapply(empty[,1:5], as.numeric)
plotdf.f = rbind.data.frame(plotdf, empty, stringsAsFactors = FALSE)
plotdf.f$type = 'HH'
plotdf.f$type[grep('c',plotdf.f$para)] = 'mask'
plotdf.f$esttype = 'ind'
plotdf.f$esttype[grep('0',plotdf.f$para)] = 'overall'

estimates = paste0(round(plotdf$`50%`, 2), ' (', round(plotdf$`10%`, 2), '-', round(plotdf$`90%`, 2), ')')

ggplot(plotdf.f, aes(x = para, colour = type)) + 
  geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`), stat = "identity", size = 0.7) +
  geom_linerange(aes(ymin = `10%`, ymax = `90%`), stat = "identity", size = 1.5) +
  geom_point(aes(y =`50%`, shape = esttype, size = 0.2)) +
  scale_shape_manual(values=c(20, 18))+
  xlab('')+
  ylab('Relative risk')+
  scale_color_manual(values = c("#003b76", "#FFA500"), labels = c('Hand hygiene', 'Mask'))+
  labs(color='') +
  scale_y_continuous(limits = c(-2, 3.5), breaks = c(0, 1, 2))+
  scale_x_discrete(limits = rev(c('holder','b.holder','b[1]', 'b[2]','b[3]','b[4]','b[5]','b[6]', 'b0', 
                                  'c.holder', 'c[1]', 'c[2]','c[3]','c[4]','c[5]', 'c0')), 
                   labels = rev(c('','Hand hygiene', study_names, 'Overall mean', 'Mask', study_names[-6], 'Overall mean'))) +
  geom_segment(aes(x = 0, y = 1, xend = 15, yend = 1) , colour = "darkgrey", linetype = 'dotted') +
  geom_vline(xintercept = 7.5, linetype="solid", color = "black", size= 0.2)+
  annotate('text', label = 'Relative risk (80% CrI)', x = 16, y = -1)+
  annotate('text', label = estimates[3], x = 14, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[4], x = 13, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[5], x = 12, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[6], x = 11, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[7], x = 10, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[8], x = 9, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[2], x = 8, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[9], x = 6, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[10], x = 5, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[11], x = 4, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[12], x = 3, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[13], x = 2, y = -1, color = '#333333', size = 3)+
  annotate('text', label = estimates[1], x = 1, y = -1, color = '#333333', size = 3)+
  coord_flip()+
  theme(legend.position = 'none', 
        panel.background = element_rect(fill = "white"),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(hjust = 0.55), 
        axis.text.y = element_text(face=c("bold","plain", "plain", "plain", "plain",  "plain", "bold.italic",
                                          "bold", "plain", "plain", "plain", "plain", "plain", "plain", "bold.italic"), 
                                   size = c(9, 8, 8, 8, 8, 8, 10, 9, 8, 8, 8, 8, 8, 8, 10))) 
ggsave(filename = paste0('../../../../Desktop/metareg.jpeg'), width = 20, height = 15, units = 'cm')


#####compare to each trial 
load(file = '../../../../Desktop/trial1.metareg.Rdata')
load(file = '../../../../Desktop/trial2.metareg.Rdata')
load(file = '../../../../Desktop/trial3.metareg.Rdata')
load(file = '../../../../Desktop/trial4.metareg.Rdata')
load(file = '../../../../Desktop/trial5.metareg.Rdata')
load(file = '../../../../Desktop/trial6.metareg.Rdata')

trial1extract<-extract(fit_trial1, pars=c("c", "b" ))#gives posterior values
df1 <- data.frame(matrix(unlist(trial1extract), nrow=lengths(trial1extract)))
colnames(df1) = c("c_trial1", "b_trial1" )

trial2extract<-extract(fit_trial2, pars=c("c", "b" ))#gives posterior values
df2 <- data.frame(matrix(unlist(trial2extract), nrow=lengths(trial2extract)))
colnames(df2) = c("c_trial2", "b_trial2" )

trial3extract<-extract(fit_trial3, pars=c("c", "b" ))#gives posterior values
df3 <- data.frame(matrix(unlist(trial3extract), nrow=lengths(trial3extract)))
colnames(df3) = c("c_trial3", "b_trial3" )

trial4extract<-extract(fit_trial4, pars=c("c", "b" ))#gives posterior values
df4 <- data.frame(matrix(unlist(trial4extract), nrow=lengths(trial4extract)))
colnames(df4) = c( "c_trial4", "b_trial4" )

trial5extract<-extract(fit_trial5, pars=c("c", "b" ))#gives posterior values
df5 <- data.frame(matrix(unlist(trial5extract), nrow=lengths(trial5extract)))
colnames(df5) = c("c_trial5", "b_trial5" )

trial6extract<-extract(fit_trial6, pars=c("b" ))#gives posterior values
df6 <- data.frame(matrix(unlist(trial6extract), nrow=lengths(trial6extract)))
colnames(df6) = c("b_trial6")

plot.df = cbind(df, df1, df2, df3, df4, df5, df6)

quants = c(0.025, 0.1, 0.5, 0.9, 0.975)
plotdf = as.data.frame(t(sapply(plot.df, quantile, probs = quants)))
plotdf$para = rownames(plotdf)

ggplot(plotdf, aes(x = para)) + 
  geom_boxplot(aes(ymin = `2.5%`, lower = `10%`, middle = `50%`, upper = `90%`, ymax = `97.5%`),
               stat = "identity") +
  ylim(c(-1.65, 1.25))+ 
  annotate('text', label = 'Posterior too low to show', x = 'c_trial4', y = -3) + 
  xlab('')+
  scale_x_discrete(limits = rev(c('b0', 'b[1]', 'b_trial1', 'b[2]','b_trial2','b[3]','b_trial3','b[4]','b_trial4','b[5]','b_trial5','b[6]','b_trial6',
                                  'c0', 'c[1]', 'c_trial1', 'c[2]','c_trial2','c[3]','c_trial3','c[4]','c_trial4','c[5]','c_trial5'))) +
  coord_flip()+ 
  theme_minimal()