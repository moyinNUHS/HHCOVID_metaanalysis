fit0.9<-readRDS("archive/fit0.9_5k_main.RDS")
library(ggplot2); library(reshape); library(bayesplot); library(ggpubr); library(rstan)
study_names = c("Aiello 2012", "Simmerman 2011", "Larson 2010", "Nicholson 2014", "Suess 2012", "Pandejpong 2012")
p1<-NULL
for(i in 1:21) p1<-c(p1, paste0("predicted_mean[",i,"]") )
p2<-NULL
for(i in 1:21) p2<-c(p2, paste0("predicted_mean_new_trial[",i,"]") )
params<-c(p1,p2)
overallextract = rstan::extract(fit0.9, pars= params)#gives posterior values
posterior = as.data.frame(matrix(unlist(overallextract), byrow = F, ncol = length(params)))
colnames(posterior) = params
quants = c(0.025, 0.1,0.25, 0.5, 0.75,0.9, 0.975)
plotdf = as.data.frame(t(sapply(posterior, quantile, probs = quants)))
plotdfnew<-plotdf[1:21,]
plotdfnew2<-plotdf[22:42,]

## plot trend line
colnames(plotdfnew)<- paste0("pred_mean", quants)
colnames(plotdfnew2)<- paste0("pred_mean_new", quants)
plotdf<-cbind(plotdfnew,plotdfnew2)
plotdf$numHWs<-0:20
rm(plotdfnew,plotdfnew2)


h <- ggplot(plotdf, aes(numHWs))
h +
geom_ribbon(aes(ymin = pred_mean0.1, ymax = pred_mean0.9), fill = "grey90") +
geom_ribbon(aes(ymin = pred_mean0.25, ymax = pred_mean0.75), fill = "grey80") +
  
  geom_line(aes(y = pred_mean0.5)) +
  ylim(0.0,0.05)

####plot predicted overall metaregression results
params = c("c0", "b0", "b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","c[1]","c[2]","c[3]","c[4]","c[5]" )
params.hh = params[grep('b', params)]
params.mask = params[grep('c', params)]
overallextract = rstan::extract(fit0.9, pars= params)#gives posterior values
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

ggsave(filename = "fit0.9_metareg_posterior_masks.jpeg", width = 50, height = 20, units = 'cm')



plot.df<-exp(posterior)
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
ggsave(filename = paste0('fit0.9_metareg.jpeg'), width = 20, height = 15, units = 'cm')

