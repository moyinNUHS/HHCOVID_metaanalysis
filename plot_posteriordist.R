# ========================================== #
# Plot posterior distributions model output 
# ========================================== #

rm(list=ls()) # clear environment
library(ggplot2); library(rstan) # load required libraries 

##############
# Get data for plot

plot.data.list = lapply(list.files('runs/')[grep('Rdata', list.files('runs/'))], function(x){
  
  # load output 
  load(paste0('runs/', x))
  
  # posterior values from stan model output 
  post = rstan::extract(mod_fit, pars = c("RR_masks", "RR_handwashing"))
  
  # quartiles 
  postIQR = lapply(post, quantile, c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  # put into dataframe 
  IQRdf = as.data.frame(do.call('rbind', postIQR))
  IQRdf$intervention = names(postIQR)
  IQRdf$model = x
  
  return(IQRdf)
}
)

plot.data = do.call('rbind', plot.data.list)
plot.data$intervention = as.factor(plot.data$intervention)
plot.data$intervention = factor(plot.data$intervention, levels = c('RR_masks', 'RR_handwashing'))
plot.data$model = as.factor(plot.data$model)
plot.data$model = factor(plot.data$model, levels = c('fit0.9b_sens2.Rdata', 'fit0.9b_sens1.Rdata', 'fit0.9b_main.Rdata'))

##############
# Produce plot 
scaleFUN <- function(x) sprintf("%.2f", x) # x axis 2 decimal places

xmax = max(plot.data$`90%`)

ggplot(plot.data, aes(x = `50%`, y = intervention, group = model, color = model)) +
  geom_errorbarh(aes(xmin = `10%`, xmax=`90%`), position = position_dodge(width = 0.2), size = 1.5, height=0) +
  geom_errorbarh(aes(xmin = `25%`, xmax=`75%`), position = position_dodge(width = 0.2), size = 3, height=0) +
  geom_point(size = 2.5, colour = 'grey90', shape = 4, position = position_dodge(width = 0.2)) +
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'red') +
  scale_y_discrete(limits = unique(plot.data$intervention),
                   labels = rev(c('Hand hygiene', 'Face mask'))) +
  scale_color_manual(values = c("#9ED0E6", '#032B43', '#136F63'), name = '', 
                     labels = rev(c('Main analysis (assumed baseline hand washes 4 times a day and hours of mask worn\n2 hours a day in the face mask intervention arms when not reported)' ,
                                    'Assumed baseline handwashes 6 times a day', 
                                    'Assumed hours of mask worn 4 hours a day in the face mask intervention arms when not reported'))) +
  xlab("Relative risk of acquiring respiratory tract infection per day\nper handwash or per hour of face mask worn")+
  ylab("")+
  scale_x_continuous(lim = c(0.85, xmax), breaks = seq(0.85, xmax, by = 0.05), labels = scaleFUN)+
  theme_minimal() + 
  theme(legend.position = 'bottom', text = element_text(size = 20)) + 
  guides(color = guide_legend(nrow = 3, reverse = TRUE))


# save plot
ggsave('~/Documents/nBox/COVID/Hand_hygiene_covid/graphics/CrI_plot.jpg', units = 'cm', width = 30, height= 15)

