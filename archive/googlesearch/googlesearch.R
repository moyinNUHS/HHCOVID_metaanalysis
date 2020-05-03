df1 = read.csv('archive/googlesearch/multiTimeline.csv', skip = 2, as.is = T)
df2 = read.csv('archive/googlesearch/multiTimeline1.csv', skip = 2, as.is = T)

d = cbind.data.frame(df1, df2[,-1])
for (i in 2:ncol(d)){
  print(i)
  d[which(d[i]=='<1'), i] = 0
  d[i] = as.numeric(d[,i])
}

d.combined = data.frame(Day = d$Day, 
                        Coronavirus = d$covid...Worldwide.+d$coronavirus...Worldwide./2, 
                        Handwash = d$Hand.sanitizer...Worldwide.+d$hand.wash...Worldwide.+d$clean.hands...Worldwide./3,
                        Quarantine = d$quarantine...Worldwide.+d$stay.home...Worldwide./2, 
                        Mask = d$mask...Worldwide.)

dm = data.table::melt(d.combined, id.var = 'Day')
dm$Day = ymd(dm$Day)
ggplot(data = dm , aes(x = Day, y = value, color = variable)) + 
  geom_line() + 
  ylab('Normalised google search volume (worldwide)') + 
  scale_color_manual(values = c('#D45113', '#F3B700', '#0E9594', '#33658A')) + 
  xlab('Months in 2020') +
  theme_bw()+
  theme(legend.position = 'bottom', 
        legend.title = element_blank())

ggsave(paste0('googleCOVIDHH.jpeg'), units = 'cm', width = 15, height=15)

