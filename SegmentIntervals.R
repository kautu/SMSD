library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

library(Cairo)

##
interest.rate$segment <- 0 

interest.rate[1:sort(cp[1:9,1])[1],9] <- 1

#interest.rate[sort(cp[1:9,1])[1]:sort(cp[1:9,1])[2],9] <- 2
for (i in 1:8) {
  interest.rate[sort(cp[1:9,1])[i]:sort(cp[1:9,1])[i+1],9] <- i+1
}

interest.rate[sort(cp[1:9,1])[9]:3109,9] <- 10

interest.rate$R3M_1 <- c(interest.rate$R3M[-1], interest.rate[3109,1])
interest.rate$diff <- interest.rate$R3M_1 - interest.rate$R3M

interest.rate$diff[which(interest.rate$diff>1)] <- 0.62
interest.rate$trend <- 'negative'
interest.rate$trend[which(interest.rate$diff>0)] <- 'positive'
interest.rate$trend[which(interest.rate$diff==0)] <- NA

interest.rate$location <- c(1:3109)
interest.rate$segment <- as.factor(interest.rate$segment)

#summary(interest.rate$R3M)

CairoPDF('r3mdensity.pdf', width = 5.83, height = 3.6)
ggplot(interest.rate, aes(R3M)) +
  geom_histogram(aes(y=..density..), binwidth = 0.025) +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(xintercept = 1.9900, linetype="dashed", color = '#56B4E9', size = 0.8) +
  geom_vline(xintercept = 3.1428, linetype="dashed", color = '#56B4E9', size = 0.8)
dev.off()

CairoPDF('r3mfill.pdf', width = 5.83, height = 3.6)
ggplot(interest.rate, aes(R3M, fill = segment)) +
  geom_histogram(binwidth = 0.025) +
  scale_fill_viridis_d(option = 'C') +
  geom_vline(xintercept = 2.6179, linetype="dashed", color = 'antiquewhite2', size = 0.8)
dev.off()


CairoPDF('segment.pdf', width = 5.83, height = 3.6)
ggplot(interest.rate, aes(segment, R3M, fill = segment)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = 'C') 
dev.off()

cusum <- left_join(data.frame(changepoints), interest.rate[,c(9,13)], by = 'location') %>% 
  arrange(-max.proj.cusum) 
#summary(cusum$max.proj.cusum)

CairoPDF('cusum.pdf', width = 5.83, height = 3.6)
ggplot(subset(cusum, max.proj.cusum > 12.553), aes(location, log(max.proj.cusum))) +
  geom_line(alpha = 1/5) +
  geom_point(size = 0.6, alpha = 2/5) +
  scale_x_continuous(breaks = c(0, 305, 673, 843, 1185, 1424, 1864, 2223, 2442, 2783, 3109)) +
  geom_vline(xintercept = 1185, linetype="dashed", color = '#FF3366', size = 0.5) +
  geom_vline(xintercept =  305, linetype="dashed", color = '#FF3366', size = 0.5) +
  geom_vline(xintercept =  673, linetype="dashed", color = '#FF3366', size = 0.5) +
  geom_vline(xintercept = 2223, linetype="dashed", color = '#FF3366', size = 0.2) +
  geom_vline(xintercept = 2783, linetype="dashed", color = '#FF3366', size = 0.2) +
  geom_vline(xintercept = 1864, linetype="dashed", color = '#FF3366', size = 0.2) +
  geom_vline(xintercept =  843, linetype="dashed", color = '#FF3366', size = 0.2) +
  geom_vline(xintercept = 2442, linetype="dashed", color = '#FF3366', size = 0.2) +
  geom_vline(xintercept = 1424, linetype="dashed", color = '#FF3366', size = 0.2)
dev.off()


## Supplementary
ggplot(interest.rate, aes(R3M, R5Y, colour = as.factor(segment))) +
  geom_point(alpha = 2/5)+
  scale_colour_viridis_d(option = 'C') 

ggplot(interest.rate, aes(as.Date(row.names(interest.rate), '%Y/%m/%d'), diff, 
                          colour = as.factor(segment)
                          )) +
  geom_line() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_colour_viridis_d(option = 'C') 


ggplot(interest.rate, aes(as.Date(row.names(interest.rate), '%Y/%m/%d'), diff, 
                          colour = trend#colour = as.factor(segment)
)) +
  geom_line() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") 


## Steps for interest rate

#summary(interest.rate$R1Y)
CairoPDF('R1Y_Step.pdf', 12, 7.4)
ggplot(interest.rate[cusum$location[1:163],], aes(location, R1Y)) +
  geom_step(colour = "red", size = 1, direction = "hv") +
  scale_x_continuous(breaks = c(0, 305, 673, 843, 1185, 1424, 1864, 2223, 2442, 2783, 3109)) +
  #geom_hline(yintercept = 2.8116, linetype = 'dashed', color = '#56B4E9', size = 0.8) +
  geom_hline(yintercept = 2.1346, linetype = 'dashed', color = '#56B4E9', size = 0.8) +
  geom_hline(yintercept = 3.3298, linetype = 'dashed', color = '#56B4E9', size = 0.8)
dev.off()
#                     lables = c("2006/3/1",
#                                "2007/5/18", "2008/11/4", "2009/7/9",  "2010/11/22", "2011/11/7", 
#                                "2013/8/8",  "2015/1/15", "2015/12/3", "2017/4/14" ,
#                                "2018/7/31"))

