library(dplyr)
library(tidyr)
library(ggplot2)
library(Cairo)
library(vars)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## Daily VARs

linkage <- na.omit(left_join(read.csv('US_CMT_rates.csv'), 
                             read.csv('Un_Ch.csv'), 
                             by = 'DATE'))

var5 <- VAR(linkage[, c('CMT_1MONTH', 'KOF', 'R3M')], p = 5, type = 'both')

daily.irf <- irf(var5, response = "R3M", n.ahead = 450, boot = TRUE) 

response.daily <- cbind.data.frame(as.data.frame(c(1:451)),
                                   as.data.frame(daily.irf$irf),
                                   as.data.frame(daily.irf$Lower),
                                   as.data.frame(daily.irf$Upper))

colnames(response.daily) <- c('Days',
                              'CMT', 'KOF', 'CGB',
                              'low.cmt', 'low.kof', 'low.r3m', 
                              'upp.cmt', 'upp.kof', 'upp.r3m')

## Weekly VARs

systemic.stress <- read.csv('CISS.csv') 

weekly <- na.omit(left_join(systemic.stress, read.csv('Un_Ch.csv'), by = 'DATE'))

var3 <- VAR(weekly[, c('CISS', 'KOF', 'R3M')], p = 3, type = 'both')

weekly.irf <- irf(var3, response = "R3M", n.ahead = 100, boot = TRUE) 

response.weekly <- cbind.data.frame(as.data.frame(c(1:101)),
                                    as.data.frame(weekly.irf$irf),
                                    as.data.frame(weekly.irf$Lower),
                                    as.data.frame(weekly.irf$Upper))

colnames(response.weekly) <- c('Weeks',
                               'CISS', 'KOF', 'CGB',
                               'low.ciss', 'low.kof', 'low.cgb',
                               'upp.ciss', 'upp.kof', 'upp.cgb')


## Monthly VARs

monthly <- read.csv('YieldCurve.csv') %>%
  separate(DATE, c('year', 'month', 'day'), sep = '/') %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month)) %>% 
  group_by(year, month) %>%
  summarise(CR3M = last(R3M),
            CR1Y = last(R1Y)) %>%
  unite(ym, c('year', 'month'), sep = '.') 


money <- subset(read.csv('Money_Supply.csv'), Month >= 2006.03 & Month <= 2018.06)
stock <- subset(read.csv('Stock_Market.csv'), Month >= 2006.03 & Month <= 2018.06)
jln <- read.csv('MacroUncertaintyToCirculate.csv')[549:696,]

larger <- bind_cols(monthly[-149,], money[,2:4], 
                    stock[,c('Volume', 'Turnover', 'Voli', 'TMC', "IndexSH")],
                    jln[,2:4]) 

larger$DATE <- c(1:148)

vecm <- ca.jo(log(larger[,c('M2', 'TMC', 'CR3M')]), 
              type = "eigen", ecdet = "none", K = 2, spec = "transitory") 

SR <- matrix(NA, nrow = 3, ncol = 3) 
LR <- matrix(NA, nrow = 3, ncol = 3) 
LR[1, 2:3] <- 0 
LR[2, 3] <- 0 

svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE,  runs = 500) 

monthly.irf <- irf(svec, response = "CR3M", n.ahead = 60, boot = TRUE) 

response.monthly <- cbind.data.frame(as.data.frame(c(1:61)),
                                     as.data.frame(monthly.irf$irf),
                                     as.data.frame(monthly.irf$Lower),
                                     as.data.frame(monthly.irf$Upper))

colnames(response.monthly) <- c('Months',
                                'M2', 'TMC', 'CGB',
                                'low.m2', 'low.tmc', 'low.r3m', 
                                'upp.m2', 'upp.tmc', 'upp.r3m')

CairoPDF("VARs.pdf", 18, 12)

multiplot(

ggplot(response.daily, aes(Days, CMT)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel A. CMT -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.cmt, ymax = upp.cmt), alpha = "0.2")
,

ggplot(response.daily, aes(Days, KOF)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel A. KOF -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.kof, ymax = upp.kof), alpha = "0.2")
,

ggplot(response.daily, aes(Days, CGB)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel A. CGB -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.r3m, ymax = upp.r3m), alpha = "0.2")
,


ggplot(response.weekly, aes(Weeks, CISS)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel B. CISS -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.ciss, ymax = upp.ciss), alpha = "0.2")
,

ggplot(response.weekly, aes(Weeks, KOF)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel B. KOF -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.kof, ymax = upp.kof), alpha = "0.2")
,

ggplot(response.weekly, aes(Weeks, CGB)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel B. CGB -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.cgb, ymax = upp.cgb), alpha = "0.2")
,


ggplot(response.monthly, aes(Months, M2)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel C. M2 -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.m2, ymax = upp.m2), alpha = "0.2")
,

ggplot(response.monthly, aes(Months, TMC)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel C. TMC -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.tmc, ymax = upp.tmc), alpha = "0.2")
,

ggplot(response.monthly, aes(Months, CGB)) +
  geom_line(size = 1, color = "black") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Panel C. CGB -> CGB') +
  ylab('CGB') +
  geom_ribbon(aes(ymin = low.r3m, ymax = upp.r3m), alpha = "0.2")
,

cols = 3)

dev.off()



