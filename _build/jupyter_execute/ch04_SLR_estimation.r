# Run this cell before continuing.
library(tidyverse)
library(repr)
library(digest)
library(infer)
library(cowplot)
library(broom)
dat <- read.csv("data/Assessment_2015.csv")
dat <- dat %>% filter(ASSESSCLAS=="Residential")  %>% 
        mutate(assess_val = ASSESSMENT / 1000)

set.seed(561)

dat_s <- sample_n(dat, 1000, replace = FALSE)

options(repr.plot.width = 8, repr.plot.height = 5) # Adjust these numbers so the plot looks good in your desktop.

plot_value <- dat_s %>% ggplot(aes(BLDG_METRE, assess_val)) + 
  geom_point() +
  xlab("building size (mts)") + 
  ylab("assessment value ($/1000)") +
     geom_abline(intercept=145,slope=1.5, size=2, col = "grey")+
     geom_abline(intercept=500,slope=-1, size=2, col = "orange")+
     geom_abline(intercept=600,slope=-.5, size=2, col = "red")+
     geom_abline(intercept=10,slope=4, size=2, col = "green")+
     geom_abline(intercept=147,slope=1.9, size=2, col = "purple")+
  geom_smooth(method = lm, se = FALSE, color = "blue") +
  ggtitle("Random Sample and Estimated Linear Regression")


plot_value

lm_s <- lm(assess_val~BLDG_METRE,data=dat_s)
tidy(lm_s)%>%mutate_if(is.numeric, round, 3)

plot_value <- dat %>% ggplot(aes(BLDG_METRE, assess_val)) + 
  geom_point(color = "grey") +
  xlab("building size (mts)") + 
  ylab("assessed value ($/1000)") +
  geom_point(data = dat_s, aes(BLDG_METRE, assess_val), color = "black") +
  geom_smooth(method = lm, se = FALSE, color = "blue") +
  ggtitle("Random Sample and Estimated Linear Regression")


plot_value


