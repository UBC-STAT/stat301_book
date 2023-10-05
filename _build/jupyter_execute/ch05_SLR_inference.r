# packages
library(tidyverse)
library(repr)
library(infer)
library(cowplot)
library(broom)

#data
dat <- read.csv("data/Assessment_2015.csv")
dat <- dat %>% filter(ASSESSCLAS=="Residential")  %>% 
        mutate(assess_val = ASSESSMENT / 1000)

set.seed(561)

dat_s <- sample_n(dat, 1000, replace = FALSE)


lm_value <- lm(assess_val~BLDG_METRE,data=dat_s)%>%
                tidy()%>%mutate_if(is.numeric, round, 3)
many_SLR <- lm_value %>% select(term,estimate)

many_SLR

set.seed(301)

dat_rep2 <- rep_sample_n(dat, size = 1000)

# ANOTHER POINT ESTIMATES

lm_value_rep2 <- tidy(lm(assess_val~BLDG_METRE,data=dat_rep2))  %>% 
                    select(estimate) %>% mutate_if(is.numeric, round, 3)  %>% pull()
many_SLR <- many_SLR  %>% mutate(estimate2 = lm_value_rep2)

many_SLR

set.seed(30)

dat_rep3 <- rep_sample_n(dat, size = 1000)

# ANOTHER POINT ESTIMATES

lm_value_rep3 <- tidy(lm(assess_val~BLDG_METRE,data=dat_rep3))  %>% 
                    select(estimate) %>% mutate_if(is.numeric, round, 3) %>% pull()
many_SLR <- many_SLR  %>% mutate(estimate3 = lm_value_rep3)


many_SLR

lm_value

options(repr.plot.width = 8, repr.plot.height = 5)

plot_value <- dat_s %>% ggplot(aes(BLDG_METRE, assess_val)) + 
  xlab("building size (mts)") + 
  ylab("assessed value ($/1000)") +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, color = "blue", level=0.99) +
  ggtitle("The bands are NOT the SE of the estimators")

plot_value

lm_value

lm_value_CIs <- tidy(lm(assess_val~BLDG_METRE,data=dat_s), conf.int = TRUE) %>% mutate_if(is.numeric, round, 3)

lm_v <- lm(assess_val~BLDG_METRE,data=dat_s)
tidy(data=lm_v, conf.int = TRUE)

lm_value_CIs

set.seed(123)

n <- 1000
B <- 10000

lm_boot <- replicate(B, {
  sample_n(dat_s, n, replace = TRUE) %>%
    lm(formula = assess_val~BLDG_METRE, data = .) %>%
    .$coef
})
lm_boot <- data.frame(boot_intercept = lm_boot[1, ], boot_slope = lm_boot[2, ])

head(lm_boot)

tail(lm_boot)

options(repr.plot.width = 8, repr.plot.height = 5)

slope_sampling_dist <-  ggplot(lm_boot, aes(x = boot_slope)) +
    geom_histogram(color = "white", fill = "blue") +
    xlab("Estimated Slope") +
    ggtitle("Sampling distribution for the estimator of the slope")

slope_sampling_dist

options(repr.plot.width = 8, repr.plot.height = 5)

slope_sampling_dist <-  ggplot(lm_boot, aes(x = boot_slope)) +
    geom_histogram(color = "white", fill = "blue") +
    xlab("Estimated Slope") +
    ggtitle("Sampling distribution for the estimator of the slope")+
    geom_vline(aes(xintercept = quantile(boot_slope,0.025)),size=1)+
    geom_vline(aes(xintercept = quantile(boot_slope,0.975)),size=1) +
    geom_segment(aes(x = quantile(boot_slope,0.025), y = 0, xend = quantile(boot_slope,0.975), yend = 0, 
        colour = "red", linewidth= 2),show.legend = FALSE)


slope_sampling_dist
