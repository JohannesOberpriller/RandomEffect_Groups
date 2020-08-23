## Ridge plot figures ## 

library(tidyverse)
library(cowplot); library(patchwork)
library(ggridges)

### RANDOM INTERCEPT --------

results_intercept <- readRDS("./Results/results_intercept_lme4.Rds")
results_glmm<- readRDS("./Results/results_intercept_glmer.Rds")

names(results_intercept) <- rep(2:10)
results_intercept <- lapply(results_intercept, as.data.frame)
res.int <- bind_rows(results_intercept, .id="N_levels")
res.int$N_levels <- fct_relevel(res.int$N_levels, "2", "3", "4", "5", "6", "7", "8", "9", "10")
res.int <- res.int %>%filter(N_levels %in% c("2", "3", "4", "5"))

names(results_glmm) <- rep(2:10)
results_glmm<- lapply(results_glmm, as.data.frame)
res.glmm <- bind_rows(results_glmm, .id="N_levels")
res.glmm$N_levels <- fct_relevel(res.glmm$N_levels, "2", "3", "4", "5", "6", "7", "8", "9", "10")
res.glmm <- res.glmm %>%filter(N_levels %in% c("2", "3", "4", "5"))


## PLOTS -----
com.theme <- theme(legend.position = "none",
                   axis.line.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())

p1 <- ggplot(res.int, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 13.2, col="black", linetype="dotted") +
  xlim(0,40) +
  scale_y_discrete(name="Number of Levels") +
  xlab("SD random intercept") +
  theme(legend.position = "none", axis.line.y = element_blank()) +
  labs(tag="A)")

p2 <-  ggplot(res.int, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 66, col="black", linetype="dotted") +
  com.theme +
  scale_y_discrete(name="") + ggtitle("Normal distribution models") + 
  xlab("Estimate intercept") 

p3 <-  ggplot(res.int, aes(x=p_value_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 0.05, col="black", linetype="dotted") +
  xlim(0,0.2) +
  com.theme +
  scale_y_discrete(name="") + 
  xlab("P-value intercept") 

p4 <- ggplot(res.glmm, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=3) +
  geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
  theme(legend.position = "none", axis.line.y = element_blank()) +
  xlab("SD random intercept")  + ylab("Number of levels") +
  labs(tag="B)")

p5 <- ggplot(res.glmm, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=1) +
  geom_vline(xintercept = 2, col="black", linetype="dotted") +
  com.theme + ggtitle("Binomial distribution models")+
  scale_y_discrete(name="") +
  xlab("Estimate intercept") 

p6 <- ggplot(res.glmm, aes(x=p_value_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=1) +
  geom_vline(xintercept = 0.05, col="black", linetype="dotted") +
  xlim(0,0.2) +
  com.theme +
  scale_y_discrete(name="") +
  xlab("P-value intercept") 
  

jpeg("Figures/RE_intercept_ridgesplot.jpeg",width=860, height = 600)
(p1+p2+p3)/(p4+p5+p6) ## option 1
dev.off()


### RANDOM SLOPES ----

rslope_norm <- readRDS("./Results/results_slope_lme4.Rds")
rslope_bin <- readRDS("./Results/results_slope_glmer.Rds")


names(rslope_norm) <- rep(2:10)
snorm <- lapply(rslope_norm, as.data.frame)
snorm <- bind_rows(snorm, .id="N_levels")
snorm$N_levels <- fct_relevel(snorm$N_levels, "2", "3", "4", "5", "6", "7", "8", "9", "10")
snorm <- snorm %>% filter(N_levels %in% c("2", "3", "4", "5"))

names(rslope_bin) <- rep(2:10)
sbin <- lapply(rslope_bin, as.data.frame)
sbin <- bind_rows(sbin, .id="N_levels")
sbin$N_levels <- fct_relevel(sbin$N_levels, "2", "3", "4", "5", "6", "7", "8", "9", "10")

sbin <- sbin%>%filter(N_levels %in% c("2", "3", "4", "5"))

## PLOTS ----
com.theme <- theme(legend.position = "none",
                   axis.line.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())

p1s <- ggplot(snorm, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 0.4 col="black", linetype="dotted") +
  scale_y_discrete(name="Number of Levels") +
  xlab("SD random slope") +
  theme(legend.position = "none", axis.line.y = element_blank()) +
  labs(tag="A)")

p2s <-  ggplot(snorm, aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 3, col="black", linetype="dotted") +
  com.theme +
  scale_y_discrete(name="") + ggtitle("Normal distribution models") + 
  xlab("Estimate slope") 

p3s <-  ggplot(snorm, aes(x=p_value_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 0.05, col="black", linetype="dotted") +
  xlim(-0.01,0.2) +
  com.theme +
  scale_y_discrete(name="") + 
  xlab("P-value slope") 

p4s <- ggplot(sbin, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 0.06, col="black", linetype="dotted") +
  theme(legend.position = "none", axis.line.y = element_blank()) +
  xlab("SD random slope")  + ylab("Number of levels") +
  labs(tag="B)")

p5s <- ggplot(sbin, aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=1) +
  geom_vline(xintercept = 0.3, col="black", linetype="dotted") +
  com.theme + ggtitle("Binomial distribution models")+
  scale_y_discrete(name="") +
  xlab("Estimate slope") 

p6s <- ggplot(sbin, aes(x=p_value_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=1) +
  geom_vline(xintercept = 0.05, col="black", linetype="dotted") +
  xlim(-0.01,0.2) +
  com.theme +
  scale_y_discrete(name="") +
  xlab("P-value slope") 


jpeg("Figures/RE_slope_ridgesplot.jpeg",width=860, height = 600)
(p1s+p2s+p3s)/(p4s+p5s+p6s) ## option 1
dev.off()
