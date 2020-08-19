## Ridge plot figures ## 

library(tidyverse)
library(cowplot); library(patchwork)
library(ggridges)

results_intercept <- readRDS("./Results/results_intercept_lme4.Rds")
results_glmm<- readRDS("./Results/results_intercept_glmer.Rds")

names(results_intercept) <- rep(2:10)
results_intercept <- lapply(results_intercept, as.data.frame)
res.int <- bind_rows(results_intercept, .id="N_levels")
res.int$N_levels <- fct_relevel(res.int$N_levels, "2", "3", "4", "5", "6", "7", "8", "9", "10")

res.int <- res.int %>%filter(N_levels %in% c("2", "3", "4", "5"))

p1<- ggplot(res.int, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 13.2, col="black", linetype="dotted") +
  xlim(0,40) +
  scale_y_discrete(name="Number of Levels") +
  theme(legend.position = "none") +
  xlab("SD random effects") 
  
p2<-  ggplot(res.int, aes(x=p_value_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 0.05, col="black", linetype="dotted") +
  xlim(0,0.2) +
  theme(legend.position = "none") +
  scale_y_discrete(name="") +
  ggtitle("Normal distribution models")+
  xlab("P-value intercept") 
  
p3 <-  ggplot(res.int, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=2) +
  geom_vline(xintercept = 66, col="black", linetype="dotted") +
  theme(legend.position = "none") +
  scale_y_discrete(name="") +
  xlab("Estimate intercept") 


names(results_glmm) <- rep(2:10)
results_glmm<- lapply(results_glmm, as.data.frame)
res.glmm <- bind_rows(results_glmm, .id="N_levels")
res.glmm$N_levels <- fct_relevel(res.glmm$N_levels, "2", "3", "4", "5", "6", "7", "8", "9", "10")

res.glmm <- res.glmm %>%filter(N_levels %in% c("2", "3", "4", "5"))

p4 <- ggplot(res.glmm, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=3) +
  geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
  #xlim(0,40) +
  scale_y_discrete(name="Number of Levels") +
  theme(legend.position = "none") +
  xlab("SD random effects") 

p5 <- ggplot(res.glmm, aes(x=p_value_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=1) +
  geom_vline(xintercept = 0.05, col="black", linetype="dotted") +
  xlim(0,0.2) +
  theme(legend.position = "none") +
  scale_y_discrete(name="") +
  ggtitle("Binomial distribution models")+
  xlab("P-value intercept") 
  
 p6 <- ggplot(res.glmm, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(stat="binline", bins=30, alpha=0.3, scale=1) +
  geom_vline(xintercept = 2, col="black", linetype="dotted") +
  theme(legend.position = "none") +
  scale_y_discrete(name="") +
  xlab("Estimate intercept") 

jpeg("Figures/test_ridgesplot.jpeg",width=860, height = 600)
(p1+p2+p3)/(p4+p5+p6)
dev.off()

