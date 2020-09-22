## Ridge plot figures ## 

library(tidyverse)
library(cowplot); library(patchwork)
library(ggridges)

## Color-blind-friendly palette
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#D55E00", "#E69F00", "#0072B2", "#009E73")

### DATA -----

# Random Intercepts
results_intercept <- readRDS("./Results/results_intercept_lme4.Rds")
results_glmm<- readRDS("./Results/results_intercept_glmer.Rds")

names(results_intercept) <- rep(2:5)
results_intercept <- lapply(results_intercept, as.data.frame)
res.int <- bind_rows(results_intercept, .id="N_levels")
res.int$N_levels <- fct_relevel(res.int$N_levels, "2", "3", "4", "5")

names(results_glmm) <- rep(2:5)
results_glmm<- lapply(results_glmm, as.data.frame)
res.glmm <- bind_rows(results_glmm, .id="N_levels")
res.glmm$N_levels <- fct_relevel(res.glmm$N_levels, "2", "3", "4", "5")


# Random slopes
rslope_norm <- readRDS("./Results/results_slope_lme4.Rds")
rslope_bin <- readRDS("./Results/results_slope_glmer.Rds")

names(rslope_norm) <- rep(2:5)
snorm <- lapply(rslope_norm, as.data.frame)
snorm <- bind_rows(snorm, .id="N_levels")
snorm$N_levels <- fct_relevel(snorm$N_levels, "2", "3", "4", "5")

names(rslope_bin) <- rep(2:5)
sbin <- lapply(rslope_bin, as.data.frame)
sbin <- bind_rows(sbin, .id="N_levels")
sbin$N_levels <- fct_relevel(sbin$N_levels, "2", "3", "4", "5")




### NORMAL PLOTS --------
plots_normal = list()
plots_binomial = list()
for(i in c(0, 1)) {
  # 1 := Singularity
  # 0 := W/o singular values
  
  plots_list = list()
  
  com.theme <- theme(legend.position = "none",
                     axis.line.y = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())
  
  p1 <- ggplot(res.int %>% filter(Singularity == i), aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=25, alpha=0.5, scale=1.5) +
    geom_vline(xintercept = 13.2, col="black", linetype="dotted") +
    xlim(0,40) +
    scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    xlab("SD random intercept") +
    theme(legend.position = "none",
          axis.line.y = element_blank()) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    labs(tag="A)") 
  
  p2 <-  ggplot(res.int %>% filter(Singularity == i), aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=25, alpha=0.5, scale=1.5) +
    scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    geom_vline(xintercept = 66, col="black", linetype="dotted") +
    com.theme +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    xlab("Estimate intercept") 
  
  p3 <-  ggplot(res.int %>% filter(Singularity == i), aes(x=se_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=25, alpha=0.5, scale=1.5) + 
    scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    #geom_vline(xintercept = 0.05, col="black", linetype="dotted") +
    com.theme +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    xlab("SE intercept") 
  
  p1s <- ggplot(snorm %>% filter(Singularity == i), aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=25, alpha=0.5, scale=1.5) +
    scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
    xlab("SD random slope") +
    theme(legend.position = "none", axis.line.y = element_blank()) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    labs(tag="B)")
  
  p2s <-  ggplot(snorm %>% filter(Singularity == i), aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=25, alpha=0.5, scale=1.5) +
    scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    geom_vline(xintercept = 3, col="black", linetype="dotted") +
    com.theme +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    xlab("Estimate slope") 
  
  p3s <- ggplot(snorm %>% filter(Singularity == i), aes(x=se_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=25, alpha=0.5, scale=1.5) +
    scale_y_discrete(name="", 
                     expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    #geom_vline(xintercept = 0.05, col="black", linetype="dotted") +
    com.theme +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    xlab("SE slope") 
  
  plots_normal[[i+1]] = (p1+p2+p3)/(p1s+p2s+p3s) ## option 1
  
  
  ## PLOTS ----
  com.theme <- theme(legend.position = "none",
                     axis.line.y = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())

  
  g1 <- ggplot(res.glmm %>% filter(Singularity == i), aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=30, alpha=0.5, scale=1.2) +
    geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
    theme(legend.position = "none", axis.line.y = element_blank()) +
    xlab("SD random intercept")  + 
    scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    labs(tag="A)")
  
  g2 <- ggplot(res.glmm %>% filter(Singularity == i), aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=30, alpha=0.5, scale=1.2) +
    geom_vline(xintercept = 2, col="black", linetype="dotted") +
    com.theme + 
    scale_y_discrete(name="", 
                     expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    xlab("Estimate intercept") 
  
  g3 <- ggplot(res.glmm %>% filter(Singularity == i), aes(x=se_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=30, alpha=0.5, scale=1.2) +
    com.theme +
    scale_y_discrete(name="", 
                     expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    xlab("SE intercept") 
  
  g1s <- ggplot(sbin %>% filter(Singularity == i), aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=30, alpha=0.5, scale=1.2) +
    geom_vline(xintercept = 0.06, col="black", linetype="dotted") +
    theme(legend.position = "none", axis.line.y = element_blank()) +
    xlab("SD random slope")  + 
    scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    labs(tag="B)")
  
  g2s <- ggplot(sbin %>% filter(Singularity == i), aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=30, alpha=0.5, scale=1.2) +
    geom_vline(xintercept = 0.3, col="black", linetype="dotted") +
    com.theme + 
  scale_y_discrete(name="", 
                   expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    xlab("Estimate slope") 
  
  g3s <- ggplot(sbin %>% filter(Singularity == i), aes(x=se_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(stat="binline", bins=30, alpha=0.5, scale=1.2) +
    com.theme +
    scale_y_discrete(name="", 
                     expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    #scale_fill_manual(values=cbPalette) +
    #scale_color_manual(values=cbPalette) +
    xlab("SE slope") 
  
  
  plots_binomial[[i+1]] = (g1+g2+g3)/(g1s+g2s+g3s) ## option 1
}


jpeg(paste0("Figures/RE_normal_ridgesplot_V3_without_singularity.jpeg" ),width=860, height = 650)
plots_normal[[1]] ## option 1
dev.off()

jpeg(paste0("Figures/RE_normal_ridgesplot_V3_singularity.jpeg"),width=860, height = 650)
plots_normal[[2]] ## option 1
dev.off()


jpeg(paste0("Figures/RE_binomial_ridgesplot_V3_without_singularity.jpeg"),width=860, height = 650)
plots_binomial[[1]]
dev.off()

jpeg(paste0("Figures/RE_binomial_ridgesplot_V3_singularity.jpeg"),width=860, height = 650)
plots_binomial[[2]]
dev.off()
