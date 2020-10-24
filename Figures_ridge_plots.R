## Ridge plot figures ## 

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(patchwork)
library(ggridges)
library(ggpubr) # include table in the plot
library(viridis) # color blind pallete

## Color-blind-friendly palette
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#scale_fill_manual(values=cbPalette) +
#scale_color_manual(values=cbPalette) +
#cbPalette <- c("#D55E00", "#E69F00", "#0072B2", "#009E73")

# quantile_fun=function(x,...)mean(x) # mean lines


### DATA -----

# Random Intercepts
results_intercept <- readRDS("./Results/results_intercept_lme4.Rds")
results_glmm <- readRDS("./Results/results_intercept_glmer.Rds")

names(results_intercept) <- rep(2:8)
results_intercept <- lapply(results_intercept, as.data.frame)
res.int <- bind_rows(results_intercept, .id="N_levels")
res.int <- filter(res.int, N_levels %in% c("2","3","4","5"))

names(results_glmm) <- rep(2:8)
results_glmm<- lapply(results_glmm, as.data.frame)
res.glmm <- bind_rows(results_glmm, .id="N_levels")
res.glmm <- filter(res.glmm, N_levels %in% c("2","3","4","5"))


# Random slopes
rslope_norm <- readRDS("./Results/results_slope_lme4.Rds")
rslope_bin <- readRDS("./Results/results_slope_glmer.Rds")

names(rslope_norm) <- rep(2:8)
snorm <- lapply(rslope_norm, as.data.frame)
snorm <- bind_rows(snorm, .id="N_levels")
snorm <- filter(snorm, N_levels %in% c("2","3","4","5"))

names(rslope_bin) <- rep(2:5) ## results only up to 5 groups?
sbin <- lapply(rslope_bin, as.data.frame)
sbin <- bind_rows(sbin, .id="N_levels")
sbin <- filter(sbin, N_levels %in% c("2","3","4","5"))

##### SINGULARITY TABLES -----

sil <- res.int %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Singularity)*100/n() )
sib <- res.glmm %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)` = sum(Singularity)*100/n() )
ssl <- snorm %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)` = sum(Singularity)*100/n() )
ssb <- sbin %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)` = sum(Singularity)*100/n() )

sing <- bind_rows(list(linear.inter=sil, binomial.inter=sib, linear.slope=ssl, 
                       binomial.slope=ssb), .id="model") %>%
  separate(model, c("model", "estimate")) %>%
  arrange(estimate, desc(N_levels))

base_size = 20
uni <- unit(c(17,17), "mm")
lint <- ggtexttable(sing %>% filter(model=="linear", estimate=="inter") %>%
                      select(`Singularity (%)`), 
            rows = NULL,
            theme = ttheme(base_style = "blank",
                           base_size = base_size,
                           padding = uni,
                           tbody.style = tbody_style(color="black",
                                                     fill = "white",
                                                     size=13),
                           rownames.style = rownames_style(linewidth = 50)
                                      )) 
lslop <- ggtexttable(sing %>% filter(model=="linear", estimate=="slope") %>%
                       select(`Singularity (%)`), 
                     rows = NULL,
                     theme = ttheme(base_style = "blank",
                                    base_size = base_size,
                                    padding = uni,
                                    tbody.style = tbody_style(color="black",
                                                              fill = "white",
                                                              size=13),
                                    rownames.style = rownames_style(linewidth = 50)
                     ))
bint <- ggtexttable(sing %>% filter(model=="binomial", estimate=="inter") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = base_size,
                                   padding = uni,
                                   tbody.style = tbody_style(color="black",
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)
                    )) 
bslop <- ggtexttable(sing %>% filter(model=="binomial", estimate=="slope") %>%
                       select(`Singularity (%)`), 
                     rows = NULL,
                     theme = ttheme(base_style = "blank",
                                    base_size = base_size,
                                    padding = uni,
                                    tbody.style = tbody_style(color="black",
                                                              fill = "white",
                                                              size=13),
                                    rownames.style = rownames_style(linewidth = 50)
                     ))



### PLOTS --------

com.theme <- theme(legend.position = "none",
                   axis.line.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   text = element_text(size=25),
                   axis.text = element_text(size=20))


plots_normal = list()
plots_binomial = list()
for(i in c(0, 1)) {
  # 1 := Singularity
  # 0 := W/o singular values
  
  plots_list = list()
  


## NORMAL ----
  p1 <- ggplot(res.int %>% filter(Singularity %in% c(0,i)), aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                        quantiles=2) +
    geom_vline(xintercept = 13.2, col="black", linetype="dotted") +
    scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    xlab("SD random intercept") +
    theme(legend.position = "none", 
          text = element_text(size=25),axis.text = element_text(size=20),
          axis.line.y = element_blank()) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    labs(tag="a)", size=30) +xlim(0, 1.05*max(res.int$stddev_randeff))
  
  p2 <-  ggplot(res.int %>% filter(Singularity %in% c(0,i)), aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                         quantiles=2) +
    scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    geom_vline(xintercept = 66, col="black", linetype="dotted") +
    com.theme +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab("Estimate intercept")  +xlim(min(res.int$estimate_intercept),max(res.int$estimate_intercept))
  
  p3 <-  ggplot(res.int %>% filter(Singularity %in% c(0,i)), aes(x=se_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                         quantiles=2) + 
    scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    com.theme +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab("SE intercept") + xlim(0,1.05*max(res.int$se_intercept))
  
  p1s <- ggplot(snorm %>% filter(Singularity %in% c(0,i)), aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                         quantiles=2) +
    scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
    xlab("SD random slope") +
    theme(legend.position = "none", 
          text = element_text(size=25),axis.text = element_text(size=20),
          axis.line.y = element_blank()) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    labs(tag="b)", size=30) + xlim(0,1.05*max(snorm$stddev_randeff))
  
  p2s <-  ggplot(snorm %>% filter(Singularity %in% c(0,i)), aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                         quantiles=2) +
    scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    geom_vline(xintercept = 3, col="black", linetype="dotted") +
    com.theme +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab("Estimate slope") + xlim(min(snorm$estimate_effect),max(snorm$estimate_effect))
  
  p3s <- ggplot(snorm %>% filter(Singularity %in% c(0,i)), aes(x=se_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                         quantiles=2) +
    scale_y_discrete(name="", 
                     expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    com.theme +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab("SE slope") + xlim(0,1.05*max(snorm$se_effect))
  
  if(i==0){
    plots_normal[[i+1]] = (p1+p2+p3+lint+plot_layout(ncol=4))/(p1s+p2s+p3s+lslop+ plot_layout(ncol=4))
  }else{
    plots_normal[[i+1]] = (p1+p2+p3)/(p1s+p2s+p3s)
  }
  
  
 
   
  ## BINOMIAL ----
  
  g1 <- ggplot(res.glmm %>% filter(Singularity %in% c(0,i)), aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(alpha=0.5, scale=1.2, quantile_lines=TRUE, 
                        quantiles=2) +
    geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
    theme(legend.position = "none", 
          text = element_text(size=25),axis.text = element_text(size=20),
          axis.line.y = element_blank()) +
    xlab("SD random intercept")  + 
    scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    labs(tag="a)", size=30) + 
    xlim(0,1.05*max(res.glmm$stddev_randeff))
  
  g2 <- ggplot(res.glmm %>% filter(Singularity %in% c(0,i)), aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(alpha=0.5, scale=1.2, quantile_lines=TRUE, 
                        quantiles=2) +
    geom_vline(xintercept = 2, col="black", linetype="dotted") +
    com.theme + 
    scale_y_discrete(name="", 
                     expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab("Estimate intercept") + xlim(min(res.glmm$estimate_intercept),max(res.glmm$estimate_intercept))
  
  g3 <- ggplot(res.glmm %>% filter(Singularity %in% c(0,i)), aes(x=se_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(alpha=0.5, scale=1.2, quantile_lines=TRUE, 
                        quantiles=2) +
    com.theme +
    scale_y_discrete(name="", 
                     expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab("SE intercept") +xlim(0,1.05*max(res.glmm$se_intercept))
  
  g1s <- ggplot(sbin %>% filter(Singularity %in% c(0,i)), aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(alpha=0.5, scale=1.2, quantile_lines=TRUE, 
                        quantiles=2) +
    geom_vline(xintercept = 0.24, col="black", linetype="dotted") +
    theme(legend.position = "none", 
          text = element_text(size=25),axis.text = element_text(size=20),
          axis.line.y = element_blank()) +
    xlab("SD random slope")  + 
    scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    labs(tag="b)", size=30) +xlim(0,1.05*max(sbin$stddev_randeff))
  
  g2s <- ggplot(sbin %>% filter(Singularity %in% c(0,i)), aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(alpha=0.5, scale=1.2, quantile_lines=TRUE, 
                        quantiles=2) +
    geom_vline(xintercept = 0.3, col="black", linetype="dotted") +
    com.theme + 
  scale_y_discrete(name="", 
                   expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab("Estimate slope")  +xlim(min(sbin$estimate_effect),max(sbin$estimate_effect))
  
  g3s <- ggplot(sbin %>% filter(Singularity %in% c(0,i)), aes(x=se_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
    geom_density_ridges(alpha=0.5, scale=1.2, quantile_lines=TRUE, 
                        quantiles=2) +
    com.theme +
    scale_y_discrete(name="", 
                     expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    xlab("SE slope")  +xlim(0,1.05*max(sbin$se_effect))
  
  if(i==0){
    plots_binomial[[i+1]] = (g1+g2+g3+bint+plot_layout(ncol=4))/(g1s+g2s+g3s+bslop+ plot_layout(ncol=4))
  }else{
    plots_binomial[[i+1]] = (g1+g2+g3)/(g1s+g2s+g3s)
  }

}


pdf(paste0("Figures/RE_normal_WITHOUT_singularity.pdf" ),width=18, height = 12)
plots_normal[[1]] ## option 1
dev.off()

pdf(paste0("Figures/RE_normal_WITH_singularity.pdf"),width=18, height = 12)
plots_normal[[2]] ## option 1
dev.off()


pdf(paste0("Figures/RE_binomial_WITHOUT_singularity.pdf"),width=18, height =12)
plots_binomial[[1]]
dev.off()

pdf(paste0("Figures/RE_binomial_WITH_singularity.pdf"),width=18, height = 12)
plots_binomial[[2]]
dev.off()





### glmmTMB plots ------


# because there are less than 1% of singularity in slope models in 2-3
# levesl, I made the figures including all simulations

tmb_intercept <- readRDS("./Results/results_intercept_glmmtmb_normal.Rds")
tmb_slope <- readRDS("./Results/results_slope_glmmtmb_normal.Rds")

names(tmb_intercept) <- rep(2:8)
tmb_intercept <- lapply(tmb_intercept, as.data.frame)
tmb.int <- bind_rows(tmb_intercept, .id="N_levels")
tmb.int <- filter(tmb.int, N_levels %in% c("2","3","4","5"))

names(tmb_slope) <- rep(2:8)
tmb_slope <- lapply(tmb_slope, as.data.frame)
tmb.slope <- bind_rows(tmb_slope, .id="N_levels")
tmb.slope <- filter(tmb.slope, N_levels %in% c("2","3","4","5"))

# singularity table
tmb.si <- tmb.int %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Convergence)*100/n() )
tmb.ss <- tmb.slope %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Convergence)*100/n() )

tmb.sing <- bind_rows(list(intercept=tmb.si, slope=tmb.ss), .id="estimate") %>%
  arrange(estimate, desc(N_levels))

uni <- unit(c(17,17), "mm")
lint <- ggtexttable(tmb.sing %>% filter(estimate=="intercept") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = 13,
                                   padding = uni,
                                   tbody.style = tbody_style(color=viridis_pal()(4)[4:1],
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)))
lslo <- ggtexttable(tmb.sing %>% filter(estimate=="slope") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = 13,
                                   padding = uni,
                                   tbody.style = tbody_style(color=viridis_pal()(4)[4:1],
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)))

p1 <- ggplot(tmb.int, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 13.2, col="black", linetype="dotted") +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("SD random intercept") +
  theme(legend.position = "none",
        text = element_text(size=25),axis.text = element_text(size=20),
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0, 1.05*max(tmb.int$stddev_randeff))+
  labs(tag="a)", size=30)

p2 <-  ggplot(tmb.int, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 66, col="black", linetype="dotted") +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("Estimate intercept") + xlim(min(tmb.int$estimate_intercept),max(tmb.int$estimate_intercept))

p3 <-  ggplot(tmb.int, aes(x=se_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) + 
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("SE intercept") + xlim(0, 1.05*max(tmb.int$se_intercept))

p1s <- ggplot(tmb.slope, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
  xlab("SD random slope") +
  theme(legend.position = "none", 
        text = element_text(size=25),axis.text = element_text(size=20),
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(tag="b)", size=30) + xlim(0, 1.05*max(tmb.slope$stddev_randeff))

p2s <-  ggplot(tmb.slope, aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 3, col="black", linetype="dotted") +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("Estimate slope")  + xlim(min(tmb.slope$estimate_effect), max(tmb.slope$estimate_effect))

p3s <- ggplot(tmb.slope, aes(x=se_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", 
                   expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("SE slope") + xlim(0, 1.05*max(tmb.slope$se_effect))

pdf("Figures/RE_normal_glmmTMB.pdf",width=18, height = 12)
(p1+p2+p3+plot_layout(ncol=3))/(p1s+p2s+p3s+ plot_layout(ncol=3))
dev.off()






### glmmTMB plots for binomial ------


# because there are less than 1% of singularity in slope models in 2-3
# levesl, I made the figures including all simulations

tmb_intercept <- readRDS("./Results/results_intercept_glmmtmb_binomial.Rds")
tmb_slope <- readRDS("./Results/results_slope_glmmtmb_binomial.Rds")

names(tmb_intercept) <- rep(2:8)
tmb_intercept <- lapply(tmb_intercept, as.data.frame)
tmb.int <- bind_rows(tmb_intercept, .id="N_levels")
tmb.int <- filter(tmb.int, N_levels %in% c("2","3","4","5"))

names(tmb_slope) <- rep(2:5)
tmb_slope <- lapply(tmb_slope, as.data.frame)
tmb.slope <- bind_rows(tmb_slope, .id="N_levels")
tmb.slope <- filter(tmb.slope, N_levels %in% c("2","3","4","5"))

# singularity table
tmb.si <- tmb.int %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Singularity)*100/n() )
tmb.ss <- tmb.slope %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Singularity)*100/n() )

tmb.sing <- bind_rows(list(intercept=tmb.si, slope=tmb.ss), .id="estimate") %>%
  arrange(estimate, desc(N_levels))

uni <- unit(c(17,17), "mm")
lint <- ggtexttable(tmb.sing %>% filter(estimate=="intercept") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = 13,
                                   padding = uni,
                                   tbody.style = tbody_style(color=viridis_pal()(4)[4:1],
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)))
lslo <- ggtexttable(tmb.sing %>% filter(estimate=="slope") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = 13,
                                   padding = uni,
                                   tbody.style = tbody_style(color=viridis_pal()(4)[4:1],
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)))

p1 <- ggplot(tmb.int, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
  # xlim(0,40) +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("SD random intercept") +
  theme(legend.position = "none",
        text = element_text(size=25),axis.text = element_text(size=20),
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0, 1.05*max(tmb.int$stddev_randeff)) +
  labs(tag="a)", size=30)

p2 <-  ggplot(tmb.int, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 2., col="black", linetype="dotted") +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(min(tmb.int$estimate_intercept), max(tmb.int$estimate_intercept)) +
  xlab("Estimate intercept") 

p3 <-  ggplot(tmb.int, aes(x=se_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) + 
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0,1.05*max(tmb.int$se_intercept)) +
  xlab("SE intercept") 

p1s <- ggplot(tmb.slope, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 0.24, col="black", linetype="dotted") +
  xlab("SD random slope") +
  theme(legend.position = "none", 
        text = element_text(size=25),axis.text = element_text(size=20),
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0,1.05*max(tmb.slope$stddev_randeff)) +
  labs(tag="b)", size=30)

p2s <-  ggplot(tmb.slope, aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 0.3, col="black", linetype="dotted") +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d()  + xlim(min(tmb.slope$estimate_effect),max(tmb.slope$estimate_effect)) +
  xlab("Estimate slope") 

p3s <- ggplot(tmb.slope, aes(x=se_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", 
                   expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0,1.05*max(tmb.slope$se_effect)) +
  xlab("SE slope") 

pdf("Figures/RE_binomial_glmmTMB.pdf",width=18, height = 12)
(p1+p2+p3+plot_layout(ncol=3))/(p1s+p2s+p3s+plot_layout(ncol=3))
dev.off()

## FIXED EFFECTS -------------

# Results treating RE groups as a fixed effect


fixed_intercept_normal <- readRDS("./Results/results_intercept_fixed_effect.Rds")
fixed_slope_normal <- readRDS("./Results/results_slope_fixed_effect.Rds")

names(fixed_intercept_normal) <- rep(2:8)
fixed_intercept_normal <- lapply(fixed_intercept_normal, as.data.frame)
fixed.int_normal <- bind_rows(fixed_intercept_normal, .id="N_levels")
fixed.int_normal <- filter(fixed.int_normal, N_levels %in% c("2","3","4","5"))


names(fixed_slope_normal) <- rep(2:8)
fixed_slope_normal <- lapply(fixed_slope_normal, as.data.frame)
fixed.slope_normal <- bind_rows(fixed_slope_normal, .id="N_levels")
fixed.slope_normal <- filter(fixed.int_normal, N_levels %in% c("2","3","4","5"))


fixed_intercept_glm <- readRDS("Results/results_glm_fixed_effect.Rds")
fixed_slope_glm <- readRDS("Results/results_glm_fixed_effect.Rds")


names(fixed_intercept_glm) <- rep(2:8)
fixed_intercept_glm <- lapply(fixed_intercept_glm, as.data.frame)
fixed.int_glm <- bind_rows(fixed_intercept_glm, .id="N_levels")
fixed.int_glm <- filter(fixed.int_glm, N_levels %in% c("2","3","4","5"))

names(fixed_slope_glm) <- rep(2:8)
fixed_slope_glm <- lapply(fixed_slope_glm, as.data.frame)
fixed.slope_glm <- bind_rows(fixed_slope_glm, .id="N_levels")
fixed.slope_glm <- filter(fixed.int_glm, N_levels %in% c("2","3","4","5"))


p1 <- ggplot(fixed.int_normal, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels))+
  labs(tag="a) Normal model", size=25)+
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 66, col="black", linetype="dotted") +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("Estimated intercept") +
  theme(legend.position = "none",
        text = element_text(size=20),axis.text = element_text(size=20),
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(min(fixed.int_normal$estimate_intercept), max(fixed.int_normal$estimate_intercept))

p2 <- ggplot(fixed.slope_normal, aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 3, col="black", linetype="dotted") +
  # xlim(0,40) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("Estimated slope") +
  theme(legend.position = "none",
        text = element_text(size=20),axis.text = element_text(size=20),
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(min(fixed.slope_normal$estimate_effect),max(fixed.slope_normal$estimate_effect))


p3 <- ggplot(fixed.int_glm, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels))+
  labs(tag="b) Binomial model", size=25) +
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 2, col="black", linetype="dotted") +
  # xlim(0,40) +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("Estimated intercept") +
  theme(legend.position = "none",
        text = element_text(size=20),axis.text = element_text(size=20),
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(min(fixed.int_glm$estimate_intercept), max(fixed.int_glm$estimate_intercept))

p4 <- ggplot(fixed.slope_glm, aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 0.3, col="black", linetype="dotted") +
  # xlim(0,40) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("Estimated slope") +
  theme(legend.position = "none",
        text = element_text(size=20),axis.text = element_text(size=20),
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()+ xlim(min(fixed.slope_glm$estimate_effect),max(fixed.slope_glm$estimate_effect))




pdf("Figures/Fixed_effects.pdf",width=16, height = 10)
(p1+p2+plot_layout(ncol=2))/(p3+p4+plot_layout(ncol=2))
dev.off()




########### p-values and non-associated effects ############


### glmmTMB plots ------



tmb_intercept <- readRDS("./Results/results_intercept_glmmtmb_normal.Rds")
tmb_slope <- readRDS("./Results/results_slope_glmmtmb_normal.Rds")

names(tmb_intercept) <- rep(2:8)
tmb_intercept <- lapply(tmb_intercept, as.data.frame)
tmb.int <- bind_rows(tmb_intercept, .id="N_levels")
tmb.int <- filter(tmb.int, N_levels %in% c("2","3","4","5"))

names(tmb_slope) <- rep(2:5)
tmb_slope <- lapply(tmb_slope, as.data.frame)
tmb.slope <- bind_rows(tmb_slope, .id="N_levels")
tmb.slope <- filter(tmb.slope, N_levels %in% c("2","3","4","5"))

# singularity table
tmb.si <- tmb.int %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Convergence)*100/n() )
tmb.ss <- tmb.slope %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Convergence)*100/n() )

tmb.sing <- bind_rows(list(intercept=tmb.si, slope=tmb.ss), .id="estimate") %>%
  arrange(estimate, desc(N_levels))

uni <- unit(c(17,17), "mm")
lint <- ggtexttable(tmb.sing %>% filter(estimate=="intercept") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = 13,
                                   padding = uni,
                                   tbody.style = tbody_style(color=viridis_pal()(4)[4:1],
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)))
lslo <- ggtexttable(tmb.sing %>% filter(estimate=="slope") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = 13,
                                   padding = uni,
                                   tbody.style = tbody_style(color=viridis_pal()(4)[4:1],
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)))

p1 <- ggplot(tmb.int, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 13.2, col="black", linetype="dotted") +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("SD random intercept") +
  theme(legend.position = "none",
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0, 1.05*max(tmb.int$stddev_randeff))+
  annotate("text", x = 3., y=5.3, label = "a)",size =10)

p2 <-  ggplot(tmb.int, aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 66, col="black", linetype="dotted") +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("Estimate effect") + xlim(min(tmb.int$estimate_effect),max(tmb.int$estimate_effect))

p3 <-  ggplot(tmb.int, aes(x=se_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) + 
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("Standard error effect") + xlim(0.05, 1.05*max(tmb.int$se_effect))


p1s <- ggplot(tmb.slope, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
  xlab("SD random slope") +
  theme(legend.position = "none", axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  annotate("text", x = 0.1, y=5.3, label = "b)",size =10) + xlim(0, 1.05*max(tmb.slope$stddev_randeff))

p2s <-  ggplot(tmb.slope, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 3, col="black", linetype="dotted") +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("Estimate Intercept")  + xlim(min(tmb.slope$estimate_intercept), max(tmb.slope$estimate_intercept))

p3s <- ggplot(tmb.slope, aes(x=se_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", 
                   expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("Standard error intercept") + xlim(0.02, 1.05*max(tmb.slope$se_intercept))

pdf("Figures/RE_normal_glmmTMB_not_affected.pdf",width=18, height = 12)
(p1+p2+p3+plot_layout(ncol=3))/(p1s+p2s+p3s+ plot_layout(ncol=3))
dev.off()


tmb_intercept <- readRDS("./Results/results_intercept_glmmtmb_binomial.Rds")
tmb_slope <- readRDS("./Results/results_slope_glmmtmb_binomial.Rds")

names(tmb_intercept) <- rep(2:8)
tmb_intercept <- lapply(tmb_intercept, as.data.frame)
tmb.int <- bind_rows(tmb_intercept, .id="N_levels")
tmb.int <- filter(tmb.int, N_levels %in% c("2","3","4","5"))

names(tmb_slope) <- rep(2:)5
tmb_slope <- lapply(tmb_slope, as.data.frame)
tmb.slope <- bind_rows(tmb_slope, .id="N_levels")
tmb.slope <- filter(tmb.slope, N_levels %in% c("2","3","4","5"))

# singularity table
tmb.si <- tmb.int %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Singularity)*100/n() )
tmb.ss <- tmb.slope %>% group_by(N_levels) %>% 
  summarise(`Singularity (%)`= sum(Singularity)*100/n() )

tmb.sing <- bind_rows(list(intercept=tmb.si, slope=tmb.ss), .id="estimate") %>%
  arrange(estimate, desc(N_levels))

uni <- unit(c(17,17), "mm")
lint <- ggtexttable(tmb.sing %>% filter(estimate=="intercept") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = 13,
                                   padding = uni,
                                   tbody.style = tbody_style(color=viridis_pal()(4)[4:1],
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)))
lslo <- ggtexttable(tmb.sing %>% filter(estimate=="slope") %>%
                      select(`Singularity (%)`), 
                    rows = NULL,
                    theme = ttheme(base_style = "blank",
                                   base_size = 13,
                                   padding = uni,
                                   tbody.style = tbody_style(color=viridis_pal()(4)[4:1],
                                                             fill = "white",
                                                             size=13),
                                   rownames.style = rownames_style(linewidth = 50)))

p1 <- ggplot(tmb.int, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 0.4, col="black", linetype="dotted") +
  # xlim(0,40) +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("SD random intercept") +
  theme(legend.position = "none",
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0, 1.05*max(tmb.int$stddev_randeff)) +
  annotate("text", x = 0.1, y=5.3, label = "a)",size =10)

p2 <-  ggplot(tmb.int, aes(x=estimate_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 2., col="black", linetype="dotted") +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(min(tmb.int$estimate_effect), max(tmb.int$estimate_effect)) +
  xlab("Estimate Effect") 

p3 <-  ggplot(tmb.int, aes(x=se_effect, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) + 
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0.1,1.05*max(tmb.int$se_effect)) +
  xlab("Standard error effect") 

p1s <- ggplot(tmb.slope, aes(x=stddev_randeff, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 0.24, col="black", linetype="dotted") +
  xlab("SD random slope") +
  theme(legend.position = "none", axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0,1.05*max(tmb.slope$stddev_randeff)) +
  annotate("text", x = 0.1, y=4.3, label = "b)",size =10)

p2s <-  ggplot(tmb.slope, aes(x=estimate_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  geom_vline(xintercept = 0.3, col="black", linetype="dotted") +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d()  + xlim(min(tmb.slope$estimate_intercept),max(tmb.slope$estimate_intercept)) +
  xlab("Estimate intercept") 

p3s <- ggplot(tmb.slope, aes(x=se_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges( alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                       quantiles=2) +
  scale_y_discrete(name="", 
                   expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  com.theme +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0.05,1.05*max(tmb.slope$se_intercept)) +
  xlab("Standard error intercept") 

pdf("Figures/RE_binomial_glmmTMB_non_affected.pdf",width=18, height = 12)
(p1+p2+p3+plot_layout(ncol=3))/(p1s+p2s+p3s+plot_layout(ncol=3))
dev.off()

######### p-value difference of lme4 and glmmtmb ##########

### intercept normal ### 


tmb_intercept <- readRDS("./Results/results_intercept_glmmtmb_normal.Rds")

names(tmb_intercept) <- rep(2:8)
tmb_intercept <- lapply(tmb_intercept, as.data.frame)
tmb.int <- bind_rows(tmb_intercept, .id="N_levels")
tmb.int <- filter(tmb.int, N_levels %in% c("2","3","4","5"))


results_intercept <- readRDS("./Results/results_intercept_lme4.Rds")

names(results_intercept) <- rep(2:8)
results_intercept <- lapply(results_intercept, as.data.frame)
res.int <- bind_rows(results_intercept, .id="N_levels")
res.int <- filter(res.int, N_levels %in% c("2","3","4","5"))

p1 <- ggplot(res.int %>% filter(Singularity %in% c(0,i)), aes(x=p_value_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 13.2, col="black", linetype="dotted") +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("P-value intercept") +
  theme(legend.position = "none",
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlim(0, 1.05*max(res.int$p_value_intercept))+
  annotate("text", x = 0.15, y=5.3, label = "lme4",size =6)

p2 <- ggplot(tmb.int, aes(x=p_value_intercept, y = N_levels, fill=N_levels,col=N_levels)) + 
  geom_density_ridges(alpha=0.5, scale=1.5, quantile_lines=TRUE, 
                      quantiles=2) +
  geom_vline(xintercept = 13.2, col="black", linetype="dotted") +
  scale_y_discrete(name="Number of Levels", expand = expansion(mult=c(0.01,0.01), c(0, 1.5))) +
  xlab("P-value intercept") +
  theme(legend.position = "none",
        axis.line.y = element_blank()) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() + xlim(0, 1.05*max(tmb.int$p_value_intercept))+
  annotate("text", x = 0.02, y=5.3, label = "glmmTMB",size =6)

pdf("Figures/RE_normal_pvalues.pdf",width=18, height = 8)
(p1+p2+plot_layout(ncol=2))
dev.off()

