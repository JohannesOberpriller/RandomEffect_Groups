
library(lme4)
library(tidyverse)
library(cowplot); library(patchwork)
theme_set(theme_cowplot())
library(ggeffects)

cores <- c("#6f279c", "#456fc1", "#70af50")

set.seed(2)
n_each <- 50
n_groups <- 3
n = (n_groups)*n_each

x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
sd_randeff = 0.2 # original 0.1 

beta = 10.0
beta0 = 0.4
g <- rep(1:n_groups, n_each)
group <-  as.factor(g)
randintercep <- rnorm(n_groups, mean = beta, sd = sd_randeff)
randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)

mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],randslope[g[i]])) 
sigma <- 0.4 # original 0.5
y <- rnorm(n, mu, sd = sigma) 



# EXAMPLE DATA
data <- data.frame(y,x,group)
data$group <- fct_recode(as.character(data$group), A="1", B="2", C="3")

# lm
fit_lm1 <- lm(y~x+group, data=data)
fit_lm2 <- lm(y~x*group, data=data)

# lme4 - ML
fit_lmm1 <- lmer(y ~ x  + (1 | group), data=data, REML = TRUE) 
fit_lmm2 <- lmer(y ~ x  + (x | group), data=data, REML = TRUE) 

#predict RE intercetp + slope
plm1 <- ggpredict(fit_lm1, terms=c('x', "group"))
plm2 <- ggpredict(fit_lm2, terms=c('x', "group"))
plmm1f <- ggpredict(fit_lmm1)
plmm1r <- ggpredict(fit_lmm1,terms=c("x", "group"), type="re")
plmm2f <- ggpredict(fit_lmm2)
plmm2r <- ggpredict(fit_lmm2,terms=c("x", "group"), type="re")
#plot(predLM)


box2<-ggplot(data, aes(x=x,y=y, col=group)) + 
  geom_point(alpha=0.3, size=3, show.legend = F) +
  geom_line(data=as.data.frame(plm1), aes(x=x,y=predicted, col=group), size=1.5) +
  scale_color_manual(name="Mountain", values=cores) +
  xlab("") +
  ylab("Height (cm)") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_x_continuous(breaks=c(-1,0,1)) +
  scale_y_continuous(breaks=c(9,10,11)) +
  ggtitle("Fixed effects models") +

ggplot(data, aes(x=x,y=y, col=group)) + 
  geom_point(alpha=0.3, size=3, show.legend = F) +
  geom_line(data=as.data.frame(plm2), aes(x=x,y=predicted, col=group), size=1.5) +
  scale_color_manual(name="Mountain", values=cores) +
  xlab("Temperature") +
  ylab("Height (cm)") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=c(-1,0,1)) +
  scale_y_continuous(breaks=c(9,10,11)) +
  
  ggplot(data, aes(x=x,y=y, col=group)) + 
  geom_point(alpha=0.3, size=3, show.legend = F) +
  geom_line(data=as.data.frame(plmm1r), aes(x=x,y=predicted, col=group), size=1.5) +
  geom_line(data=as.data.frame(plmm1f$x), aes(x=x,y=predicted), col="red", size=3,)+
  scale_color_manual(name="Mountain", values=cores) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(breaks=c(-1,0,1)) +
  ggtitle("Mixed effects models") +
  
ggplot(data, aes(x=x,y=y, col=group)) + 
  geom_point(alpha=0.3, size=3, show.legend = F) +
  geom_line(data=as.data.frame(plmm2r), aes(x=x,y=predicted, col=group), size=1.5) +
  geom_line(data=as.data.frame(plmm2f$x), aes(x=x,y=predicted), col="red", size=3)+
  scale_color_manual(name="Mountain", values=cores) +
  xlab("Temperature") +
  ylab("") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_x_continuous(breaks=c(-1,0,1)) +

plot_layout(guides = 'collect',byrow=FALSE) + 
  plot_annotation(tag_levels = 'a')





jpeg("figures_googledocs/box2_figure.jpeg", height = 400, width = 500, quality=100)
box2
dev.off()
 