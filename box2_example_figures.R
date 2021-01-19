
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

# lme4 - ML
fit_lmm1 <- lmer(y ~ x  + (1 | group), data=data, REML = TRUE) 
fit_lmm2 <- lmer(y ~ x  + (x | group), data=data, REML = TRUE) 
fit_lm <- lm(y~x*group, data=data)


#predict RE intercetp + slope
predLM <- ggpredict(fit_lm, terms=c('x', "group"))
predF1 <- ggpredict(fit_lmm1)
predR1 <- ggpredict(fit_lmm1,terms=c("x", "group"), type="re")
predF2 <- ggpredict(fit_lmm2)
predR2 <- ggpredict(fit_lmm2,terms=c("x", "group"), type="re")
#plot(predLM)



box2 <- ggplot(data, aes(x=x,y=y, col=group)) + 
  geom_point(alpha=0.3, size=3, show.legend = F) +
  geom_line(data=as.data.frame(predLM), aes(x=x,y=predicted, col=group), size=1.5) +
  scale_color_manual(name="Mountain", values=cores) +
  xlab("") +
  ylab("Height (cm)") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=c(-1,0,1)) +
  scale_y_continuous(breaks=c(9,10,11)) +
  ggtitle("Fixed effects only")+
ggplot(data, aes(x=x,y=y, col=group)) + 
  geom_point(alpha=0.3, size=3, show.legend = F) +
  geom_line(data=as.data.frame(predR1), aes(x=x,y=predicted, col=group), size=1.5) +
  geom_line(data=as.data.frame(predF1$x), aes(x=x,y=predicted), col="red", size=3,)+
  scale_color_manual(name="Mountain", values=cores) +
  xlab("Temperature") +
  ylab("") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(breaks=c(-1,0,1)) +
  ggtitle("Random intercept")+
ggplot(data, aes(x=x,y=y, col=group)) + 
  geom_point(alpha=0.3, size=3, show.legend = F) +
  geom_line(data=as.data.frame(predR2), aes(x=x,y=predicted, col=group), size=1.5) +
  geom_line(data=as.data.frame(predF2$x), aes(x=x,y=predicted), col="red", size=3)+
  scale_color_manual(name="Mountain", values=cores) +
  xlab("") +
  ylab("") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_x_continuous(breaks=c(-1,0,1)) +
  ggtitle("Random intercept and slope")

jpeg("figures_googledocs/box2_figure.jpeg", height = 250, width = 800, quality=100)
box2
dev.off()
 