library(tidyverse); library(cowplot)
library(patchwork)

set.seed(1)

### NORMAL -----


### RANDOM INTERCEPT ------

n <- 100
n_groups <- 4
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(66, 3)
sd_randeff = 0.2*beta[1]   ### why?

g <- sort(sample.int(n_groups, size = n, replace = T))
group <-  as.factor(g)
randintercep <- rnorm(n_groups, mean = beta[1], sd = sd_randeff)

mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta[2])) 


sigma <- 0.5
y <- rnorm(n, mu, sd = sigma) 


data <- data.frame(y=y,x=x,groups=group)

nri <- ggplot(data, aes(x=x,y=y)) + geom_point(aes(col=groups)) +
  geom_smooth(method = "lm", aes(col=groups), se=F, linetype="dashed") +
  geom_smooth(method = "lm", size=2, col="black") +
  ylab("Height (mm)") + xlab("Temperature")
nri



### RANDOM SLOPE ------
n <- 100
n_groups <- 4

x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(66, 3)

sd_randslope = 0.2*beta[2]

g <- sort(sample.int(n_groups, size = n, replace = T))
group <-  as.factor(g)
randslope <- rnorm(n_groups, mean = beta[2], sd = sd_randslope)
# maybe do some correlation between random slope and fixed intercept 
#MASS::mvrnorm()
mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta[1],randslope[g[i]])) 


sigma <- 0.5
y <- rnorm(n, mu, sd = sigma) 

data <- data.frame(y=y,x=x,groups=group)

nrs <- ggplot(data, aes(x=x,y=y)) + geom_point(aes(col=groups)) +
  geom_smooth(method = "lm", aes(col=groups), se=F, linetype="dashed") +
  geom_smooth(method = "lm", size=2, col="black") +
  ylab("Height (mm)") + xlab("Temperature")


### PLOT ----

jpeg("Figures/ex_data_normal.jpeg", width=800)
nri + ggtitle("Random Intercept") + nrs + ggtitle("Random Slope")
dev.off()



### BINOMIAL -----


### RANDOM INTERCEPT -----

n <- 1000L
n_groups <- 4

inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}

## The data generating process ### 
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(0.1,1)     ## suggestion c(0.1, 1)
sd_randeff = 0.2*beta[1]

g <- sort(sample.int(n_groups, size = n, replace = T))
group <-  as.factor(g)
randintercep <- rnorm(n_groups, mean = beta[1], sd = sd_randeff)

mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta[2])) 

## The binomial model
y <- rbinom(n, 1, inv.logit(mu))

data <- data.frame(y=y,x=x,groups=group)

# grouping data for ploting percentages of repr success
x2 <- round(seq(-1,1,length.out = 10),2)
data$x2 <- 1 
for(i in 9:1){
  data$x2[data$x<=x2[i]] <- x2[i]
}
table(data$x2)
data2 <- data %>% group_by(groups, x2) %>% summarise(one=sum(y), tot=n()) %>%
  mutate(prop = one/tot)


bri <- ggplot(data, aes(x=x,y=y)) + 
  geom_point(data=data2, aes(col=groups, x=x2,y=prop)) +
  geom_smooth(method = "glm", method.args= list(family="binomial"),
              aes(col=groups), se=F, linetype="dashed") +
  geom_smooth(method = "glm",  method.args= list(family="binomial"),
              size=2, col="black") +
  ylab("Reproductive success %") + xlab("Temperature")
bri




### RANDOM SLOPE -----


set.seed(1)
n <- 1000L

inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}
## The data generating process ### 
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(0.1,1)
sd_randslope = 0.2*beta[2]

g <- sort(sample.int(n_groups, size = n, replace = T))
group <-  as.factor(g)
randslope <- rnorm(n_groups, mean = beta[2], sd = sd_randslope)

mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta[1],randslope[g[i]]))  

## The binomial model
y <- rbinom(n, 1, inv.logit(mu))

data <- data.frame(y=y,x=x,groups=group)

# grouping data for ploting percentages of repr success
x2 <- round(seq(-1,1,length.out = 10),2)
data$x2 <- 1 
for(i in 9:1){
  data$x2[data$x<=x2[i]] <- x2[i]
}
table(data$x2)
data2 <- data %>% group_by(groups, x2) %>% summarise(one=sum(y), tot=n()) %>%
  mutate(prop = one/tot)


brs <- ggplot(data, aes(x=x,y=y)) + 
  geom_point(data=data2, aes(col=groups, x=x2,y=prop)) +
  geom_smooth(method = "glm", method.args= list(family="binomial"),
              aes(col=groups), se=F, linetype="dashed") +
  geom_smooth(method = "glm",  method.args= list(family="binomial"),
              size=2, col="black") +
  ylab("Reproductive success %") + xlab("Temperature")

brs

### PLOT ----
jpeg("Figures/ex_data_binomial_2.jpeg", width=800)
bri + ggtitle("random intercept") + brs + ggtitle("random slope")
dev.off()

