# 1 ライブラリ
library(dplyr)
library(ggplot2)
library(rstan)
library(bayesplot)
library(gridExtra)

set.seed(1)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 2 データ
## 2.1 データの作成
日付 <- seq(as.POSIXct("2021/05/01"), as.POSIXct("2021/08/01"), "days")
目標 <- c()
mu <- c()
mu[1] <- -2
T <- length(日付)
for(t in 2:T){
  mu[t] <- rnorm(1, mu[t-1], 0.2)
}
p <- 1/(1+exp(-mu))
for(t in 1:T){
  目標[t] <- rbinom(1, 1, p[t])
}
data <- data.frame(日付, 目標)
data %>% head()

## 2.2 データの可視化
plot <- data %>% 
  ggplot(aes(x=日付)) +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 10)) +
  scale_x_datetime(date_labels = "%m/%d")

plot + 
  geom_point(aes(y=目標))+
  labs(x="日付",y="目標",title="売り上げの推移")

## 2.3 パラメータの可視化
plot_mu_sim <- plot + 
  geom_point(aes(y=mu))+
  labs(x="日付",y="μ",title="μの推移")

plot_p_sim <- plot + 
  geom_point(aes(y=p))+
  labs(x="日付",y="p",title="pの推移")

grid.arrange(plot_mu_sim, plot_p_sim)

# 3 stanの利用
data_list <- list(
  T = nrow(data),
  y = data$目標
)

mcmc_result <- stan(
  file="21DGLM(ベルヌーイ分布).stan",
  data=data_list,
  seed=1,
  iter = 2000, warmup = 200, chains = 3, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result, pars=c("sigma_w"), probs = c(0.025, 0.5, 0.975))

## 4.2 収束の確認
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
mcmc_combo(mcmc_sample, pars=c("sigma_w"))

## 4.3 パラメータの確認
mcmc_sample <- rstan::extract(mcmc_result)
func <- function(x){
  return (quantile(x, c(0.025, 0.5, 0.975)))
}

mu <- apply(mcmc_sample[["mu"]], 2, func)
plot_mu <- plot + 
  geom_line(aes(y=mu[2,]), col="blue") + 
  geom_ribbon(aes(ymin=mu[1,],ymax=mu[3,]), alpha=0.5, fill="gray", col="blue")+
  labs(x="日付",y="μ",title="μの推移")

p <- apply(mcmc_sample[["p"]], 2, func)
plot_p <- plot + 
  geom_point(aes(y=目標))+
  geom_line(aes(y=p[2,]), col="blue") + 
  geom_ribbon(aes(ymin=p[1,],ymax=p[3,]), alpha=0.5, fill="gray", col="blue")+
  labs(x="日付",y="p",title="pの推移")

gridExtra::grid.arrange(plot_mu, plot_p)
