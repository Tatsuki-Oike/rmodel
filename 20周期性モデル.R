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
売り上げ <- c()
mu <- c()
gamma <- c()
mu[1] <- rnorm(1, 200, 10) %>% round(1)
gamma[1:6] <- c(-70,-50,20,30,60,90)/2
T <- length(日付)
for(t in 2:T){
  mu[t] <- rnorm(1, mu[t-1], 5)
}
for(t in 7:T){
  gamma[t] <- rnorm(1, -sum(gamma[(t-6):(t-1)]), 1)
}
alpha = mu + gamma
for(t in 1:T){
  売り上げ[t] <- rnorm(1, alpha[t], 3)
}
data <- data.frame(日付, 売り上げ)
data %>% head()

## 2.2 データの可視化
plot <- data %>% 
  ggplot(aes(x=日付)) +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 10)) +
  scale_x_datetime(date_labels = "%m/%d")

plot + 
  geom_point(aes(y=売り上げ))+
  geom_line(aes(y=売り上げ)) +
  labs(x="日付", y="売り上げ",title="売り上げの推移")

## 2.3 パラメータの可視化
plot_alpha_sim <- plot + 
  geom_point(aes(y=alpha))+
  geom_line(aes(y=alpha)) +
  labs(x="日付", y="α",title="αの推移")

plot_mu_sim <- plot + 
  geom_point(aes(y=mu))+
  geom_line(aes(y=mu)) +
  labs(x="日付", y="μ",title="μの推移")

plot_gamma_sim <- plot + 
  geom_point(aes(y=gamma))+
  geom_line(aes(y=gamma)) +
  labs(x="日付", y="γ",title="γの推移")

grid.arrange(plot_alpha_sim, plot_mu_sim, plot_gamma_sim)

# 3 stanの利用
data_list <- list(
  T = nrow(data),
  y = data$売り上げ
)

mcmc_result <- stan(
  file="20周期性モデル.stan",
  data=data_list,
  seed=1,
  iter = 2000, warmup = 200, chains = 3, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result, pars=c("sigma_w", "sigma_v", "sigma_s"), probs = c(0.025, 0.5, 0.975))

## 4.2 収束の確認
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
mcmc_combo(mcmc_sample, pars=c("sigma_w","sigma_v","sigma_s"))

## 4.3 muの確認
mcmc_sample <- rstan::extract(mcmc_result)
func <- function(x){
  return (quantile(x, c(0.025, 0.5, 0.975)))
}
alpha <- apply(mcmc_sample[["alpha"]], 2, func)
plot_alpha <- plot + 
  geom_point(aes(y=売り上げ))+
  geom_line(aes(y=alpha[2,]), col="blue") + 
  geom_ribbon(aes(ymin=alpha[1,],ymax=alpha[3,]), alpha=0.5, fill="gray", col="blue")

mu <- apply(mcmc_sample[["mu"]], 2, func)
plot_mu <- plot + 
  geom_line(aes(y=mu[2,]), col="blue") + 
  geom_ribbon(aes(ymin=mu[1,],ymax=mu[3,]), alpha=0.5, fill="gray", col="blue")+
  labs(x="日付",y="μ",title="μの推移")

gamma <- apply(mcmc_sample[["gamma"]], 2, func)
plot_gamma <- plot + 
  geom_line(aes(y=gamma[2,]), col="blue") + 
  geom_ribbon(aes(ymin=gamma[1,],ymax=gamma[3,]), alpha=0.5, fill="gray", col="blue")+
  labs(x="日付",y="γ",title="γの推移")

gridExtra::grid.arrange(plot_alpha, plot_mu, plot_gamma)

## 4.4 予測分布
y_pred <- apply(mcmc_sample[["y_pred"]], 2, func)

plot + 
  geom_point(aes(y=売り上げ))+
  geom_line(aes(y=y_pred[2,]), col="blue") + 
  geom_ribbon(aes(ymin=y_pred[1,],ymax=y_pred[3,]), alpha=0.5, fill="gray", col="blue") +
  labs(x="日付",y="売り上げの推移",title="予測分布")
