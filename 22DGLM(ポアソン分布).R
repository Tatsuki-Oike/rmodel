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
## 2.1 データ作成
日付 <- seq(as.POSIXct("2021/05/01"), as.POSIXct("2021/08/01"), "days")
売り上げ個数 <- c()
mu <- c()
gamma <- c()
mu[1] <- 0
gamma[1:6] <- c(-7,-5,2,3,6,9)/10 
T <- length(日付)
for(t in 2:T){
  mu[t] <- rnorm(1, mu[t-1], 0.3)
}
for(t in 7:T){
  gamma[t] <- rnorm(1, -sum(gamma[(t-6):(t-1)]), 0.1)
}
lambda <- exp(mu + gamma)
for(t in 1:T){
  売り上げ個数[t] <- rpois(1, lambda[t])
}
data <- data.frame(日付, 売り上げ個数)
data %>% head()

## 2.2 データの可視化
plot <- data %>% 
  ggplot(aes(x=日付)) +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 10))+
  scale_x_datetime(date_labels = "%m/%d")

plot + 
  geom_point(aes(y=売り上げ個数))+
  geom_line(aes(y=売り上げ個数)) +
  labs(x="日付",y="売り上げ個数",title="売り上げ個数の推移")

## 2.3 パラメータの可視化
plot_lambda_sim <- plot + 
  geom_point(aes(y=lambda))+
  geom_line(aes(y=lambda)) +
  labs(x="日付",y="λ",title="λの推移")

plot_mu_sim <- plot + 
  geom_point(aes(y=mu))+
  geom_line(aes(y=mu)) +
  labs(x="日付",y="μ",title="μの推移")

plot_gamma_sim <- plot + 
  geom_point(aes(y=gamma))+
  geom_line(aes(y=gamma)) +
  labs(x="日付",y="γ",title="γの推移")

grid.arrange(plot_lambda_sim, plot_mu_sim, plot_gamma_sim)

# 3 stanの利用
data_list <- list(
  T = nrow(data),
  y = data$売り上げ
)

mcmc_result <- stan(
  file="20DGLM(ポアソン分布).stan",
  data=data_list,
  seed=1,
  iter = 2000, warmup = 200, chains = 3, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result, pars=c("sigma_w", "sigma_s"), probs = c(0.025, 0.5, 0.975))

## 4.2 収束の確認
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
mcmc_combo(mcmc_sample, pars=c("sigma_w","sigma_s"))

## 4.3 パラメータの確認
mcmc_sample <- rstan::extract(mcmc_result)
func <- function(x){
  return (quantile(x, c(0.025, 0.5, 0.975)))
}

lambda <- apply(mcmc_sample[["lambda"]], 2, func)
plot_lambda <- plot + 
  geom_line(aes(y=lambda[2,]), col="blue") + 
  geom_ribbon(aes(ymin=lambda[1,],ymax=lambda[3,]), alpha=0.5, fill="gray", col="blue") +
  labs(x="日付",y="λ",title="λの推移")

mu <- apply(mcmc_sample[["mu"]], 2, func)
plot_mu <- plot + 
  geom_line(aes(y=mu[2,]), col="blue") + 
  geom_ribbon(aes(ymin=mu[1,],ymax=mu[3,]), alpha=0.5, fill="gray", col="blue")+
  labs(x="日付",y="μ",title="μの推移")

gamma <- apply(mcmc_sample[["gamma"]], 2, func)
plot_gamma <- plot + 
  geom_line(aes(y=gamma[2,]), col="blue") + 
  geom_ribbon(aes(ymin=gamma[1,],ymax=gamma[3,]), alpha=0.5, fill="gray", col="blue")+
  labs(x="日付",y="μ",title="μの推移")

gridExtra::grid.arrange(plot_lambda, plot_mu, plot_gamma)

## 4.4 予測分布
y_pred <- apply(mcmc_sample[["y_pred"]], 2, func)

plot + 
  geom_point(aes(y=売り上げ個数))+
  geom_line(aes(y=y_pred[2,]), col="blue") + 
  geom_ribbon(aes(ymin=y_pred[1,],ymax=y_pred[3,]), alpha=0.5, fill="gray", col="blue") +
  labs(x="日付",y="売り上げ個数",title="売り上げ個数の推移")
