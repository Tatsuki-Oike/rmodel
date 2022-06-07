# 1 ライブラリ
library(dplyr)
library(ggplot2)
library(rstan)
library(bayesplot)

set.seed(1)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 2 データ
日付 <- seq(as.POSIXct("2021/05/01"), as.POSIXct("2021/08/01"), "days")
売り上げ <- c()
mu <- c()
mu[1] <- rnorm(1, 200, 10) %>% round(1)
T <- length(日付)
for(t in 2:T){
  mu[t] <- rnorm(1, mu[t-1], 5)
}
for(t in 1:T){
  売り上げ[t] <- rnorm(1, mu[t], 3) 
}
data <- data.frame(日付, 売り上げ)
data %>% head()

plot <- ggplot() +
  geom_point(aes(x=data$日付, y=data$売り上げ))+
  geom_line(aes(x=data$日付, y=data$売り上げ))+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 30))+
  labs(x="日付",y="売り上げ",title="売り上げの推移") +
  scale_x_datetime(date_labels = "%m/%d")
plot

# 3 stanの利用
data_list <- list(
  T = nrow(data), y = data$売り上げ,
  
  T_pred = 10
)

mcmc_result <- stan(
  file="17ローカルレベルモデル.stan",
  data=data_list,
  seed=1,
  iter = 2000, warmup = 200, chains = 3, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result, pars=c("sigma_w", "sigma_v"), probs = c(0.025, 0.5, 0.975))

## 4.2 収束の確認
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
mcmc_combo(mcmc_sample, pars=c("sigma_w","sigma_v"))

## 4.3 μの確認
mcmc_sample <- rstan::extract(mcmc_result)
func <- function(x){
  return (quantile(x, c(0.025, 0.5, 0.975)))
}
mu_pred <- apply(mcmc_sample[["mu_pred"]], 2, func)
日付_pred <- seq(as.POSIXct("2021/05/01"), as.POSIXct("2021/08/11"), "days")

plot + 
  labs(title="μの推定結果") +
  geom_line(aes(x=日付_pred, y=mu_pred[2,]), col="blue") + 
  geom_ribbon(aes(x=日付_pred, ymin=mu_pred[1,], ymax=mu_pred[3,]), alpha=0.5, fill="gray", col="blue")

## 4.4 予測分布
y_pred <- apply(mcmc_sample[["y_pred"]], 2, func)

plot +
  labs(title="予測分布") +
  geom_line(aes(x=日付_pred, y=y_pred[2,]), col="blue") + 
  geom_ribbon(aes(x=日付_pred, ymin=y_pred[1,], ymax=y_pred[3,]), alpha=0.5, fill="gray", col="blue")
