# 1 ライブラリ
library(dplyr)
library(ggplot2)
library(rstan)
library(bayesplot)

set.seed(1)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 2 データ
資格 <- rbinom(100, 1, 0.3)
年齢 <- rnorm(100, 50, 20) %>% round()
年収 <- rnorm(100, 300 + 10*年齢 + 200*資格, 100) %>% round()
data <- data.frame(資格, 年齢, 年収) %>% filter(年齢>18, 年齢<65)
data %>% head()

plot <- ggplot() +
  geom_point(aes(x=data$年齢, y=data$年収, color=factor(data$資格))) + 
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text=element_text(size=25))+
  labs(x="年齢", y="年収(万)", title="モデル") +
  scale_color_manual("資格",values=c("red","blue"))
plot

# 3 stanの利用
x_hat <- seq(min(data$年齢), max(data$年齢))
data_list <- list(
  N = nrow(data), y = data$年収,
  x1 = data$年齢, x2 = data$資格,
  
  N_hat = length(x_hat), x_hat = x_hat
)

mcmc_result <- stan(
  file="3正規線形モデル.stan",
  data=data_list,
  seed=1,
  iter = 1000, warmup = 200, chains = 4, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result, probs = c(0.025, 0.5, 0.975), pars=c("b","sigma"))

## 4.2 収束の確認
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
mcmc_combo(mcmc_sample, pars=c("b[1]","b[2]","b[3]","sigma"))

## 4.3 μの確認
mcmc_sample <- rstan::extract(mcmc_result)
func <- function(x){
  return (quantile(x, c(0.025, 0.5, 0.975)))
}
mu_hat1 <- apply(mcmc_sample[["mu_hat1"]], 2, func)
mu_hat0 <- apply(mcmc_sample[["mu_hat0"]], 2, func)

plot_mu <- plot + 
  labs(title="μの推定結果") +
  geom_line(aes(x=x_hat, y=mu_hat1[2,]), col="blue") + 
  geom_line(aes(x=x_hat, y=mu_hat0[2,]), col="red") +
  geom_ribbon(aes(x=x_hat, ymin=mu_hat1[1,],ymax=mu_hat1[3,]), alpha=0.5, fill="gray", col="blue")+
  geom_ribbon(aes(x=x_hat, ymin=mu_hat0[1,],ymax=mu_hat0[3,]), alpha=0.5, fill="gray", col="red")
plot_mu

## 4.4 予測分布
y_hat1 <- apply(mcmc_sample[["y_hat1"]], 2, func)
y_hat0 <- apply(mcmc_sample[["y_hat0"]], 2, func)

plot_y <- plot +
  labs(title="予測分布") +
  geom_line(aes(x=x_hat, y=y_hat1[2,]), col="blue") + 
  geom_line(aes(x=x_hat, y=y_hat0[2,]), col="red") +
  geom_ribbon(aes(x=x_hat, ymin=y_hat1[1,],ymax=y_hat1[3,]), alpha=0.5, fill="gray", col="blue")+
  geom_ribbon(aes(x=x_hat, ymin=y_hat0[1,],ymax=y_hat0[3,]), alpha=0.5, fill="gray", col="red")
plot_y
