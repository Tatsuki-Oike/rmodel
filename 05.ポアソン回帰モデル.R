# 1 ライブラリ
library(dplyr)
library(ggplot2)
library(rstan)
library(bayesplot)

set.seed(1)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 2 データ
気温 <- rnorm(100, 20,5) %>% round(1) 
休日 <- rbinom(100, 1, 2/7)
lambda <- exp(-2+0.2*気温+0.5*休日)
売り上げ個数 <- rpois(100, lambda)
data　<- data.frame(気温, 休日, 売り上げ個数)
data %>% head()

plot <- ggplot() +
  geom_point(aes(x=data$気温, y=data$売り上げ個数, color=factor(data$休日))) + 
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text=element_text(size=25))+
  labs(x="気温", y="売り上げ個数", title="データ") +
  scale_color_manual("休日",values=c("red","blue"))
plot

# 3 stanの使用
x_hat <- seq(min(data$気温), max(data$気温))
data_list <- list(
  N = nrow(data), y = data$売り上げ個数,
  x1 = data$気温, x2 = data$休日,
  
  N_hat = length(x_hat), x_hat = x_hat
)

mcmc_result <- stan(
  file="5ポアソン回帰モデル.stan",
  data=data_list,
  seed=10,
  iter = 2000, warmup = 200, chains = 3, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result, probs = c(0.025, 0.5, 0.975), pars="b")

## 4.2 収束の確認
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
mcmc_combo(mcmc_sample, pars=c("b[1]","b[2]","b[3]"))

## 4.3 λの確認
mcmc_sample <- rstan::extract(mcmc_result)
func <- function(x){
  return (quantile(x, c(0.025, 0.5, 0.975)))
}
lambda_hat1 <- apply(mcmc_sample[["lambda_hat1"]], 2, func)
lambda_hat0 <- apply(mcmc_sample[["lambda_hat0"]], 2, func)

plot_lambda<- plot + 
  labs(title="λの推定結果") +
  geom_line(aes(x=x_hat, y=lambda_hat1[2,]), col="blue") + 
  geom_line(aes(x=x_hat, y=lambda_hat0[2,]), col="red") +
  geom_ribbon(aes(x=x_hat, ymin=lambda_hat1[1,],ymax=lambda_hat1[3,]), alpha=0.5, fill="gray", col="blue")+
  geom_ribbon(aes(x=x_hat, ymin=lambda_hat0[1,],ymax=lambda_hat0[3,]), alpha=0.5, fill="gray", col="red")
plot_lambda

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
