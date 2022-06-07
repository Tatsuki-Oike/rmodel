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
r <- rnorm(100, 0, 0.6)
lambda <- exp(-2+0.2*気温+0.5*休日+r)
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
  file="9ランダム切片モデル1.stan",
  data=data_list,
  seed=10,
  iter = 2000, warmup = 200, chains = 2, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result, probs = c(0.025, 0.5, 0.975), pars=c("b","sigma"))

## 4.2 収束の確認
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
mcmc_combo(mcmc_sample, pars=c("b[1]","b[2]","b[3]","sigma"))

## 4.3 λの確認
mcmc_sample <- rstan::extract(mcmc_result)
func <- function(x){
  return (quantile(x, c(0.025, 0.5, 0.975)))
}
lambda_hat1 <- apply(mcmc_sample[["lambda_hat1"]], 2, func)
lambda_hat0 <- apply(mcmc_sample[["lambda_hat0"]], 2, func)
r <- quantile(mcmc_sample[["r"]], c(0.025, 0.5, 0.975))

plot_lambda<- plot + 
  labs(title="λの推定結果") +
  geom_line(aes(x=x_hat, y=lambda_hat1[2,]*exp(r[2])), col="blue") + 
  geom_line(aes(x=x_hat, y=lambda_hat0[2,]*exp(r[2])), col="red") +
  geom_ribbon(aes(x=x_hat, ymin=lambda_hat1[1,]*exp(r[1]),ymax=lambda_hat1[3,]*exp(r[3])), alpha=0.5, fill="gray", col="blue")+
  geom_ribbon(aes(x=x_hat, ymin=lambda_hat0[1,]*exp(r[1]),ymax=lambda_hat0[3,]*exp(r[3])), alpha=0.5, fill="gray", col="red")
plot_lambda
