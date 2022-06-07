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
店舗 <- runif(100, 1, 4) %>% round()
店舗ランダム効果 <- rnorm(4, 0, 1)
data　<- data.frame(気温, 休日, 店舗) %>% 
  mutate(r=ifelse(店舗==1, 店舗ランダム効果[1], ifelse(店舗==2, 店舗ランダム効果[2],
                                ifelse(店舗==3, 店舗ランダム効果[3],店舗ランダム効果[4] ))))%>%
  mutate(lambda=exp(-2+0.2*気温+0.5*休日+r))
data$売り上げ個数 <- rpois(100, data$lambda)
data %>% select(気温, 休日, 店舗, 売り上げ個数) %>% head()

plot <- ggplot() +
  geom_point(data=data,aes(x=気温, y=売り上げ個数, color=factor(休日))) + 
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text=element_text(size=25))+
  labs(x="気温", y="売り上げ個数", title="データ") +
  scale_color_manual("資格",values=c("red","blue")) + 
  facet_wrap(.~ 店舗)
plot

# 3 stanの使用
data_list <- list(
  N = nrow(data), K = 4, y = data$売り上げ個数,
  x1 = data$気温, x2 = data$休日, x3 = data$店舗
)

mcmc_result <- stan(
  file="11ランダム切片モデル2.stan",
  data=data_list,
  seed=1,
  iter = 2000, warmup = 200, chains = 4, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result, probs = c(0.025, 0.5, 0.975), pars=c("b","sigma", "r"))

## 4.2 収束の確認
mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
mcmc_combo(mcmc_sample, pars=c("b[1]","b[2]","b[3]","sigma"))
