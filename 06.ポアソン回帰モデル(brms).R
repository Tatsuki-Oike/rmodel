# 1 ライブラリ
library(dplyr)
library(ggplot2)
library(rstan)
library(brms)
library(patchwork)

set.seed(1)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 2 データ
気温 <- rnorm(100, 20,5) %>% round(1) 
休日 <- rbinom(100, 1, 2/7)
lambda <- exp(-2+気温*0.2+休日*0.5)
売り上げ個数 <- rpois(100, lambda)
data　<- data.frame(気温, 休日=factor(休日), 売り上げ個数)
data %>% head()

plot <- ggplot() +
  geom_point(aes(x=data$気温, y=data$売り上げ個数, color=factor(data$休日))) + 
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text=element_text(size=25))+
  labs(x="気温", y="売り上げ個数", title="データ") +
  scale_color_manual("休日",values=c("red","blue"))
plot

# 3 brmsの使用
mcmc_result <- brm(
  data = data,
  formula = 売り上げ個数~ 気温 + 休日,
  family = poisson,
  seed = 1,
  iter = 1000, warmup = 200, chains = 4, thin=1
)

# 4 分析結果
## 4.1 推定結果
print(mcmc_result)

## 4.2 収束の確認
theme_set(theme_classic(base_size = 10, base_family = "HiraKakuProN-W3"))
plot(mcmc_result)

## 4.3 λの確認
plot(conditional_effects(mcmc_result, effects="気温:休日"), points=TRUE) %>%
  wrap_plots() + plot_annotation(title="λの推定結果")

## 4.4 予測分布
plot(conditional_effects(mcmc_result, effects="気温:休日", method="predict"), points=TRUE) %>%
  wrap_plots() + plot_annotation(title="予測分布")
