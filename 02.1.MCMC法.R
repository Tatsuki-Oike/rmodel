# 1 ライブラリ
library(ggplot2)
set.seed(1)

# 2 事後分布とKernel

## 2.1 事後分布
x <- seq(-5, 5, 0.1)
p <- dnorm(x, 0, 1)

ggplot() + 
  geom_line(aes(x, p)) +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 30))+
  labs(x="θ", y="p(θ|X)")

## 2.2 Kernel
p_kernel <- dnorm(x, 0, 1)*100

ggplot() + 
  geom_line(aes(x, p_kernel)) +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 30))+
  labs(x="θ", y="Kernel(θ)")

## 2.3 Kernel関数
Kernel <- function(theta){
  y <- dnorm(theta, 0, 1)*100
  return(y)
}

# 3 MH法
N <- 1000
mcmcsample<- c()
mcmcsample[1] <- runif(1,-10,10)

for(n in 1:(N-1)){
  
  # rateの計算
  propose <- mcmcsample[n] + rnorm(1,0,1)
  rate <- Kernel(propose)/Kernel(mcmcsample[n])
  r <- min(rate,1)
  
  # サンプルが採用か否か
  if(r>=runif(1,0,1)){
    mcmcsample[n+1] <- propose
  }else{
    mcmcsample[n+1] <- mcmcsample[n]
  }
}

# 4 可視化
ggplot() + 
  geom_line(aes(x=1:N, mcmcsample)) +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 30)) +
  labs(x="回数", y="MCMCサンプル")

ggplot() +
  geom_histogram(aes(mcmcsample), binwidth = 0.5) +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 30))+
  labs(x="MCMCsample", y="個数")
