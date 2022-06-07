# 1 ライブラリ
library(ggplot2)
set.seed(1)

# 2 MH法
Kernel <- function(theta){
  y <- dnorm(theta, 0, 1)*100
  return(y)
}

MH <- function(current){
  propose <- current + rnorm(1,0,1)
  rate <- Kernel(propose)/Kernel(current)
  r <- min(rate,1)
  if(r>=runif(1,0,1)) propose else current
}

# 3 MCMCサンプル

## 3.1 MCMCサンプル
N <- 100 # iter
mcmcsample<- c()
mcmcsample[1] <- runif(1,-10,10)
for(n in 1:(N-1)){
  mcmcsample[n+1] <- MH(mcmcsample[n])
}

## 3.2 warmup
warmup <- 10
mcmcsample[(warmup+1):N]

## 3.3 thin
thin <- 2
mcmcsample[seq(1,N,thin)]

## 3.4 chains
mcmcsample1 <- c()
mcmcsample2 <- c()
mcmcsample3 <- c()
mcmcsample1 [1] <- runif(1,-10,10)
mcmcsample2 [1] <- runif(1,-10,10)
mcmcsample3 [1] <- runif(1,-10,10)
for(n in 1:(N-1)){
  mcmcsample1[n+1] <- MH(mcmcsample1[n])
  mcmcsample2[n+1] <- MH(mcmcsample2[n])
  mcmcsample3[n+1] <- MH(mcmcsample3[n])
}
mcmcsamples <- cbind(mcmcsample1, mcmcsample2, mcmcsample3)

ggplot() + 
  geom_line(aes(x=1:N, mcmcsample1)) +
  geom_line(aes(x=1:N, mcmcsample2), col="blue") +
  geom_line(aes(x=1:N, mcmcsample3), col="red") +
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 30)) +
  labs(x="回数", y="MCMCサンプル")

# 4 パラメータ推定
median <- median(mcmcsamples)
mean <- mean(mcmcsamples)
quantile <- quantile(mcmcsamples, p=c(0.025, 0.975))

ggplot() +
  geom_histogram(aes(mcmcsamples), binwidth = 0.1) +
  geom_point(aes(x=median, 0), size=5, col="blue") + 
  geom_point(aes(x=mean, 0), size=5, col="red") + 
  geom_point(aes(x=quantile, 0), size=5, col="green") + 
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 30))+
  labs(x="MCMCsample", y="個数")
