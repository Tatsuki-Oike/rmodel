# 1 rstanのインストール
## 1.1 rstanのインストール
install.packages("rstan", repos='https://cloud.r-project.org/', dependencies = TRUE)
library(rstan)

## 1.2 C++の確認
pkgbuild::has_build_tools(debug=TRUE)

# 3 Stanの利用例
# 3.1 Stanの効率化
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 3.2 sampleデータ
set.seed(1)
data <- rnorm(100, 50, 10)
dlist <- list(N=100, y=data)

# 3.3 Stanの利用
mcmc_result <- stan(file="1.sample.stan", data=dlist, seed=1)
mcmc_result

