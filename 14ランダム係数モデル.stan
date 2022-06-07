data {
  int N;
  int y[N];
  vector[N] x1;
  vector[N] x2;
  
  int K;
  int<lower=1, upper=K> x3[N];
}

parameters {
  vector[3] b;
  vector[K] r;
  
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] lambda;
  lambda = exp(b[1] + b[2]*x1 + (b[3] + r[x3]).*x2);
}

model{
  r ~ normal(0, sigma);
  y ~ poisson(lambda);
}
