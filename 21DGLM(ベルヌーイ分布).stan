data {
  int T;
  int y[T];
}

parameters {
  vector[T] mu;
  real<lower=0> sigma_w;
}

transformed parameters{
  vector[T] p;
  p = inv_logit(mu);
}

model {
  mu[2:T] ~ normal(mu[1:(T-1)], sigma_w);
  y ~ bernoulli(p);
}
