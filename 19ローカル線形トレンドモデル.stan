data {
  int T;
  vector[T] y;
}

parameters {
  vector[T] mu;
  vector[T] delta;
  real<lower=0> sigma_w;
  real<lower=0> sigma_v;
  real<lower=0> sigma_d;
}

model {
  delta[2:T] ~ normal(delta[1:(T-1)], sigma_d);
  mu[2:T] ~ normal(mu[1:(T-1)]+delta[1:(T-1)], sigma_w);
  y ~ normal(mu, sigma_v);
}

generated quantities{
  vector[T] y_pred;
  for(t in 1:T){
    y_pred[t] = normal_rng(mu[t], sigma_v);
  }
}
