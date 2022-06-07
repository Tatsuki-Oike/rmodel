data {
  int T;
  vector[T] y;
  
  int T_pred;
}

parameters {
  vector[T] mu;
  real<lower=0> sigma_w;
  real<lower=0> sigma_v;
}

model {
  mu[2:T] ~ normal(mu[1:(T-1)], sigma_w);
  y ~ normal(mu, sigma_v);
}

generated quantities{
  vector[T+T_pred] mu_pred;
  vector[T+T_pred] y_pred;
  
  mu_pred[1:T] = mu;
  for(t in 1:T_pred){
    mu_pred[T+t] = normal_rng(mu_pred[T+t-1], sigma_w);
  }
  for(t in 1:(T+T_pred)){
    y_pred[t] = normal_rng(mu_pred[t], sigma_v);
  }
}
