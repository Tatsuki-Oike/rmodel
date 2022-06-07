data {
  int T;
  vector[T] y;
}

parameters {
  vector[T] mu;
  vector[T] gamma;
  real<lower=0> sigma_w;
  real<lower=0> sigma_s;
  real<lower=0> sigma_v;
}

transformed parameters{
  vector[T] alpha;
  alpha = mu + gamma;
}

model {
  mu[2:T] ~ normal(mu[1:(T-1)], sigma_w);
  for(t in 7:T){
    gamma[t] ~ normal(-sum(gamma[(t-6):(t-1)]), sigma_s);
  }
  y ~ normal(alpha, sigma_v);
}

generated quantities{
  vector[T] y_pred;
  for(t in 1:T){
    y_pred[t] = normal_rng(alpha[t], sigma_v);
  }
}
