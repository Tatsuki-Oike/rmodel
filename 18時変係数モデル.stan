data {
  int T;
  vector[T] x;
  vector[T] y;
}

parameters {
  vector<lower=0>[T] mu;
  vector<lower=0>[T] b;
  real<lower=0> sigma_w;
  real<lower=0> sigma_v;
  real<lower=0> sigma_t;
}

transformed parameters{
  vector[T] alpha;
  for(t in 1:T){
    alpha[t] = mu[t] + b[t]*x[t];
  }
}

model {
  mu[2:T] ~ normal(mu[1:(T-1)], sigma_w);
  b[2:T] ~ normal(b[1:(T-1)], sigma_t);
  y ~ normal(alpha, sigma_v);
}

generated quantities{
  vector[T] y_pred;
  for(t in 1:T){
    y_pred[t] = normal_rng(alpha[t], sigma_v);
  }
}
