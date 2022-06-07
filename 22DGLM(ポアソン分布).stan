data {
  int T;
  int y[T];
}

parameters {
  vector[T] mu;
  vector[T] gamma;
  real<lower=0> sigma_w;
  real<lower=0> sigma_s;
  real<lower=0> sigma_v;
}

transformed parameters{
  vector[T] lambda;
  lambda = exp(mu+gamma);
}

model {
  mu[2:T] ~ normal(mu[1:(T-1)], sigma_w);
  for(t in 7:T){
    gamma[t] ~ normal(-sum(gamma[(t-6):(t-1)]), sigma_s);
  }
  y ~ poisson(lambda);
}

generated quantities{
  int y_pred[T];
  for(t in 1:T){
    y_pred[t] = poisson_rng(lambda[t]);
  }
}
