data {
  int N;
  vector[N] y;
  vector[N] x1;
  vector[N] x2;
  
  int N_hat;
  vector[N_hat] x_hat;
}

parameters {
  vector[3] b;
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] mu;
  mu = b[1] + b[2]*x1 + b[3]*x2;
}

model{
  y ~ normal(mu, sigma);
}

generated quantities{
  vector[N_hat] mu_hat1;
  vector[N_hat] mu_hat0;
  vector[N_hat] y_hat1;
  vector[N_hat] y_hat0;
  
  for(n in 1:N_hat){
    mu_hat1[n] = b[1] + b[2]*x_hat[n] + b[3];
    mu_hat0[n] = b[1] + b[2]*x_hat[n];
    y_hat1[n] = normal_rng(mu_hat1[n], sigma);
    y_hat0[n] = normal_rng(mu_hat0[n], sigma);
  }
}
