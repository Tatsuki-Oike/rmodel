data {
  int N;
  int y[N];
  vector[N] x1;
  vector[N] x2;
  
  int N_hat;
  vector[N_hat] x_hat;
}

parameters {
  vector[3] b;
  
  vector[N] r;
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] lambda;
  lambda = exp(b[1] + b[2]*x1 + b[3]*x2+r);
}

model{
  r ~ normal(0, sigma);
  y ~ poisson(lambda);
}

generated quantities{
  vector[N_hat] lambda_hat1;
  vector[N_hat] lambda_hat0;
  
  for(n in 1:N_hat){
    lambda_hat1[n] = exp(b[1] + b[2]*x_hat[n] + b[3]);
    lambda_hat0[n] = exp(b[1] + b[2]*x_hat[n]);
  }
}
