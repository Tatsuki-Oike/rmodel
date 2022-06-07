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
}

transformed parameters{
  vector[N] lambda;
  lambda = exp(b[1] + b[2]*x1 + b[3]*x2);
}

model{
  y ~ poisson(lambda);
}

generated quantities{
  vector[N_hat] lambda_hat1;
  vector[N_hat] lambda_hat0;
  int y_hat1[N_hat];
  int y_hat0[N_hat];
  
  for(n in 1:N_hat){
    lambda_hat1[n] = exp(b[1] + b[2]*x_hat[n] + b[3]);
    lambda_hat0[n] = exp(b[1] + b[2]*x_hat[n]);
    y_hat1[n] = poisson_rng(lambda_hat1[n]);
    y_hat0[n] = poisson_rng(lambda_hat0[n]);
  }
}
