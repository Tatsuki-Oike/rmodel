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
  vector[N] p;
  p = inv_logit(b[1] + b[2]*x1 + b[3]*x2);
}

model{
  y ~ bernoulli(p);
}

generated quantities{
  vector[N_hat] p_hat1;
  vector[N_hat] p_hat0;
  
  for(n in 1:N_hat){
    p_hat1[n] = inv_logit(b[1] + b[2]*x_hat[n] + b[3]);
    p_hat0[n] = inv_logit(b[1] + b[2]*x_hat[n]);
  }
}
