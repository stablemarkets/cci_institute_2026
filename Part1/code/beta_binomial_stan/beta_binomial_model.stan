data {
  int<lower=0> n; // sample size
  int<lower=0, upper=1> y[n]; // binary outcome
  real<lower=0> alpha; // hyperparamter value
  real<lower=0> beta; // hyperparamter value
}

parameters {
  real<lower=0, upper=1> omega;
}

model {
  omega ~ beta(alpha, beta);
  y ~ bernoulli(omega);
}