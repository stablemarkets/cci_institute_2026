data {
  int<lower=0> n;
  int<lower=0> K;
  int<lower=1> v[n];
  int<lower=0, upper=1> a[n]; 
  int<lower=0, upper=1> l[n]; 
  real y[n]; 
}

parameters {
  
  real mu;
  
  real beta0;
  real beta1[K];
  real beta2;

  real<lower=0> sigma;
  real<lower=0> tau;
}

model {
  
  sigma ~ gamma(1,1);
  beta0 ~ normal(0,1);
  beta2 ~ normal(0,1);
  
  tau ~ gamma(1,1);
  beta1 ~ normal(mu,sigma/tau);

  for(i in 1:n){
    y[i] ~ normal( beta0 + beta1[ v[i] ]*a[i] + beta2*l[i], sigma );
  }
  
}


