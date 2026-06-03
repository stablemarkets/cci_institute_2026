data {
  int<lower=0> n;
  int<lower=0> K;
  int<lower=1> v[n];
  int<lower=0, upper=1> a[n]; 
  int<lower=0, upper=1> l[n]; 
  real y[n]; 
}

parameters {
  
  real beta0;
  real beta1[K];
  real beta2;
  
  real<lower=0> sigma;

}

model {
  
  for(i in 1:n){
    y[i] ~ normal( beta0 + beta1[ v[i] ]*a[i] + beta2*l[i], sigma );
  }
  
}
