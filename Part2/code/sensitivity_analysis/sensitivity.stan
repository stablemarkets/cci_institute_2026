data {
  int<lower=0> n; 
  int<lower=0, upper=1> a[n]; 
  int<lower=0, upper=1> l[n]; 
  int<lower=0, upper=1> y[n]; 
  real xi1;
  real xi2;
}

parameters {
  
  real beta0;
  real beta1;
  real beta2;
  real u[n];
  
  real gam0;
  real gam1;
  
  real<lower=0, upper=1> theta1;
}

model {
  
  theta1 ~ beta(2,2);
  u ~ normal(0,1);
  beta1 ~ normal(0,1);
  
  for(i in 1:n){
    y[i] ~ bernoulli( inv_logit( beta0 + beta1*a[i] + beta2*l[i] + xi1 * u[i] ) );
    a[i] ~ bernoulli( inv_logit( gam0 + gam1*l[i] + xi2* u[i] ) );
    l[i] ~ bernoulli(theta1);
  }
  
}


generated quantities {
  
  real diff[n];
  real causal_effect;
  // standardize over empirical distribution of l
  for(i in 1:n){
    diff[i] = inv_logit(beta0 + beta1 *1 + beta2 * l[i] + xi1 * u[i] ) - inv_logit(beta0 + beta1*0 + beta2 * l[i] + xi1 * u[i] );
  }
  
  causal_effect = mean(diff) ;
  
}
