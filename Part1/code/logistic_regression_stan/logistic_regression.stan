data {
  int<lower=0> n; 
  int<lower=0> P; 
  int<lower=0, upper=1> y[n]; 
  matrix[n, P+1] X;
}

parameters {
  vector[P+1] beta;
}

model {
  beta ~ normal(0,3);
  
  for(i in 1:n){
    y[i] ~ bernoulli( inv_logit( X[i,]*beta) );
  }
}
