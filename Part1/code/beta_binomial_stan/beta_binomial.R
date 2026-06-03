library(rstan)

#///////////////////////////////////////////////////////////////////////////////
#### Example: beta-bernoulli model ####
#///////////////////////////////////////////////////////////////////////////////

set.seed(1)

n = 30
y = rbinom(n = n, size = 1, prob = .37 )

mod = stan_model("beta_binomial_model.stan")

## data fed to R must be named list
## with name corresponding to object 
## names declared in "data" block
stan_data = list(n = n, 
                 y = y, 
                 alpha = 1, 
                 beta=1)

sampling_res = sampling(mod, seed=1,
                        data = stan_data,
                        chains = 4,
                        iter = 2000,
                        warmup = 1000)

summary(sampling_res)

#///////////////////////////////////////////////////////////////////////////////
#### Example: reparameterized beta-bernoulli model ####
#///////////////////////////////////////////////////////////////////////////////

mod = stan_model("beta_bern_reparam.stan")

## data fed to R must be named list
## with name corresponding to object 
## names declared in "data" block
stan_data = list(n = n, 
                 y = y, 
                 mu = 0, 
                 sigma=1)

sampling_res = sampling(mod, seed=1,
                        data = stan_data,
                        chains = 1,
                        iter = 10000,
                        warmup = 5000)

summary(sampling_res)

plot(sampling_res)
traceplot(sampling_res)

draws_list = extract(sampling_res, pars = 'omega')
draws = draws_list$omega
head(draws)
hist(draws, breaks=100)
