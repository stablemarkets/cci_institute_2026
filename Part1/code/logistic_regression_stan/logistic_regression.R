library(rstan)

n = 100
x1 = rnorm(n)
x2 = rnorm(n)
y = rbinom(n,1 , plogis(1 +1*x1 -1*x2) )

X = model.matrix( ~ x1 + x2)
head(X)
dim(X)


stan_data = list(n=n, P=2, X=X, y=y)

mod = stan_model("logistic_regression.stan")

sampling_res = sampling(mod, seed=1,data = stan_data,
                        chains = 1, iter = 10000, warmup = 5000)

summary(sampling_res, pars=c('beta'))

## compare with frequentist estimator of the GLM 
summary(glm(y ~ x1 + x2, family=binomial(link='logit')))
