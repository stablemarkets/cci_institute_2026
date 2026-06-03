#_______________________________________________________________________________
## Load Packaged _______________________________________________________________
#_______________________________________________________________________________
library(rstan)
library(latex2exp)

#_______________________________________________________________________________
## Simulate some data __________________________________________________________
#_______________________________________________________________________________

set.seed(1)

par_vec = c(2,3,4,2,6,7,8,9,10,10)

K=10 # K strata
n = 100 ## 300 subjects
v = sample(1:K, n, replace=TRUE, prob=c(.30,.30,rep(.05, 8)  ) ) ## strata variable
l = rbinom(n, 1, 0.5) ## measured confounder
a = rbinom(n, 1, plogis(1 + 1*l ) ) ## simulate treatment assignment
y = rnorm(n, 1 + 1*l + par_vec[v]*a, 10) ## simulate outcome

#_______________________________________________________________________________
## Estimate causal effect using Bayes___________________________________________
#_______________________________________________________________________________

## stan model implementing sensitivty method described in slides.
mod = stan_model("hierarchical.stan")

## Bayesian standardization (over empirical distribution of l|v. ideally should 
##   standardize over Bayesian bootstrap as in Oganisian & Roy 2021 but keeping
##   things simple for this course.)
stan_data = list(n=n, K=K, a=a, l=l, y=y, v=v )

sampling_res = sampling(mod, seed=1,data = stan_data, chains = 1, iter = 2000, warmup = 1000)
beta1_post = summary(sampling_res, pars=c('beta1'))$summary

mu_mean = summary(sampling_res, pars=c('mu'))$summary[,1]

post_mean = beta1_post[,1]
post_lwr = beta1_post[,4]
post_upr = beta1_post[,8]

#_______________________________________________________________________________
## Estimate causal effect using MLE ____________________________________________
#_______________________________________________________________________________

mod_mle = stan_model("MLE.stan")

optimize_res = optimizing(mod_mle, seed=133, data=stan_data, hessian=TRUE)
mle_res = optimize_res$par[2:11]


#_______________________________________________________________________________
## Plot Results ________________________________________________________________
#_______________________________________________________________________________

plot(1:K, post_mean, ylim=c(-20, 20), pch=19, 
     xlab="Strata", 
     ylab="Causal Effect Estimate", 
     main="Bayesian Standardization")

segments(1:K, post_lwr, 1:K, post_upr, col="darkgray", lwd=2)
points(1:K, mle_res, col="blue", pch=19)
abline(h=mu_mean, col='black', lty=2)


plot(1:K, post_mean, pch=20, ylim=c(-5, 20), 
     xlab='Stratum, V', ylab=TeX('Estimates of $\\Psi(v)$'), 
     axes=F)
segments(1:K, post_lwr, 1:K, post_upr)

points(1:K, mle_res, col='red', pch=20)

abline(h=mu_mean, col='black', lty=2)

nk = table(v)
xlab_vals = paste0(1:K, "\n(n=", nk,")")
axis(1, at = 1:10, labels = xlab_vals, padj = .5  )
axis(2, at = seq(-5,20,5), labels = seq(-5,20,5)  )

legend('topleft', horiz=F, 
       legend=c(TeX('Posterior Estimate, $\\Psi(v)$'),
                TeX('Posterior Mean, $\\mu$ '),
                'Unpooled Empirical Estimate',), 
       pch=c(20, NA, 20,), lty=c(1,2, NA), 
       col=c('black','black','red'), bty='n')







       