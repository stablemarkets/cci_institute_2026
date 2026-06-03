library(latex2exp)
set.seed(1)

par(mfrow=c(1,2))

n = 30
alpha = 1
beta = 1

y = rbinom(n = n, size = 1, prob = .2 )
ybar = mean(y)

## draw prior density
curve(dbeta(x, shape1=alpha, shape2=beta), from=0, to=1, ylim=c(0,8), 
      xlab=TeX("$\\theta$"), ylab='density', 
      main = TeX('Beta-Binomial Posterior, $n=30$, $\\alpha = \\beta = 1$') )
## draw prior mean
prior_mean = alpha/(alpha+beta)
segments(x0 = prior_mean, x1=prior_mean, 
         y0 = 0 , y1=dbeta( prior_mean, alpha, beta ),
         col='black', lty=2 ) 
# draw posterior density
curve( dbeta( x, shape1 = n*ybar + alpha, shape2 = n*(1-ybar) + beta ) , 
       from=0, to=1, add=T, col='blue')
## draw posterior mean
post_mean = (n*ybar + alpha) / (n + alpha + beta)
segments(x0 = post_mean, x1=post_mean, 
         y0 = 0 , y1=dbeta( post_mean, n*ybar + alpha, n*(1-ybar) + beta ),
         col='blue', lty=2 ) 

## draw MLE
abline(v=ybar, col='red')

legend('topright', 
       legend = c('Prior density', 'Prior mean','Posterior density', 
                  'Posterior mean', 'MLE'), 
       col=c('black', 'black','blue', 'blue', 'red'), 
       lty=c(1,2,1,2,1))

alpha = 10
beta = 10

## draw prior density
curve(dbeta(x, shape1=alpha, shape2=beta), from=0, to=1, ylim=c(0,8), 
      xlab=TeX("$\\theta$"), ylab='density', 
      main = TeX('Beta-Binomial Posterior, $n=30$, $\\alpha = \\beta = 10$') )
## draw prior mean
prior_mean = alpha/(alpha+beta)
segments(x0 = prior_mean, x1=prior_mean, 
         y0 = 0 , y1=dbeta( prior_mean, alpha, beta ),
         col='black', lty=2 ) 
# draw posterior density
curve( dbeta( x, shape1 = n*ybar + alpha, shape2 = n*(1-ybar) + beta ) , 
       from=0, to=1, add=T, col='blue')
## draw posterior mean
post_mean = (n*ybar + alpha) / (n + alpha + beta)
segments(x0 = post_mean, x1=post_mean, 
         y0 = 0 , y1=dbeta( post_mean, n*ybar + alpha, n*(1-ybar) + beta ),
         col='blue', lty=2 ) 

## draw MLE
abline(v=ybar, col='red')

legend('topright', 
       legend = c('Prior density', 'Prior mean','Posterior density', 
                  'Posterior mean', 'MLE'), 
       col=c('black', 'black','blue', 'blue', 'red'), 
       lty=c(1,2,1,2,1))
