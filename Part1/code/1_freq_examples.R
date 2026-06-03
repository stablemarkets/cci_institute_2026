library(latex2exp)


##### -------------------------------------------------------------------- ##### 
##### ---------------------- Frequentist Examples ------------------------ ##### 
##### -------------------------------------------------------------------- ##### 

sim = function(true_omega,n){
  ## simulate data
  y = rbinom(n, 1,true_omega)
  
  ## point estimate 
  ybar = mean(y)
  
  ## test statistic
  test_stat = abs(  ( sqrt(n)*(ybar - .5) ) / sqrt( ybar*(1-ybar) )  ) 

  ## 95% interval estimate
  lwr = ybar - 1.96*sqrt( (ybar*(1-ybar)) /n )
  upr = ybar + 1.96*sqrt( (ybar*(1-ybar)) /n )
  
  
  return(c(ybar, test_stat, lwr, upr))
}

## simulate 100 data sets and compute point estimate, test statistic, and 
## interval estimates for each.
set.seed(13)
res = replicate(100, sim(true_omega = .5, n=100))

png("1_freq_examples.png", width = 800, height = 300)
par(mfrow=c(1,3))

plot(res[1,], 1:100, pch=20, 
     xlab=TeX("$\\hat{\\omega}$"), ylab='Dataset', 
     ylim=c(1,100))
abline(v=.5, col='red', lty=2)

plot(res[2,], 1:100, pch=20, 
     xlab=TeX("$|T(Y_n)|$"), ylab='Dataset', 
     ylim=c(1,100), 
     col=ifelse(res[2,]>1.96, 'red', 'black'))
abline(v=1.96, col='red', lty=2)

plot(res[1,], 1:100, pch=20, 
     xlab="95% Confidence Interval", ylab='Dataset', 
     ylim=c(1,100), 
     xlim=c(.2,.8),
     col=ifelse(res[2,]>1.96, 'red', 'black'))
segments(res[3,], 1:100, res[4,], 1:100,
         col=ifelse(res[2,]>1.96, 'red', 'black'))
abline(v=.5, col='red', lty=2)
dev.off()
