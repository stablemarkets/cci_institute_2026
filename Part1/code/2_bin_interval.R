set.seed(1)

foo = function(n,omega){ ## implement MLE interval
  y = rbinom(n,1, omega)
  interval = mean(y) + qnorm(c(.025,.975))*sqrt( (mean(y)*(1-mean(y)) ) /n ) 
  cover = interval[1] < omega & omega < interval[2]
  return(cover)
}

par(mfrow=c(1,2))

xvec = 1:100
do_all = function(x){ mean(replicate(1000, foo(x, .965))) }
res = sapply(xvec, do_all)

plot(xvec, res, ylim=c(0,1), pch=20, ylab='Estimated Coverage', xlab='n', 
     main = 'True omega=.965')
abline(h=.95, col='red', lty=2)

legend('bottomright', legend=c('MLE'), 
       col=c('black'), pch=c(20,20))

xvec = seq(.01,.99,by=.01)
do_all = function(x){ mean(replicate(1000, foo(30, x))) }
res = sapply(xvec, do_all)


plot(xvec, res, ylim=c(0,1), pch=20, ylab='Estimated Coverage', 
     xlab='True omega', main = 'n=30')
abline(h=.95, col='red', lty=2)


