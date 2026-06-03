set.seed(1)

foo = function(n,omega){ ## implement MLE interval
  y = rbinom(n,1, omega)
  interval = mean(y) + qnorm(c(.025,.975))*sqrt( (mean(y)*(1-mean(y)) ) /n ) 
  cover = interval[1] < omega & omega < interval[2]
  return(cover)
}

foo2 = function(n,omega){ ## implement agresti-coull interval
  y = rbinom(n,1, omega)
  
  n_tilde = n + 4
  
  omega_hat = (1/n_tilde) * ( 2 + sum(y) )
  
  interval = omega_hat + qnorm(c(.025,.975))*sqrt( (omega_hat*(1-omega_hat) ) /n ) 
  
  cover = interval[1] < omega & omega < interval[2]
  
  return(cover)
}


par(mfrow=c(1,2))

xvec = 1:100
do_all = function(x){ mean(replicate(1000, foo(x, .965))) }
res = sapply(xvec, do_all)

do_all2 = function(x){ mean(replicate(1000, foo2(x, .965))) }
res2 = sapply(xvec, do_all2)

plot(xvec, res, ylim=c(0,1), pch=20, ylab='Estimated Coverage', xlab='n', 
     main = 'True omega=.965')
points(xvec, res2, col='blue',pch=20, cex=.5)
abline(h=.95, col='red', lty=2)

legend('bottomright', legend=c('Agresti-Coull','MLE'), 
       col=c('blue','black'), pch=c(20,20))

xvec = seq(.01,.99,by=.01)
do_all = function(x){ mean(replicate(1000, foo(30, x))) }
res = sapply(xvec, do_all)

do_all2 = function(x){ mean(replicate(1000, foo2(30, x))) }
res2 = sapply(xvec, do_all2)

plot(xvec, res, ylim=c(0,1), pch=20, ylab='Estimated Coverage', 
     xlab='True omega', main = 'n=30')
points(xvec, res2, col='blue',pch=20, cex=.5)
abline(h=.95, col='red', lty=2)


