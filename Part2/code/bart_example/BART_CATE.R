library(BayesTree)
library(scales)

n <- 300 
set.seed(312)

l = seq(1,pi, length.out = n) + rnorm(n,0,.1) # confounder
a = rbinom(n, 1, plogis( -2 + .1*l) )
y0 = rnorm( n = length(l), sin(3*l), .1*l)
y1 = rnorm( n = length(l), sin(2*l) + .5*(y0 - sin(3*l)), .07*l)

y = a*y1 + (1-a)*y0

d = data.frame(y = y, a = a, l = l)

grid = seq(min(d$l), max(d$l), length.out=1000)

d_a1 = d[d$a==1, ]
d_a0 = d[d$a==0, ]

bart_a1 = bart(x.train = d_a1$l, y.train = d_a1$y, x.test = grid )
bart_a0 = bart(x.train = d_a0$l, y.train = d_a0$y, x.test = grid )

## bart_a0$yhat.test contains draws of E^{(t)}[Y^0 | l ]
## for t=1,2, ..., 1000 draws (rows) at each l in grid (columns).

## bart_a1$yhat.test is a matrix of draws of E^{(t)}[Y^1 | l ]
## for t=1,2, ..., 1000 draws (rows) at each l in grid (columns).

png("bart_example.png", width=1000, height=500)
par(mfrow=c(1,2))
plot(d$l[d$a==0], d$y[d$a==0], xlim=c(.9,3.3), pch=20,col=alpha('black',alpha=.2),
     xlab='L', ylab='Y', ylim=c(-1.7,1.7), 
     main='Posterior Draws of E[Y^a|L=l] \nBART', 
     cex.axes=1.5, cex.main=1.5)

legend('bottomleft', 
       legend=c('a=1','a=0'),
       col = c('steelblue','black'), bty='n', lty=1, lwd=2, 
       cex=1.5)

for(j in 1:50){
  lines(grid, bart_a0$yhat.test[100+j,], col=alpha('darkgray',alpha=.8), pch=20)
}
lines(grid, bart_a0$yhat.test.mean, col='black', lwd=2) 

for(j in 1:50){
  lines(grid, bart_a1$yhat.test[100+j,], col=alpha('steelblue',alpha=.5), pch=20)
}
lines(grid, bart_a1$yhat.test.mean, col='darkblue', lwd=2) 

points(d$l[d$a==1], d$y[d$a==1], col=alpha('darkblue',alpha=.5),, pch=20)
points(d$l[d$a==0], d$y[d$a==0], col=alpha('black',alpha=.2),, pch=20)


cate = bart_a1$yhat.test - bart_a0$yhat.test

post_mean_cate = apply(cate,2,mean)
plot(grid, post_mean_cate, xlim=c(.9,3.3), type='l',col='black',
     xlab='L', ylab='CATE', ylim=c(-4,2.5), 
     main='Posterior Draws of the CATE\nBART', 
     cex.labs=1.5, cex.main=1.5)

for(j in 1:50){
  lines(grid, cate[100+j,], col=alpha('steelblue',alpha=.5), pch=20)
}
lines(grid, post_mean_cate, col='darkblue', lwd=2)
lines(grid, sin(2*grid)-sin(3*grid), col='red', lwd=2)

legend('bottomleft', 
       legend=c('Posterior Draws/Mean of CATE', 'True CATE'),
       col = c('steelblue','red'), bty='n', lty=1, lwd=2, 
       pch=c(NA, 20), cex=1.5)
dev.off()
