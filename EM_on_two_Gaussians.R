### Simulate data for a mixed model of Gaussians and run
# the E-M algorithm to estimate the parameters of the distributions.

gauss_data <- function(mixing.probs, means, sds, N){
  sample.data = c()
  distributions = apply(rmultinom(N,size=1,prob=mixing.probs),2,function(xx) which(xx>0))
  for(i in 1:N){
    d = distributions[i]
    sample.data=c(sample.data,rnorm(1,mean=means[d],sd=sds[d]))
  }
  return(cbind(sample.data,distributions))
}


# mixing.true = c(.2,.3,.5)
# means.true = c(4,1,10)
# sds.true = c(1,.5,1.3)
# set.seed(10)
mixing.true=c(.3,.4,.3)
if(sum(mixing.true)!=1){
  print('probabilities not equal to one. Adjusting')
  mixing.true=mixing.true/sum(mixing.true)
}
means.true=c(1,7,15)
sds.true=c(1,1,1)
#sds.true=runif(3,max=3)
N=10000
results = gauss_data(mixing.true,means.true,sds.true,N)
sample.data=results[,1]

par(mfrow=c(2,2))
hist(sample.data,20,main='True Distributions')
xseq= seq(min(means.true)-3*max(sds.true),max(means.true)+3*max(sds.true),by = .001)
colors = c('red','blue','green')
for (i in 1:length(means.true)){
  # new.mean=N*mixing.true[i]*means.true[i]
  lines(xseq,N*mixing.true[i]*dnorm(xseq,means.true[i],sds.true[i]),type='l',col=colors[i])
}

# mixing.prob=c(.2,.3,.5)
# means=c(4,1,10)
# sds=c(1,.5,1.3)
mixing.prob=c(.3,.4,.3); 
# means=means.true+c(2,0,-3)
means = c(0,0,0)
while (min(diff(means))<3){
  means = mean(sample.data)+rnorm(3,0,3)
}
# sds=sds.true;+c(0,0,1);# + runif(3)
sds = c(1,1,1)


K=length(mixing.prob)
cat('\nmixing probs = ',mixing.prob)
cat('\nmeans = ',means)
cat('\nsds = ',sds)

## E-M algorithm.
iter=0
means.prev = 0
sds.prev = 0
maxiter=7
while(iter<maxiter){ # will need to update this for a better stopping criteria
  if(sum(means-means.prev)<1e-4 & sum(sds-sds.prev)<1e-4){
    print('Algorithm converged within 1e-4')
    break
  }
  means.prev = means
  sds.prev = sds
  ### plot results of current step
  hist(sample.data,20,main=sprintf('iter = %i',iter))
  for (i in 1:length(means)){
    lines(xseq,N*mixing.prob[i]*dnorm(xseq,means[i],sds[i]),type='l',col=colors[i])
  }
  
  iter=iter+1
  ### E-step
  r = matrix(nrow=N,ncol=K)
  for(i in 1:N){
    denom = 0
    for(k in 1:K){
      numerator=  mixing.prob[k]*dnorm(sample.data[i],means[k],sds[k])
      denom=denom+numerator
      r[i,k]=numerator
    }
    r[i,]=r[i,]/denom
  }
  ### M-step
  rksum = apply(r,2,sum)
  rksum2= apply(r,2,function(xx) sum(xx**2))
  mixing.prob=rksum/N
  for(i in 1:K){
    means[i]=r[,i]%*%sample.data/rksum[i]
    #sds[i]=r[,i]%*%((sample.data-means[i])**2)/rksum[i]
    sds[i]=r[,i]**2%*%((sample.data-means[i])**2)/rksum2[i]
  }
  # print(means)
  # print(mixing.prob)
  cat('\nmixing probs = ',mixing.prob)
  cat('\nmeans = ',means)
  cat('\nsds = ',sds)
  
}
if(iter==maxiter){print('\n 
                        Algorithm did not converge :(')}


