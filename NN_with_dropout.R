### Basic NN example

rm(list=ls())

sigmoid <- function(x){
  return(1/(1+exp(-x)))
}

create.data <- function(m,n,test.size=30){
  # m = number of data points, n = number of nodes
  x = matrix(rnorm(m*n),nrow=m,ncol=n)
  w <- rnorm(n,2,1)
  y = (x^2)%*%w + apply(log1p(abs(x)),1,sum) + rnorm(m)
  y = y - min(y)
  y = y/max(y)
  test.sample = sample(m,test.size)
  x.test = x[test.sample,]
  y.test = y[test.sample]
  x = x[-test.sample,]
  y = y[-test.sample]
  return(list(x,y,x.test,y.test))
}

run.NN <- function(m,n,epochs,lr=.01,lr.b=.01,test.size = NULL){
  if(is.null(test.size)){test.size=round(m/4)}
  tmp = create.data(m, n[1],test.size)
  x = tmp[[1]]; y = tmp[[2]]; x.test = tmp[[3]]; y.test = tmp[[4]]
  num.layers = max(length(n)-1,1)
  # initialize parameters
  W.1 = matrix(rnorm(n*n),nrow=n,ncol=n)
  b.1 = rnorm(n)
  W.out = matrix(rnorm(n*1),nrow=n,ncol=1)
  b.out = rnorm(1)
  train.errs = c()
  test.errs = c()
  for(iter in 1:epochs){
    # train the model
    err.total=0
    for(i in 1:nrow(x)){
      # feed forward
      h.in = W.1%*%x[i,] + b.1
      h.act = sigmoid(h.in)
      out.in = sum(W.out*h.act) + b.out
      out.act = sigmoid(out.in)
      error = .5*sum(y[i]-out.act)^2
      d_W.out = (y[i]-out.act)*(out.act)*(1-out.act)*h.act
      d_W.1 = matrix((y[i]-out.act)*(out.act)*(1-out.act)*W.out*h.act*(1-h.act),ncol=1)%*%matrix(x[i,],nrow=1)
      # the b's I don't really get why - it's just a general shift of the field
      d_b.out = sum((y[i]-out.act)*(out.act)*(1-out.act))
      d_b.1 = (y[i]-out.act)*(out.act)*(1-out.act)*W.out*h.act*(1-h.act)
      W.1 = W.1 + lr*d_W.1
      W.out = W.out + lr*d_W.out
      b.1 = b.1 + lr.b*lr*d_b.1
      b.out = b.out + lr.b*d_b.out
      err.total = err.total+error
    }
    h.act = sigmoid(W.1%*%t(x.test)+t(matrix(b.1,nrow=length(y.test),ncol=length(b.1),byrow=TRUE)))
    out.act = sigmoid(matrix(W.out,nrow=1)%*%h.act+rep(b.out,length(y.test)))
    mean.test.error = .5*sum((out.act-y.test)^2)/length(y.test)
    test.errs = c(test.errs,mean.test.error)
    train.errs = c(train.errs,err.total/nrow(x))
  }
  results = list(test.errs, train.errs, W.1, W.out, b.1, b.out)
  names(results) = c('test.errs', 'train.errs', 'W.1', 'W.out', 'b.1', 'b.out')
  return(results)
}

run.NN.with.dropout <- function(m,n.layers,epochs,lr=.01,lr.b=.01,lr.decay=0,
                                test.size = NULL,p=NULL,max.norm = NULL,resample=FALSE){
  if(is.null(test.size)){test.size=round(m/4)}
  if(!resample){
    tmp = create.data(m, n.layers[1],test.size)
    x = tmp[[1]]; y = tmp[[2]]; x.test = tmp[[3]]; y.test = tmp[[4]]
  }
  num.layers = max(length(n.layers)-1,1)
  if (length(n.layers)==1){n.layers = c(n.layers,n.layers)}
  # get dropout rates squared away.
  if(is.null(p)){p=c(0.8,rep(0.5,num.layers))
  }else if(length(p)==1){p=rep(p,num.layers+1)}
  if (length(p)!=(num.layers+1)){stop('p not the correct length')}
  
  # initialize parameters
  W.list = list()
  b.list = list()
  drop.list = list()
  for(i in 1:num.layers){
    W.list[[i]] = matrix(rnorm(n.layers[i+1]*n.layers[i]),
                         nrow=n.layers[i+1],ncol=n.layers[i])
    b.list[[i]] = rnorm(n.layers[i+1])
    drop.list[[i]] = matrix(,nrow=n.layers[i],ncol=n.layers[i])
  }
  drop.list[[num.layers+1]] = matrix(,nrow=n.layers[num.layers+1],ncol=n.layers[num.layers+1])
  W.out = matrix(rnorm(n.layers[num.layers+1]*1),nrow=n.layers[num.layers+1],ncol=1)
  b.out = rnorm(1)
  h.in = list()
  h.act = list()
  train.errs = c()
  test.errs = c()
  for(iter in 1:epochs){
    
########################### Sample Data ###################################
    if (resample){
      tmp = create.data(m, n.layers[1],test.size)
      x = tmp[[1]]; y = tmp[[2]]; x.test = tmp[[3]]; y.test = tmp[[4]]
    }
    err.total=0
    for(i in 1:nrow(x)){
      
########################## Create Dropout Matrices #######################
      
      for (k in 1:length(drop.list)){
        drop.vec = 0
        while(sum(drop.vec)==0){
          drop.vec = rbinom(n.layers[k],1,p[k])
        }
        drop.list[[k]]=diag(drop.vec)
      }
      
############################# Forward Propogation #########################
      
      h.in[[1]] = drop.list[[2]]%*%(W.list[[1]]%*%drop.list[[1]]%*%x[i,]+b.list[[1]])
      h.act[[1]] = sigmoid(h.in[[1]])
      # print(dim(h.act[[1]]))
      if (num.layers>1){
        for (j in 2:num.layers){
          h.in[[j]] = drop.list[[j+1]]%*%(W.list[[j]]%*%drop.list[[j]]%*%h.act[[j-1]]+b.list[[j]])
          h.act[[j]] = sigmoid(h.in[[j]])
        }
      }
      out.in = sum(drop.list[[num.layers+1]]%*%W.out*h.act[[num.layers]]) + b.out
      out.act = sigmoid(out.in)
      
############################## Back Propogation ############################      
      
      error = .5*sum(y[i]-out.act)^2
      d_out = (y[i]-out.act)*(out.act)*(1-out.act)# means d_error/d_out.in
      d_W.out = d_out*drop.list[[num.layers+1]]%*%h.act[[num.layers]]
      d_b.out = d_out
      d_act = rep(list(NA),num.layers)
      d_act[[num.layers]] <- (y[i]-out.act)*(out.act)*(1-out.act)*drop.list[[num.layers+1]]%*%W.out
      d_W = rep(list(NA),num.layers)
      if(num.layers>1){
        d_W[[num.layers]] <- (d_act[[num.layers]]*h.act[[num.layers]]*(1-h.act[[num.layers]]))%*%t(h.act[[num.layers-1]])%*%drop.list[[num.layers]]
      } else {
        d_W[[1]] <- (d_act[[1]]*h.act[[1]]*(1-h.act[[1]]))%*%x[i,]%*%drop.list[[1]]
      }
      
      if (num.layers>1){
        for (j in (num.layers-1):1){
          d_act[[j]]=t(drop.list[[j+2]]%*%W.list[[j+1]]%*%drop.list[[j+1]])%*%(d_act[[j+1]]*(h.act[[j+1]])*(1-h.act[[j+1]]))
          if(j!=1){
            d_W[[j]]=(d_act[[j]]*h.act[[j]]*(1-h.act[[j]]))%*%t(h.act[[j-1]])%*%drop.list[[j]]
          }else{
            d_W[[j]]=(d_act[[j]]*h.act[[j]]*(1-h.act[[j]]))%*%matrix(x[i,],nrow=1)%*%drop.list[[1]]
          }
        }
      }

      W.out = W.out + lr/(1+lr.decay*iter)*d_W.out
      b.out = b.out + lr.b/(1+lr.decay*iter)*d_b.out
      for(j in 1:num.layers){
        W.list[[j]] = W.list[[j]] + lr/(1+lr.decay*iter)*d_W[[j]]
        b.list[[j]] = b.list[[j]] + lr.b/(1+lr.decay*iter)*drop.list[[j+1]]%*%(d_act[[j]]*h.act[[j]]*(1-h.act[[j]]))
      }
      err.total = err.total+error
      # Use max-norm constraint
      if (!is.null(max.norm)){
        for(jj in 1:length(W.list)){
          for(kk in 1:nrow(W.list[[jj]])){
            if(sqrt(sum(W.list[[jj]][kk,]^2))>max.norm){
              W.list[[jj]][kk,] = W.list[[jj]][kk,]*max.norm/sqrt(sum(W.list[[jj]][kk,]^2))
            }
          }
        } # for (jj...)
        for(kk in 1:nrow(W.out)){
          if(sqrt(sum(W.out[kk,]^2))>max.norm){
            W.out[kk,] = W.out[kk,]*max.norm/sqrt(sum(W.out[kk,]^2))
          }
        }
      } # if (max.norm...)
    } # for (nrow(x)...)
    
####################### Calculate test error ###########################
    
    W.scaled = b.scaled = list()
    for (jj in 1:num.layers){
      W.scaled[[jj]] = W.list[[jj]]*p[jj]*p[jj+1]
      b.scaled[[jj]] = b.list[[jj]]*p[jj+1]
    }
    W.out.scaled = W.out*p[num.layers+1]
    
    h.in[[1]] = W.scaled[[1]]%*%t(x.test)+t(matrix(b.scaled[[1]],nrow=length(y.test),ncol=length(b.scaled[[1]]),byrow=TRUE))
    h.act[[1]] = sigmoid(h.in[[1]])
    
    if(num.layers>1){
      for (j in 2:num.layers){
        h.in[[j]] = W.scaled[[j]]%*%h.act[[j-1]]+t(matrix(b.scaled[[j]],nrow=length(y.test),ncol=length(b.scaled[[j]]),byrow=TRUE))
        h.act[[j]] = sigmoid(h.in[[j]])
      }
    }
    out.in = t(W.out.scaled)%*%h.act[[num.layers]] + b.out
    out.act = sigmoid(out.in)
    mean.test.error = .5*sum((out.act-y.test)^2)/length(y.test)
    test.errs = c(test.errs,mean.test.error)
    train.errs = c(train.errs,err.total/nrow(x))
  }
  results = list(test.errs, train.errs, W.scaled, W.out.scaled, b.scaled, b.out)
  names(results) = c('test.errs', 'train.errs', 'W.list', 'W.out', 'b.list',
                     'b.out')
  return(results)
}

run.NN.with.dropout.2 <- function(m,n.layers,epochs,lr=.01,lr.b=.01,test.size = NULL,p=NULL){
  if(is.null(test.size)){test.size=round(m/4)}
  num.layers = max(length(n.layers)-1,1)
  # get dropout rates squared away.
  if(is.null(p)){p=c(0.8,rep(0.5,num.layers))
  }else if(length(p)==1){p=rep(p,num.layers+1)}
  if (length(p)!=(num.layers+1)){stop('p not the correct length')}
  
  if (length(n.layers)==1){n.layers = c(n.layers,n.layers)}
  # initialize parameters
  W.list = list()
  b.list = list()
  drop.list = list()
  dropout.rates = list()
  for(i in 1:num.layers){
    W.list[[i]] = matrix(rnorm(n.layers[i+1]*n.layers[i]),
                         nrow=n.layers[i+1],ncol=n.layers[i])
    b.list[[i]] = rnorm(n.layers[i+1])
    drop.list[[i]] = matrix(,nrow=n.layers[i],ncol=n.layers[i])
    dropout.rates[[i]] = list()
  }
  drop.list[[num.layers+1]] = matrix(,nrow=n.layers[num.layers+1],ncol=n.layers[num.layers+1])
  dropout.rates[[num.layers+1]] = list()
  W.out = matrix(rnorm(n.layers[num.layers+1]*1),nrow=n.layers[num.layers+1],ncol=1)
  b.out = rnorm(1)
  W.temp = b.temp = h.in = h.act = list()
  train.errs = test.errs = c()
  for(iter in 1:epochs){
    # sample data
    tmp = create.data(m, n.layers[1],test.size)
    x = tmp[[1]]; y = tmp[[2]]; x.test = tmp[[3]]; y.test = tmp[[4]]
    err.total=0
    for(i in 1:nrow(x)){
      
######################### Create our dropout matrices #####################
      
      for (k in 1:length(drop.list)){
        drop.vec = 0
        while(sum(drop.vec)==0){
          drop.vec = rbinom(n.layers[k],1,p[k])
        }
        dropout.rates[[k]] = c(dropout.rates[[k]],mean(drop.vec))
        drop.list[[k]]=diag(n.layers[k])[,as.logical(drop.vec)]
      }
      for (k in 1:num.layers){
        W.temp[[k]] = t(drop.list[[k+1]])%*%W.list[[k]]%*%drop.list[[k]]
        b.temp[[k]] = t(drop.list[[k+1]])%*%b.list[[k]]
      }
      W.out.temp = t(drop.list[[num.layers+1]])%*%W.out
      
############################# Forward Propogation #########################
      
      x.in = t(drop.list[[1]])%*%x[i,]
      h.in[[1]] = W.temp[[1]]%*%x.in+b.temp[[1]]
      h.act[[1]] = sigmoid(h.in[[1]])
      # print(dim(h.act[[1]]))
      if (num.layers>1){
        for (j in 2:num.layers){
          h.in[[j]] = W.temp[[j]]%*%h.act[[j-1]]+b.temp[[j]]
          h.act[[j]] = sigmoid(h.in[[j]])
        }
      }
      out.in = sum(W.out.temp*h.act[[num.layers]]) + b.out
      out.act = sigmoid(out.in)

############################## Back Propogation ############################      
      
      error = .5*sum(y[i]-out.act)^2
      d_out = (y[i]-out.act)*(out.act)*(1-out.act)# means d_error/d_out.in
      d_W.out = d_out*h.act[[num.layers]]
      d_b.out = d_out
      # d_in = rep(list(NA),num.layers)
      # d_in[[num.layers]]
      d_act = rep(list(NA),num.layers)
      d_act[[num.layers]] <- (y[i]-out.act)*(out.act)*(1-out.act)*W.out.temp
      d_W = rep(list(NA),num.layers)
      if(num.layers>1){
        d_W[[num.layers]] <- (d_act[[num.layers]]*h.act[[num.layers]]*(1-h.act[[num.layers]]))%*%t(h.act[[num.layers-1]])
      } else {
        d_W[[num.layers]] <- (d_act[[num.layers]]*h.act[[num.layers]]*(1-h.act[[num.layers]]))%*%x.in
      }
      
      if (num.layers>1){
        for (j in (num.layers-1):1){
          d_act[[j]]=t(W.temp[[j+1]])%*%(d_act[[j+1]]*(h.act[[j+1]])*(1-h.act[[j+1]]))
          if(j!=1){
            d_W[[j]]=(d_act[[j]]*h.act[[j]]*(1-h.act[[j]]))%*%t(h.act[[j-1]])
          }else{
            d_W[[j]]=(d_act[[j]]*h.act[[j]]*(1-h.act[[j]]))%*%matrix(x.in,nrow=1)
          }
        }
      }
      
######################## Update W's and b's ############################
      
      W.out = W.out + lr*drop.list[[num.layers+1]]%*%d_W.out
      b.out = b.out + lr.b*d_b.out
      # print(i)
      # print(iter)
      # print(lapply(drop.list,dim))
      # print(lapply(d_W,dim))
      for(j in 1:num.layers){
        W.list[[j]] = W.list[[j]] + lr*drop.list[[j+1]]%*%d_W[[j]]%*%t(drop.list[[j]])
        b.list[[j]] = b.list[[j]] + lr.b*drop.list[[j+1]]%*%(d_act[[j]]*h.act[[j]]*(1-h.act[[j]]))
      }
      err.total = err.total+error
    }
    
####################### Calculate test error ###########################
    W.scaled = b.scaled = list()
    sample.p = 0
    if(sample.p == 1){
      p.s = sapply(dropout.rates,function(xx) mean(xx[[1]]))
      for (jj in 1:num.layers){
        W.scaled[[jj]] = W.list[[jj]]*p.s[jj]*p.s[jj+1]
        b.scaled[[jj]] = b.list[[jj]]*p.s[jj+1]
      }
      W.out.scaled = W.out*p.s[num.layers+1]
    }else{
      for (jj in 1:num.layers){
        W.scaled[[jj]] = W.list[[jj]]*p[jj]*p[jj+1]
        b.scaled[[jj]] = b.list[[jj]]*p[jj+1]
      }
      W.out.scaled = W.out*p[num.layers+1]
    }
    
    h.in[[1]] = W.scaled[[1]]%*%t(x.test)+t(matrix(b.scaled[[1]],nrow=length(y.test),ncol=length(b.scaled[[1]]),byrow=TRUE))
    h.act[[1]] = sigmoid(h.in[[1]])
    if(num.layers>1){
      for (j in 2:num.layers){
        h.in[[j]] = W.scaled[[j]]%*%h.act[[j-1]]+t(matrix(b.scaled[[j]],nrow=length(y.test),ncol=length(b.scaled[[j]]),byrow=TRUE))
        h.act[[j]] = sigmoid(h.in[[j]])
      }
    }
    out.in = t(W.out.scaled)%*%h.act[[num.layers]] + b.out
    out.act = sigmoid(out.in)
    mean.test.error = .5*sum((out.act-y.test)^2)/length(y.test)
    test.errs = c(test.errs,mean.test.error)
    train.errs = c(train.errs,err.total/nrow(x))
  }
  dropout.rates = sapply(dropout.rates,function(xx) mean(xx[[1]]))
  results = list(test.errs, train.errs, W.scaled, W.out.scaled, b.scaled, b.out, dropout.rates)
  names(results) = c('test.errs', 'train.errs', 'W.list', 'W.out', 'b.list', 'b.out', 'dropout.rates')
  return(results)
}

run.NN.mult.layers <- function(m,n.layers,epochs,lr=.01,lr.b=.01,lr.decay=0,
                               test.size = NULL, max.norm=NULL,resample=FALSE){
  if(is.null(test.size)){test.size=round(m/4)}
  if(!resample){
    tmp = create.data(m, n.layers[1],test.size)
    x = tmp[[1]]; y = tmp[[2]]; x.test = tmp[[3]]; y.test = tmp[[4]]
  }
  num.layers = max(length(n.layers)-1,1)
  if (length(n.layers)==1){n.layers = c(n.layers,n.layers)}
  # initialize parameters
  W.list = list()
  b.list = list()
  for(i in 1:num.layers){
    W.list[[i]] = matrix(rnorm(n.layers[i+1]*n.layers[i]),
                         nrow=n.layers[i+1],ncol=n.layers[i])
    b.list[[i]] = rnorm(n.layers[i+1])
  }
  W.out = matrix(rnorm(n.layers[num.layers+1]*1),nrow=n.layers[num.layers+1],ncol=1)
  b.out = rnorm(1)
  h.in = list()
  h.act = list()
  train.errs = c()
  test.errs = c()
  for(iter in 1:epochs){
    # train the model
    err.total=0
    # resample data
    if (resample){
      tmp = create.data(m, n.layers[1],test.size)
      x = tmp[[1]]; y = tmp[[2]]; x.test = tmp[[3]]; y.test = tmp[[4]]
    }
    for(i in 1:nrow(x)){
      ############################# Forward Propogation #########################
      h.in[[1]] = W.list[[1]]%*%x[i,]+b.list[[1]]
      h.act[[1]] = sigmoid(h.in[[1]])
      # print(dim(h.act[[1]]))
      if (num.layers>1){
        for (j in 2:num.layers){
          h.in[[j]] = W.list[[j]]%*%h.act[[j-1]]+b.list[[j]]
          h.act[[j]] = sigmoid(h.in[[j]])
        }
      }
      out.in = sum(W.out*h.act[[num.layers]]) + b.out
      out.act = sigmoid(out.in)
      
      ############################## Back Propogation ############################      
      error = .5*sum(y[i]-out.act)^2
      d_out = (y[i]-out.act)*(out.act)*(1-out.act)# means d_error/d_out.in
      d_W.out = d_out*h.act[[num.layers]]
      d_b.out = d_out
      # d_in = rep(list(NA),num.layers)
      # d_in[[num.layers]]
      d_act = rep(list(NA),num.layers)
      d_act[[num.layers]] <- (y[i]-out.act)*(out.act)*(1-out.act)*W.out
      d_W = rep(list(NA),num.layers)
      if(num.layers>1){
        d_W[[num.layers]] <- (d_act[[num.layers]]*h.act[[num.layers]]*(1-h.act[[num.layers]]))%*%t(h.act[[num.layers-1]])
      } else {
        d_W[[num.layers]] <- (d_act[[num.layers]]*h.act[[num.layers]]*(1-h.act[[num.layers]]))%*%x[i,]
      }
      
      if (num.layers>1){
        for (j in (num.layers-1):1){
          d_act[[j]]=t(W.list[[j+1]])%*%(d_act[[j+1]]*(h.act[[j+1]])*(1-h.act[[j+1]]))
          if(j!=1){
            d_W[[j]]=(d_act[[j]]*h.act[[j]]*(1-h.act[[j]]))%*%t(h.act[[j-1]])
          }else{
            d_W[[j]]=(d_act[[j]]*h.act[[j]]*(1-h.act[[j]]))%*%matrix(x[i,],nrow=1)
          }
        }
      }
      # d_b.1 = (y[i]-out.act)*(out.act)*(1-out.act)*W.out*h.act*(1-h.act)
      W.out = W.out + lr/(1+lr.decay*iter)*d_W.out
      b.out = b.out + lr.b/(1+lr.decay*iter)*d_b.out
      for(j in 1:num.layers){
        W.list[[j]] = W.list[[j]] + lr/(1+lr.decay*iter)*d_W[[j]]
        b.list[[j]] = b.list[[j]] + lr.b/(1+lr.decay*iter)*(d_act[[j]]*h.act[[j]]*(1-h.act[[j]])) 
      }
      err.total = err.total+error
      # Apply max-norm constraint
      if (!is.null(max.norm)){
        for(jj in 1:length(W.list)){
          for(kk in 1:nrow(W.list[[jj]])){
            if(sqrt(sum(W.list[[jj]][kk,]^2))>max.norm){
              W.list[[jj]][kk,] = W.list[[jj]][kk,]*max.norm/sqrt(sum(W.list[[jj]][kk,]^2))
            }
          }
        } # for (jj...)
        for(kk in 1:nrow(W.out)){
          if(sqrt(sum(W.out[kk,]^2))>max.norm){
            W.out[kk,] = W.out[kk,]*max.norm/sqrt(sum(W.out[kk,]^2))
          }
        }
      } # if (max.norm...)
    } # for (nrow(x)...)
    
    h.in[[1]] = W.list[[1]]%*%t(x.test)+t(matrix(b.list[[1]],nrow=length(y.test),ncol=length(b.list[[1]]),byrow=TRUE))
    h.act[[1]] = sigmoid(h.in[[1]])
    if(num.layers>1){
      for (j in 2:num.layers){
        h.in[[j]] = W.list[[j]]%*%h.act[[j-1]]+t(matrix(b.list[[j]],nrow=length(y.test),ncol=length(b.list[[j]]),byrow=TRUE))
        h.act[[j]] = sigmoid(h.in[[j]])
      }
    }
    out.in = t(W.out)%*%h.act[[num.layers]] + b.out
    out.act = sigmoid(out.in)
    mean.test.error = .5*sum((out.act-y.test)^2)/length(y.test)
    test.errs = c(test.errs,mean.test.error)
    train.errs = c(train.errs,err.total/nrow(x))
  }
  results = list(test.errs, train.errs, W.list, W.out, b.list, b.out)
  names(results) = c('test.errs', 'train.errs', 'W.list', 'W.out', 'b.list', 'b.out')
  return(results)
}

plot.res <- function(res,m,n,test.size){
  title = 'comparing training and test error of a simple NN'
  plot(1:length(res$train.errs),res$train.errs,main=title,ylim=c(0,max(c(res$test.errs,res$train.errs))),type='l') #ylim=c(min(c(errs,test.errs)),max(c(errs,test.errs)))
  points(1:length(res$test.errs),res$test.errs,col='red',type='l')
  
  # To compare to linear regression
  test.size=round(m/4)
  tmp = create.data(m, n,test.size)
  x = tmp[[1]]; y = tmp[[2]]; x.test = tmp[[3]]; y.test = tmp[[4]]
  test = data.frame(cbind(y,x))
  #colnames(test)[1] = c('y','X1','X2','X3','X4')
  lm.fit = lm(y~.,data=test)
  x.df = data.frame(x.test); colnames(x.df) = colnames(test)[-1]
  y.pred = predict(lm.fit,x.df)
  mean.test.error = .5/length(y.test)*sum((y.pred-y.test)^2)
  abline(mean.test.error,0,col='green')
  legend(x=epochs*.7,y=max(c(res$train.errs,res$test.errs,mean.test.error))/1.3,legend=c('NN train error','NN test error','linear regression'),
         col = c('black','red','green'),
         title='legend',lty=1)
}


### Call the NN
lr = 1
lr.b = lr
epochs=100
m = 300; # Number of samples
n.layers= c(4,5,3,4) # Input layer = first number, hidden layers = numbers after
if (epochs*m>100000){
  print('this could take a while...')
}

# res.1 = run.NN.with.dropout(m,n.layers,epochs,p=c(.8,.5,.5,.5),lr.decay=0)
# plot(1:epochs,res.1$test.errs,type='l',col='red')
set.seed(1)
res.2 = run.NN.mult.layers(m,n.layers,epochs,lr.decay=0,lr=.1)
plot(1:epochs,res.2$test.errs,type='l',col='blue')
res.3 = run.NN.mult.layers(m,n.layers,epochs,lr.decay=0.1,lr=1)
points(1:epochs,res.3$test.errs,type='l',col='red')
res.3 = run.NN.mult.layers(m,n.layers,epochs,lr.decay=0.3,lr=1)
points(1:epochs,res.3$test.errs,type='l',col='green')
res.3 = run.NN.mult.layers(m,n.layers,epochs,lr.decay=0.2,lr=1)
points(1:epochs,res.3$test.errs,type='l',col='black')

results = data.frame(matrix(,ncol=3))
colnames(results) = c('lr','lr.decay','min.test.error')
lr.vec = c(0,0.01,0.1,0.5,1,2)
lr.decay.vec = c(0,0.01,0.1,0.2)
iter=0
for(i in 1:length(lr.vec)){
  for(j in 1:length(lr.decay.vec)){
    iter = iter+1
    res = run.NN.mult.layers(m,n.layers,epochs,lr.decay=lr.decay.vec[j],lr=lr.vec[i])
    results[iter,1] = lr.vec[i]
    results[iter,2] = lr.decay.vec[j]
    results[iter,3] = min(res$test.errs)
  }
}

# set.seed(1)
# res.1 = run.NN.mult.layers(m,n.layers,epochs,max.norm=NULL)
# plot(1:epochs,res.1$test.errs,type='l',col='black')
# cols = c('blue','red','green','yellow','magenta')
# i=0
# for(max.N in c(1,2,3,4,100)){
#   set.seed(1)
#   i = i+1
#   res = run.NN.mult.layers(m,n.layers,epochs,max.norm=max.N)
#   points(1:epochs,res$test.errs,type='l',col=cols[i])
# }


# Learning rates don't seem to be much of an issue here, so not sure
# I need to do changing learning rate.

# set.seed(2)
# res.1 = run.NN.with.dropout.2(m,n.layers,epochs,p=c(.8,.5,.5,.5))
# set.seed(2)
# res.2 = run.NN.with.dropout(m,n.layers,epochs,p=c(.8,.5,.5,.5))
# plot(1:length(res.1$test.errs),res.1$train.errs,type='l',col='red')
# points(1:length(res.2$test.errs),res.1$train.errs,type='l',col='blue')
# 
# for(nn in intersect(names(res.1),names(res.2))){
#   test.1 = res.1[[which(names(res.1)==nn)]]
#   test.2 = res.2[[which(names(res.2)==nn)]]
#   print(nn)
#   print(identical(test.1,test.2))
# }


### Good. Now test the time
# prc <- proc.time()
# res.2 = run.NN.with.dropout(m=100,n.layers=4*n.layers,epochs,p=c(.8,.5,.5,.5))
# prc.2 <- proc.time()
# res.1 = run.NN.mult.layers(m=100,n.layers=4*n.layers,epochs)
# prc.3 <- proc.time()
# print('original dropout time: ')
# print(prc.2-prc)
# print('new dropout time: ')
# print(prc.3-prc.2)


# What - that's interesting. The OG dropout code is much faster. I didn't expect that.
# I am sure there is something in the code that shows that, but I don't care to check.
#run.NN.with.dropout(m,n.layers,epochs)
# Cool I got it to work! Now, for this task there isn't a ton of overfitting,
# so it's not that much better. Also I need to re-write this to be faster.
# I am doing way more matrix multiplications than I need to.
# run.NN.with.dropout(m,n.layers,epochs,p=0.6)
# run.NN.with.dropout(m,n.layers,epochs,p=c(0.1,0.4))
# run.NN.with.dropout(m,n.layers,epochs,p=c(0.8,0.6,0.5))
# run.NN.with.dropout(m,8,epochs,p=0.6)

#res = run.NN(m,n,epochs)
#res = run.NN.mult.layers(m,n.layers,epochs)

# #plot.res(res,m,n.layers[1],test.size=2500)
# 
# res.1 <- run.NN.mult.layers(m,n.layers=c(8,8),epochs)
# res.2 <- run.NN.mult.layers(m,n.layers=c(8,20),epochs)
# res.3 <- run.NN.mult.layers(m,n.layers=c(8,40),epochs)
#res.4 <- run.NN.mult.layers(m,n.layers=c(8,100),epochs)

# plot(res.1$test.errs,type='l',ylim=c(0,max(res.1$test.errs)))
# points(res.2$test.errs,type='l',col='red')
# points(res.3$test.errs,type='l',col='blue')
# points(res.4$test.errs,type='l',col='green')

# Interesting. It seems like even with an input of 8 nodes a wider hidden
# layer does better. I would expect a hidden layer of around 8 to be better.
# It would be more robust to run the alg multiple times and average results,
# but who has time for that?

############ Random comments

# Fixed - wow - interesting to note that with a huge sample size the NN
# performs really well (less than half the training error of LS regression)
# Granted, with that sample size lots of things could do well, but I made
# the equation really complex so that it couldn't be picked up by linear
# models. I wonder how a more complex model would fair.
#
# Next steps:
#   Make the code flexible so that it could take in multiple layers (of different sizes?)
#   Try different activation functions.
#   Try dropout (probs would need more complex structure or at least more nodes)

############

# Way better than the NN. Could the NN be way overfitting? Let's try this with more
# data and with a lower learning rate.


# is it problematic to shift b's with the same vars as W's? Should the b's
# be shifted differently/more slowly?

#par(mfrow=c(4,3))

# min.errors = data.frame(matrix(0,nrow=11,ncol=(cv.num+1)))
# colnames(min.errors)[1] = 'b mult'
# ii=0
# for(b.err in seq(0,1,by=0.1)){
#   ii = ii+1
#   min.errors[ii,1] = b.err
#let's cross-validate!
#cv.num=10
#for(j in 1:cv.num){
# create data
