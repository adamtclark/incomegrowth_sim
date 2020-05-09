stockdat<-read.csv("stocks.csv")
bonddat<-read.csv("bonds.csv")
infldat<-read.csv("inflation.csv")
nyears_data<-min(c(nrow(stockdat), nrow(bonddat)))

niter<-1e3   #number of iterations
nyears<-50   #number of years to simulate
chunksize<-5 #number of years of data to group together

valueout<-matrix(nrow=niter, ncol=nyears)
fraction_stock<-0.5
withdrawal_rate<-0.04
rebalance<-TRUE    #rebalance investments every year?
fixed_rate<-FALSE  #withdraw a fixed fraction of principle?
selective_withdrawal<-FALSE #withdraw from best performing asset?

for(i in 1:niter) {
  #select time slices
  smp<-sample(nyears_data, ceiling(nyears/chunksize))
  ts<-numeric(ceiling(nyears/chunksize)*chunksize)
  for(j in 1:length(smp)) {
    ts[chunksize*(j-1)+1:chunksize]<-smp[j]:(smp[j]+chunksize-1)
  }
  ts[ts>nyears_data]<-ts[ts>nyears_data]-nyears_data
  
  startval<-c(fraction_stock, 1-fraction_stock) #stocks, bonds
  for(j in 1:nyears) {
    if(fixed_rate) {
      withdrawal<-withdrawal_rate
    } else {
      withdrawal<-startval*withdrawal_rate
    }
    
    if(startval[1]>0) {
      startval[1]<-startval[1]*(1+stockdat[ts[j],2]-infldat[ts[j],2])
    }
    if(startval[2]>0) {
      startval[2]<-startval[2]*(1+bonddat[ts[j],2]-infldat[ts[j],2])
    }
    
    if(selective_withdrawal) {
      whichuse<-which.max(c(stockdat[ts[j],2], bonddat[ts[j],2]))
      wdamount<-min(c(startval[whichuse], sum(withdrawal)))
      remainder<-sum(withdrawal)-wdamount
      
      startval[whichuse]<-startval[whichuse]-wdamount
      startval[-whichuse]<-startval[-whichuse]-remainder
    } else {
      startval<-startval-withdrawal
    }
    
    if(startval[1]<0) {
      startval[1]<-0
    }
    if(startval[2]<0) {
      startval[2]<-0
    }
    
    if(rebalance) {
      startval<-c(fraction_stock, 1-fraction_stock)*sum(startval)
    }
    
    valueout[i,j]<-sum(startval)
  }
  
  if(i/100 == floor(i/100)) {
    print(round(i/niter,2))
  }
}

par(mfrow=c(3,1), mar=c(4,4,2,2))
matplot(1:ncol(valueout), t(valueout+1), type="l", lty=1, xlab="year", ylab="value", col=adjustcolor(1, alpha.f = 0.1), log="y", axes=F)
axlst<-c(1,2,5)*10^rep(0:ceiling(log10(max(valueout))), each=3)
axis(1); axis(2, at=axlst+1, axlst, las=2); box()
abline(h=2, lty=2, lwd=1.5, col="red")
plot(1:ncol(valueout), apply(valueout, 2, function(x) mean(x<1)), xlab="year", ylab="pr[x<x0]", type="l")
abline(h=0, lty=3)
if(fixed_rate) {
  plot(1:ncol(valueout), apply(valueout, 2, function(x) mean(x==0)), xlab="year", ylab="pr[x==0]", type="l", axes=F)
  axis(1); axis(2, las=2); box()
  abline(h=0, lty=3)
} else {
  quantdat<-t(apply(valueout, 2, function(x) quantile(x*withdrawal_rate, c(0, 0.025, 0.5, 0.975))))
    
  matplot(1:ncol(valueout), 1e3*quantdat, xlab="year", ylab="min_with", type="l", lty=c(3, 2,1,2), col=1, log="y", ylim=c(1, max(1e3*quantdat)), axes=F)
  axis(1); axis(2, las=2); box()
  abline(h=c(1,2,5,10,20,50,100,200,500,1000), lty=1, col=adjustcolor("black", alpha.f = 0.2))
}  
