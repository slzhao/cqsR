


require(ssanv)
#Beta0: read count; Beta1: fold change

designFactorList=list()
betaToData=function(Beta0=log(5),Beta1=log(2),phi=0.2,
                    #d=log(1),
                    sampleRep=2,error=1.9*10^(-7)) {
  NN=sampleRep*2
  Tret<-gl(2,sampleRep,sampleRep*2)
  design.x<-model.matrix(~Tret)
  Beta<-matrix(c(Beta0, Beta1), 2, 1)
  mu<-( exp(design.x %*% Beta))
  max.mu<-max( mu )
  #J<-uniroot.integer( function(k) pnbinom(k, mu=max.mu, size=1/phi, lower.tail=F)-error,
  #                    interval=c(1, 999999), maxiter=1000,step.power = 12,print.steps=TRUE)$root
  J=qnbinom(error, mu=max.mu, size=1/phi,lower.tail = FALSE)

  my_dnb<-function(x, y) dnbinom(x=x, mu=y, size=1/phi)
  wij<-as.vector( outer(0:J, mu, my_dnb) )
  design.X<-design.x[rep(1:NN, each=(J+1)), ]
  row.names(design.X)<-seq(1,NN*(J+1))
  exemplary.data<-data.frame( y=rep(0:J, times=NN), design.X[,-1,drop=FALSE], weight=wij )
  return(exemplary.data)
}

get.v<-function(mybeta, error=1.9*10^(-7)){
  NN<-4
  Tret<-gl(2,2,4)
  Type<-gl(2,1,4)
  design.x<-model.matrix(~Tret+Type)
  Beta0<-as.numeric(mybeta[1])
  Beta1<-as.numeric(mybeta[2])
  Beta2<-as.numeric(mybeta[3])
  phi<-as.numeric(mybeta[4])
  Beta<-matrix(c(Beta0, Beta1, Beta2), 3, 1)
  mu<-as.matrix( exp(design.x %*% Beta))
  max.mu<-max( mu )
  require(ssanv)
  J<-uniroot.integer( function(k) pnbinom(k, mu=max.mu, size=1/phi, lower.tail=F)-error,
                      interval=c(1, 999999), maxiter=1000,step.power = 12,print.steps=TRUE)$root
  my_dnb<-function(x, y) dnbinom(x=x, mu=y, size=1/phi)
  wij<-as.vector( outer(0:J, mu[1:dim(design.x)[1],], my_dnb) )
  design.X<-design.x[rep(1:NN, each=(J+1)), ]
  row.names(design.X)<-seq(1,NN*(J+1))
  exemplary.data<-data.frame( y=rep(0:J, times=NN), design.X[,-1], weight=wij )

  require(MASS)
  require(ssanv)
#  fit1<-glm.nb(y~Tret2+Type2,  weights=weight, data=exemplary.data)
#  fit0<-glm.nb(y~Type2, weights=weight, data=exemplary.data)
  fit1<-glm.nb(y~Tret2,  weights=weight, data=exemplary.data)
  fit0<-glm.nb(y~1, weights=weight, data=exemplary.data)
  v<- -2*(logLik(fit0)[1]-logLik(fit1)[1])
  return(v)
}

#aa<-ntext1()
mypower<-function(N, power.target=0.8, alpha, m=7002, fdr=0.01,aa=aa){
  m1<-length(aa)
  m0<-m-m1
  r1<-ceiling(power.target*m1)
  alpha.star<-(r1*fdr)/(m0*(1-fdr))
  power_LRT<-sum(pchisq( qchisq(1-alpha.star, df=1), df=1, ncp=aa*N, lower.tail=F))/m1
  return(power_LRT-power.target)
}
library(ssanv)
#N<-uniroot.integer( mypower, power.target=0.8, m=2000,aa=aa,fdr=0.01, interval=c(2, 1000), pos.side=T)$root
#N

