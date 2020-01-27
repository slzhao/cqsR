


require(ssanv)
library(MASS)
#Beta0: read count; Beta1: fold change

makeEqualDesignMatrix=function(factorNum=3,sampleSize=2^factorNum,interactionFactors=NULL) {
  varList=lapply(1:factorNum,function(x) c(0,1))
  names(varList)=paste0("Factor",1:factorNum)
  varListGrid=as.matrix(do.call(expand.grid,varList))

  repTime=sampleSize/2^factorNum
  if ((repTime%%1)!=0) { #not integer, sampleSize and factorNum can't be combined
    stop(paste0("sampleSize and factorNum can't be combined in balanced design"))
  } else {
    varListGridAll=NULL
    for (i in 1:repTime) {
      varListGridAll=rbind(varListGridAll,varListGrid)
    }
    if (is.null(interactionFactors)) {
      modelFormula=as.formula(paste0("~",paste(colnames(varListGridAll),collapse="+")))
    } else {
      interactionChracter=paste(sapply(interactionFactors,function(x) paste(colnames(varListGridAll)[x],collapse=":")),collapse="+")
      modelFormula=as.formula(paste0("~",paste(colnames(varListGridAll),collapse="+"),
                                     "+",interactionChracter)
                              )
    }
    designMatrix=model.matrix(modelFormula, as.data.frame(varListGridAll))
  }
  return(designMatrix)
}

betaToData=function(
                    #Beta0=log(5),Beta1=log(2),
                    Betas=c(log(5),log(2)),
                    phi=0.2,
                    d=log(1),
                    sampleSize=2,error=10^(-7),design.x=NULL,interactionFactors=NULL) {
  #interactionFactors need to be list, such as list(1:2,2:3) indicating interaction between factor 1,2 and 2,3

#  Tret<-gl(2,sampleSize/2,sampleSize)
#  Tret<-gl(2,1,sampleSize)

  #design.x<-model.matrix(~Tret)
  if (is.null(design.x)) {
    design.x=makeEqualDesignMatrix(factorNum=length(Betas)-1-length(interactionFactors),sampleSize=sampleSize,interactionFactors=interactionFactors)
  }


#  BetaMatrix<-matrix(c(Beta0, Beta1), 2, 1)
  BetaMatrix<-as.matrix(Betas)
  mu<-exp(design.x %*% BetaMatrix)+d
  max.mu<-max( mu )
  #J<-uniroot.integer( function(k) pnbinom(k, mu=max.mu, size=1/phi, lower.tail=F)-error,
  #                    interval=c(1, 999999), maxiter=1000,step.power = 12,print.steps=TRUE)$root
  J=qnbinom(error, mu=max.mu, size=1/phi,lower.tail = FALSE)

  my_dnb<-function(x, y) dnbinom(x=x, mu=y, size=1/phi)
  wij<-as.vector( outer(0:J, mu, my_dnb) )
  design.X<-design.x[rep(1:sampleSize, each=(J+1)), ]
  row.names(design.X)<-seq(1,sampleSize*(J+1))
  exemplary.data<-data.frame( y=rep(0:J, times=sampleSize), design.X[,-1,drop=FALSE], weight=wij )
  return(exemplary.data)
}

dataToV=function(exemplary.data,testFactor=colnames(exemplary.data)[2]) {
  outcome=colnames(exemplary.data)[1]
  variables1=colnames(exemplary.data)[-c(1,ncol(exemplary.data))]
  variables0=setdiff(variables1,testFactor)
  if (length(variables0)==0) {variables0="1"} #only one factor in design

  formula1=as.formula(paste0(outcome,"~",paste(variables1,collapse="+")))
  formula0=as.formula(paste0(outcome,"~",paste(variables0,collapse="+")))
  fit1<-glm.nb(formula1, weights=weight, data=exemplary.data)
  fit0<-glm.nb(formula0, weights=weight, data=exemplary.data)
  v<- -2*(logLik(fit0)[1]-logLik(fit1)[1])
  return(v)
}
vToPower=function(v,alpha=0.05) {
  powerLrt=pchisq( qchisq(1-alpha, df=1), df=1, ncp=v, lower.tail=F)
  return(powerLrt)
}
dataToPower=function(exemplary.data,testFactor=colnames(exemplary.data)[2],alpha=0.05) {
  v=dataToV(exemplary.data,testFactor=testFactor)
  powerLrt=vToPower(v,alpha=alpha)
  return(powerLrt)
}

#exemplary.data=betaToData(Beta0=1.61,Beta1=0.77,phi=0.25,sampleSize=30,error=10^(-7))
exemplary.data=betaToData(Betas=c(1.61,0.77),phi=0.25,sampleSize=30,error=10^(-7))
dataToPower(exemplary.data)
exemplary.data=betaToData(Betas=c(1.61,0.77),phi=0.25,sampleSize=26,error=10^(-7))
dataToPower(exemplary.data)
exemplary.data=betaToData(Betas=c(1.61,0.77),phi=0.25,sampleSize=24,error=10^(-7))
dataToPower(exemplary.data)

exemplary.data=betaToData(Betas=c(1.61,log(1.5)),phi=0.25,sampleSize=242,error=10^(-7))
#dataToPower(exemplary.data,alpha=alpha.star)

est_power_fdr=function(exemplary.data,testFactor=colnames(exemplary.data)[2],m=10000,fdr=0.01,m1=m*0.01,power.target=0.8) {
  m0<-m-m1
  r1<-ceiling(power.target*m1)
  alpha.star<-(r1*fdr)/(m0*(1-fdr))

  powerLrt=dataToPower(exemplary.data,testFactor=testFactor,alpha=alpha.star)
  return(powerLrt)
}
est_power_fdr(exemplary.data)

Betas=c(1.67,0.2,-1.61,1)
interactionFactors=list(1:2)
sampleSize=60
exemplary.data=betaToData(Betas=Betas,phi=0.25,sampleSize=sampleSize,error=10^(-7),interactionFactors=interactionFactors)
est_power_fdr(exemplary.data)
est_power_fdr(exemplary.data,testFactor=colnames(exemplary.data)[4])







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

