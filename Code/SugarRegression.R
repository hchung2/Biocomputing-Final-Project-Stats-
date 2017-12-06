#Clear out old values
rm(list=ls())

#Load packages
library(ggplot2)

#Load data
results = read.csv(file = "sugar.csv", header = TRUE, sep = ",")

#Graphing Data
d = ggplot(data=results,aes(x=sugar,y=growth))+geom_point() + coord_cartesian()+ xlab("Sugar")+ylab("Growth")
d

#Defining the Null Negative Log Liklihood Function and fitting a constant model
Nullnllike<-function(p,x,y){
  B0=p[1]
  sigma=p[2]
  
  expected=B0
  
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}

#Initializing a guess for the null model and running the optimization
NullinitialGuess=c(15,1)
Nullfit=optim(par=NullinitialGuess,fn=Nullnllike,x=results$sugar,y=results$growth)


#Defining the Negative Log Liklihood Function and fitting a linear model
nllike<-function(p,x,y){
  B0=p[1]
  B1=p[2]
  sigma=p[3]
  
  expected=B0+B1*x
  
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}

#Initializing a guess for parameter values and running the optimization
initialGuess=c(1,1,1)
fit=optim(par=initialGuess,fn=nllike,x=results$sugar,y=results$growth)

D = 2*(Nullfit$value-fit$value)

chi=pchisq(D,df=1,lower.tail=FALSE)

#Graphing Data with a regression line
d = ggplot(data=results,aes(x=sugar,y=growth))+geom_point() + geom_smooth(method=lm,se=FALSE) + coord_cartesian()+ xlab("Sugar")+ylab("Growth")
d
