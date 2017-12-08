

#Defining the Null Negative Log Liklihood Function and fitting a constant model
Nullnllike<-function(p,x,y){
  B0=p[1]
  sigma=p[2]
  
  expected=B0
  
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}

#Defining the Negative Log Liklihood Function and fitting a linear model
nllike<-function(p,x,y){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  
  sigma=exp(p[3])
  
  expected=B0+(B1*x) 
  
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}

#Defining the Negative Log Liklihood Function and fitting a 2-level ANOVA (this is technically the same as linear model)
nllikeANOVA_two<-function(p,x,y){
  B0=p[1]
  B1=p[2]
  sigma=exp(p[3])
  
  expected=B0+(B1*x)
  
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}

#Defining the Negative Log Liklihood Function and fitting a 4-level ANOVA model
nllikeANOVA_four<-function(p,x,y){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  B3=p[4]
  
  sigma=exp(p[5])
  
  expected=B0 + (B1*x[,1]) + (B2*x[,2]) + (B3*x[,3])
  
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}

#Defining the Negative Log Liklihood Function and fitting a 8-level ANOVA model
nllikeANOVA_eight<-function(p,x,y){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  B3=p[4]
  B4=p[5]
  B5=p[6]
  B6=p[7]
  B7=p[8]
  sigma=exp(p[9])
  
  expected=B0+(B1*x[,1]) + (B2*x[,2]) + (B3*x[,3]) + (B4*x[,4]) + (B5*x[,5]) + (B6*x[,6]) + (B7*x[,7])
  
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}

