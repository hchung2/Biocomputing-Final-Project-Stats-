rm(list=ls())

#Load csv file
antibiotics = read.csv(file = "antibiotics.csv", header = TRUE, sep = ",")

#Setting up domain vectors for the antibiotic treatment
#View the negative null likelihood function as a function f:R^3->R
#Each vector has 1's where the corresponding treatment was effected; 0's otherwise
ab1xval=c(0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0)
ab2xval=c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0)
ab3xval=c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1)


#Make a custom function(params, observation)
# - unload parameters
# - Match parameters to ANOVA model - B0+B1x1+B2x2+B3x3+error
#control mean=B0, ab1 mean=B1+B0, ab2 mean=B2+B0, ab3 mean=B3+B0
# - dnorm(x,mean,sd)
# - repeat process for null model - B0+error
Nullnllike<-function(p,y){
  B0=p[1]
  sigma=exp(p[2])
  
  expected=B0
  
  nll=-sum(dnorm(x=y, mean=expected, sd=sigma, log = TRUE))
  return((nll))}

nllike<-function(p,x,y,z,w){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  B3=p[4]
  sigma=exp(p[5])
  
  expected=B0+B1*x+B2*y+B3*z
  
  nll=-sum(dnorm(x=w, mean = expected, sd = sigma, log = TRUE))
  return((nll))}

#Asses max likelihood of model parameters with optim()
#   Create one function to fit a null model, and one to fit a generalized linear model
# - create vector with initial guess for each model
#       -maybe look at data to guide guesses, because these models seem quite chaotic
# - minimization function with appropriate arguments - use optim - store each model seperately
NullGuess=c(12.64321,1)
Guess=c(12.64,-8.53,4.87679,-4.3,1)
Fit=optim(Guess, nllike, x=ab1xval,y=ab2xval,z=ab3xval, w=antibiotics$growth)
NullFit=optim(NullGuess, Nullnllike, y=antibiotics$growth)

#Implement the liklihood ratio test
#   pchisq(D,df=1)
#   D = 2times difference of negative log-likelihood - these two values come from the different models
D = -2*(Fit$value-NullFit$value)
pval=pchisq(q=D, df=3, lower.tail=FALSE)
