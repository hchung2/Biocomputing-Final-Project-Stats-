rm(list=ls())

#Load csv file
antibiotics = read.csv(file = "antibiotics.csv", header = TRUE, sep = ",")

#Setting up data - want x vectors with 1's and 0's, y vector with growth values
data1=antibiotics[which(antibiotics$trt=="control"),]
data1[,1]=0
ab10=antibiotics[which(antibiotics$trt=="ab1"),]
ab10[,1]=0
ab20=antibiotics[which(antibiotics$trt=="ab2"),]
ab20[,1]=0
ab30=antibiotics[which(antibiotics$trt=="ab3"),]
ab30[,1]=0
ab11=antibiotics[which(antibiotics$trt=="ab1"),]
ab11[,1]=1
ab21=antibiotics[which(antibiotics$trt=="ab2"),]
ab21[,1]=1
ab31=antibiotics[which(antibiotics$trt=="ab3"),]
ab31[,1]=1
#Combining Data
ab1val=rbind(data1,ab11,ab20,ab30)
ab2val=rbind(data1,ab10,ab21,ab30)
ab3val=rbind(data1,ab10,ab20,ab31)


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

#Testing ab1
NullGuess=c(12.64321,1)
Guess=c(12.64,-8.53,4.87679,-4.3,1)
Fit=optim(Guess, nllike, x=ab1val$trt, y=ab1val$growth)
NullFit=optim(NullGuess, Nullnllike, y=ab1val$growth)

#Implement the liklihood ratio test
#   pchisq(D,df=1)
#   D = 2times difference of negative log-likelihood - these two values come from the different models

#Test for ab1
A = 2*(Fit$value-NullFit$value)
ab1p=pchisq(q=A, df=1, lower.tail=FALSE)
