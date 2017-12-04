rm(list=ls())

#Load csv file
antibiotics = read.csv(file = "antibiotics.csv", header = TRUE, sep = ",")

#Modify data so it works with ANOVA test - we live in R^4 I think
#Model is a function f:R^3->R, so we need to take three vectors as inputs
#Thus data should be a 7 by 5 matrix?

#Loading control
data=data.frame(matrix(ncol=7,nrow=4))
colnames(data)=c("x","y","z","control","ab1","ab2","ab3")
for (i in 1:4){
data[i,4]=antibiotics[i,2]
}

#Loading ab1
for (i in 1:4){
  data[i,5]=antibiotics[i+4,2]
}

#Loading ab2
for (i in 1:4){
  data[i,6]=antibiotics[i+8,2]
}

#Loading ab3
for (i in 1:4){
  data[i,7]=antibiotics[i+12,2]
}


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
  sigma=exp(p[3])
  
  expected=B0+B1*x+B2*y+B3*z
  
  nll=-sum(dnorm(x=w, mean = expected, sd = sigma, log = TRUE))
  return((nll))}

#Asses max likelihood of model parameters with optim()
#   Create one function to fit a constant, and one to fit a linear model
# - create vector with initial guess for each model
#       -maybe look at data to guide guesses, because these models seem quite chaotic
# - minimization function with appropriate arguments - use optim - store each model seperately
NullGuess=c(2000,10)
Guess=c(2000,-1000,10)


#Implement the liklihood ratio test
#   pchisq(D,df=1)
#   D = 2times difference of negative log-likelihood - these two values come from the different models
