#Load csv file

#Modify data so it works with ANOVA test - 
#y vector is a vector with control, ab's
#x vector depends on what we wish to test?

#Make a custom function(params, observation)
# - unload parameters
# - Match parameters to ANOVA model - B0+B1x1+B2x2+B3x3+error
#control mean=B0, ab1 mean=B1+B0, ab2 mean=B2+B0, ab3 mean=B3+B0
# - dnorm(x,mean,sd)
# - repeat process for null model - B0+error

#Asses max likelihood of model parameters with optim()
#   Create one function to fit a constant, and one to fit a linear model
# - create vector with initial guess for each model
#       -maybe look at data to guide guesses, because these models seem quite chaotic
# - minimization function with appropriate arguments - use optim - store each model seperately

#Implement the liklihood ratio test
#   pchisq(D,df=1)
#   D = 2times difference of negative log-likelihood - these two values come from the different models

#Print the p-value