#Load csv file

#Plot data as a scatterplot with growth on the y axis and sugar on the x axis

#Make a custom function(params, observation)
# - unload parameters
# - Match parameters to null model - B0+error
# - dnorm(x,mean,sd)
# - repeat process for linear - B0+B1x+error

#Asses max likelihood of model parameters with optim()
#   Create one function to fit a constant, and one to fit a linear model
# - create vector with initial guess for each model
#       -maybe look at data to guide guesses, because these models seem quite chaotic
# - minimization function with appropriate arguments - use optim - store each model seperately

#Implement the liklihood ratio test
#   pchisq(D,df=1)
#   D = 2times difference of negative log-likelihood - these two values come from the different models

#Print the p-value
