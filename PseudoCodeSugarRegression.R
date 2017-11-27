#Load csv file

#Plot data as a scatterplot with growth on the y axis and sugar on the x axis

#Make a custom function(params, observation)
# - unload parameters
# - Match parameters to linear model
# - dnorm(x,mean,sd)

#Asses max likelihood of model parameters with optim()
# - create vector with initial guess
#       -maybe look at data to guide guesses, because these models seem quite chaotic
# - minimization function with appropriate arguments

#Implement the liklihood ratio test
#   pchisq(D,df=1)
#   D = 2times difference of negative log-likelihood

#Print the p-value