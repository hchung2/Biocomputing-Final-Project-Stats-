#Clear environment
rm(list=ls())
#Source file for functions
source("functions.R")

#create vectors for sd errors
errors <- c(1,2,4,6,8,12,16,24)


#create dataframe to store pvalues
pvalues <- data.frame(matrix(nrow = 2, ncol = 8))
colnames(pvalues) <- errors
rownames(pvalues) <- c("Linear","2-Level ANOVA", "4-Level ANOVA", "8-Level ANOVA")
for (i in 1:length(errors)){
  #Create vectors that will store values of each MC replicate and be used to calculate mean
  linear <- NULL
  ANOVA2 <- NULL
  ANOVA4 <- NULL
  ANOVA8 <- NULL
  for (j in 1:100){
    
    ####Linear
    #create x's
    x <- runif(24, min = 0, max = 50)
    
    #create y's with some random error
    y <- 0.4*x + 10
    y <- y + rnorm(24, sd=errors[i])
    #Null fit
    NullinitialGuess<-c(0.4,1)
    Nullfit=optim(par=NullinitialGuess,fn=Nullnllike,y=y)
    #fit
    initialGuess<-c(1,1,1)
    fit=optim(par=initialGuess, fn=nllike, x=x, y=y)
    D = abs(2*(Nullfit$value-fit$value))
    chi=pchisq(D,df=1,lower.tail=FALSE)
    linear <- c(linear,chi)
    
    
    ###2-Level ANOVA
    #create x's and y's
    x <- c(rep(10,12),rep(40,12))
    y <- 0.4*x + 10
    y <- y + rnorm(24,sd = errors[i])
    
    #create a vector called "groups" that will store 0 or 1 to represent whether these values are in a group (there is a treatment)
    groups <- c(rep(0,12), rep(1,12))
    
    Nullfit=optim(par=NullinitialGuess,fn=Nullnllike,y=y)
    
    initialGuess <- c(1,1,1)
    fit=optim(par=initialGuess, fn = nllikeANOVA_two, x = groups, y=y)
    D2 = abs(2*(Nullfit$value-fit$value))
    chi2=pchisq(D2,df=1,lower.tail=FALSE)
    ANOVA2 <- c(ANOVA2,chi2)
    
    
    ###4-Level ANOVA
    #x's and y's
    x <- c(rep(10,6),rep(20,6),rep(30,6), rep(40,6))
    y <- 0.4*x + 10
    y <- y + rnorm(24,sd = errors[i])
    
    #"is it in a group" dataframe.
    groups <- data.frame(matrix(ncol = 3, nrow = 24))
    groups[,] = 0
    groups[7:12,1] = 1
    groups[13:18,2] = 1
    groups[19:24,3] = 1
    
    #null fit
    Nullfit=optim(par=NullinitialGuess,fn=Nullnllike,y=y)
    
    #fit
    initialGuess <- c(10,4,8,12,1)
    fit=optim(par=initialGuess, fn = nllikeANOVA_four, x = as.matrix(groups), y=y)
    D4 = abs(2*(Nullfit$value-fit$value))
    chi4=pchisq(D4,df=3,lower.tail=FALSE)
    ANOVA4 <- c(ANOVA4,chi4)
    
    ###8-Level ANOVA
    #x's and y's
    x <- c(rep(1,3),rep(7,3),rep(14,3), rep(21,3),rep(28,3),rep(35,3),rep(42,3), rep(49,3))
    y <- 0.4*x + 10
    y <- y + rnorm(24,sd = errors[i])
    
    #group dataframe
    groups <- data.frame(matrix(ncol = 7, nrow = 24))
    groups[,] = 0
    groups[4:6,1] = 1
    groups[7:9,2] = 1
    groups[10:12,3] = 1
    groups[13:15,4] = 1
    groups[16:18,5] = 1
    groups[19:21,6] = 1
    groups[22:24,7] = 1
    
    #nullfit
    Nullfit=optim(par=NullinitialGuess,fn=Nullnllike,y=y)
    
    fit
    initialGuess <- c(10,4,8,12,16,20,24,28,1)
    fit=optim(par=initialGuess, fn = nllikeANOVA_eight, x = as.matrix(groups), y=y)
    D8 = abs(2*(Nullfit$value-fit$value))
    chi8=pchisq(D8,df=3,lower.tail=FALSE)
    ANOVA8 <- c(ANOVA8,chi8)
  }
  #take the mean of the vectors and store in appropriate pvalue element
  pvalues[1,i] <- mean(linear)
  pvalues[2,i] <- mean(ANOVA2)
  pvalues[3,i] <- mean(ANOVA4)
  pvalues[4,i] <- mean(ANOVA8)
  #plot(x,y, main = i)
}
#print results
print(pvalues)

#summary
print("Our results suggest that the larger standard deviation between our groups, the less likely it is that there is a statistical different between the numbers. The difference between #-level test is that lower level tests have a higher certainty at low standard deviations but higher uncertainty (lower pvalue) at high standard deviations.")