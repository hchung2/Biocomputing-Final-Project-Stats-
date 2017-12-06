###attempt number 3 lmao

source("functions.R")
##Change variables up here to change experimental design 
{
  #Define different standard errors for our trials
  errors <- c(1,2,4,6,8,12,16,24)
  # number observations
  N=24
  #levels of ANOVAs to run
  ANOVA_levels <- c(2,4,8)
  #number of trials to run in "Monte Carlo" simulation
  MC <- 10
  #range of observations
  min_obs <- 0
  max_obs <- 50
  #define relationship of x and y paramaters
  m <- 0.4
  b <- 10
}

chis <- data.frame(matrix(nrow = 4, ncol = 8))
colnames(chis) <- errors
chis_rownames <- c("linear", "2-level", "4-level", "8-level")
rownames(chis) <- chis_rownames

for (i in 1:length(errors)){
  lin_chis <- NULL
  ANOVA2_chis <- NULL
  ANOVA4_chis <- NULL
  ANOVA8_chis <- NULL

  for (j in 1:10){
  temp_x=runif(N,min=min_obs,max=max_obs)
  temp_y=(m*temp_x)+b
  # add some "noise" to y
  temp_y=temp_y+rnorm(N,mean=0,sd=errors[i])
  #Initializing a guess for the null model and running the optimization
  NullinitialGuess=c(m,errors[i])
  Nullfit=optim(par=NullinitialGuess,fn=Nullnllike,y=temp_y)
  
  #Initializing a guess for parameter values and running the optimization
  initialGuess=c(1,1,1)
  fit=optim(par=initialGuess,fn=nllike,x=temp_x,y=temp_y)
  D = 2*(Nullfit$value-fit$value)
  if (D< 0){
    D= -1 
  }
  chi=pchisq(D,df=1,lower.tail=FALSE)
  lin_chis <- c(lin_chis,chi)
  
  ##2-Level ANOVA
  group_x <- c(rep(0,6), rep(1,6))

  temp_x <- NULL
  group_delta <- max_obs/2
  for (k in 1:2){
  temp_ANOVA_x=runif(N/2, min = min_obs*(k-1), max = k*group_delta)
  temp_x <- c(temp_x, temp_ANOVA_x)
  }
  temp_y=(m*temp_x)+b
  # add some "noise" to y
  temp_y=temp_y+rnorm(N,mean=0,sd=errors[i])
  
  initialGuess=c(1,1,1,1)
  fit=optim(par=initialGuess,fn=nllikeANOVA_two,x=group_x,y=temp_y)
  D2 = -2*(Nullfit$value-fit$value)
  chi2=pchisq(D2,df=3,lower.tail=FALSE)
  ANOVA2_chis <- c(ANOVA2_chis,chi2)
  
  
  
  #4-level ANOVA
  group_x <- data.frame(matrix(ncol = 3, nrow = 24))
  group_x[,] = 0
  group_x[7:12,1] = 1
  group_x[13:18,2] = 1
  group_x[19:24,3] = 1
  
  temp_x <- NULL
  group_delta <- max_obs/4
  for (l in 1:4){
  temp_ANOVA_x=runif(N/4, min = min_obs+(group_delta*(l-1)), max = l*group_delta)
  temp_x <- c(temp_x, temp_ANOVA_x)
  }
  temp_y=(m*temp_x)+b
  # add some "noise" to y
  temp_y=temp_y+rnorm(N,mean=0,sd=errors[i])
  
  initialGuess=c(1,1,1,1,1)
  fit=optim(par=initialGuess,fn=nllikeANOVA_four,x=group_x,y=temp_y)
  D4 = -2*(Nullfit$value-fit$value)
  chi4=pchisq(D4,df=5,lower.tail=FALSE)
  ANOVA4_chis <- c(ANOVA4_chis,chi4)
 
  
  #8-Level Anova
  temp_x <- NULL
  group_delta <- max_obs/8
  for (n in 1:8){
  temp_ANOVA_x=runif(N/8, min = min_obs+(group_delta*(n-1)), max = n*group_delta)
  temp_x <- c(temp_x, temp_ANOVA_x)
  }
  temp_y=(m*temp_x)+b
  # add some "noise" to y
  temp_y=temp_y+rnorm(N,mean=0,sd=errors[i])
  group_x <- data.frame(matrix(ncol = 7, nrow = 24))
  group_x[,] = 0
  group_x[4:6,1] = 1
  group_x[7:9,2] = 1
  group_x[10:12,3] = 1
  group_x[13:15,4] = 1
  group_x[16:18,5] = 1
  group_x[19:21,6] = 1
  group_x[22:24,7] = 1
  
  initialGuess=c(1,1,1,1,1,1,1,1,1)
  fit=optim(par=initialGuess,fn=nllikeANOVA_eight,x=group_x,y=temp_y)
  D8 = -2*(Nullfit$value-fit$value)
  chi8=pchisq(D8,df=9,lower.tail=FALSE)
  ANOVA8_chis <- c(ANOVA8_chis,chi8)
  }
  chis[1,i] <-mean(lin_chis)
  chis[2,i] <- mean(ANOVA2_chis)
  chis[3,i] <- mean(ANOVA4_chis)
  chis[4,i] <- mean(ANOVA8_chis)
}
