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

ANOVA_chis <- data.frame(matrix(nrow = length(ANOVA_levels), ncol = MC))
rownames(ANOVA_chis) <- paste(ANOVA_levels, "-level ANOVA")

for (i in 1:length(errors)){
  lin_chis <- NULL
  ANOVA_chis <- data.frame(matrix(nrow = length(ANOVA_levels), ncol = MC))
  rownames(ANOVA_chis) <- paste(ANOVA_levels, "-level ANOVA")
  for (j in 1:10){
    temp_x=runif(N,min=min_obs,max=max_obs)
    temp_y=(m*temp_x)+b
    # add some "noise" to y
    temp_y=temp_y+rnorm(N,mean=0,sd=errors[i])
    #Initializing a guess for the null model and running the optimization
    NullinitialGuess=c(m,errors[i])
    Nullfit=optim(par=NullinitialGuess,fn=Nullnllike,x=temp_x,y=temp_y)
    
    #Initializing a guess for parameter values and running the optimization
    initialGuess=c(1,1,1)
    fit=optim(par=initialGuess,fn=nllike,x=temp_x,y=temp_y)
    D = 2*(Nullfit$value-fit$value)
    if (D< 0){
      D= -1 
    }
    chi=pchisq(D,df=1,lower.tail=FALSE)
    lin_chis <- c(lin_chis,chi)
    
    for (k in 1:length(ANOVA_levels)){
        temp_x <- NULL
        for (l in 1:ANOVA_levels[k]){
          group_delta <- max_obs/ANOVA_levels[k]
          temp_ANOVA_x=runif(N/ANOVA_levels[k], min = min_obs+(group_delta*(l-1)), max = l*group_delta)
          temp_x <- c(temp_x, temp_ANOVA_x)
        }
        temp_y=(m*temp_x)+b
        temp_y=temp_y+rnorm(N,mean=0,sd=errors[i])
        
        group_x <- data.frame(matrix(nrow = N, ncol = 8))
        group_x[,] = 0
        null_group_x <- group_x
        for (n in 1:ANOVA_levels[k]-1){
          if(n>0){
          group_x[(((N/ANOVA_levels[k])*n)+1):((N/ANOVA_levels[k])*(n+1)),n] = 1
          }
        }
        initialGuess=c(rep(1,10))
        #initialGuess <- replace(initialGuess, 0:ANOVA_levels[k]+1,1 )
        Nullfit=optim(par=NullinitialGuess,fn=Nullnllike,x=null_group_x,y=temp_y)
        fit=optim(par=initialGuess,fn=nllikeANOVA_eight,x=group_x,y=temp_y)
        Dtest = -2*(Nullfit$value-fit$value)
        if (Dtest < 0){
          Dtest= -1 
        }
        chitest=pchisq(Dtest,df=ANOVA_levels[k]+1,lower.tail=FALSE)
        ANOVA_chis[k,j] <- chitest

    }
  }
  
  chis[1,i] <-mean(lin_chis)
  for (m in 1:length(ANOVA_levels)){
    if (identical(ANOVA_chis[m,length(ANOVA_chis)],NA)!=TRUE){
    chis[m+1,i] <- mean(as.numeric(ANOVA_chis[m,]))
    }
  }
}
