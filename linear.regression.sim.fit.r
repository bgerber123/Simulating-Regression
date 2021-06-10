############################################
#Objective: simulate linear regression data 
#           and fit the generating model 
###############################################
#load library
  library(mvtnorm)

#Coeficients, intercept and then slopes
  beta=c(10,-1,1,2)

#sample size for one set of data
  n=100

#define how much noise/error/variation to induce in the simulated data
  stdev=0.5
  
#Number of covariates relevant to the generating model
  n.par=length(beta)-1

#Correlation among variables??
  corr.no=TRUE
  #corr.no=FALSE

  Sigma=diag(n.par)

if(corr.no){
  set.seed(54541)
  covs=rmvnorm(n, mean=rep(0,n.par),sigma=Sigma)
}else{
  set.seed(54541)
  Sigma[lower.tri(Sigma)]=runif(length(which(lower.tri(Sigma))),min=0.5,max=0.95)
  Sigma=matrix(Matrix::forceSymmetric(Sigma,uplo="L"),ncol=n.par)
  covs=rmvnorm(n, mean=rep(0,n.par),sigma=Sigma)
}
  
#check correlation
  cor(covs)

#Create design matrix for generating data
  X=model.matrix(~covs)

#simulate one dataset  
  set.seed(5454)
  y=X%*%beta+rnorm(n,0,stdev)

  hist(y)
  
#Fit the simulated data with the generating model
  
  fit=lm(y~0+X)
  summary(fit)  

#compare estimats with true beta coeficients
  cbind(fit$coefficients,beta)

  #plot the data and predictions
  plot(y,predict(fit))
  abline(0,1,lwd=2,col=2)
  
############################################
#Objective: simulate linear regression data   
#           and fit not generating model with correlated data
###############################################
  #load library
  library(mvtnorm)
  
  #Coeficients, intercept and then slopes
  beta=c(10,-1,1,2)
  
  #sample size for one set of data
  n=100
  
  #define how much noise/error/variation to induce in the simulated data
  stdev=0.5
  
  #Number of covariates relevant to the generating model
  n.par=length(beta)-1
  
  #number of correlated covariates not relevant to the generating model
  n.par2=n.par
  
  #Simulate covariate data with correlation
  Sigma=diag(n.par+n.par2)
  
  #Let's keep the first n.par covariates without correlation and then
  #the next n.par2 set with high correlation
  Sigma[4,1]=Sigma[1,4]=0.9
  Sigma[5,2]=Sigma[2,5]=0.9
  Sigma[6,3]=Sigma[3,6]=0.9
  
  Sigma
  covs=rmvnorm(n, mean=rep(0,n.par+n.par2),sigma=Sigma)
  
  
  #Create design matrix for generating data- excludes correlated variable
  X=model.matrix(~covs[,1:n.par])
  
  #Create design matrix with only the correlated variable
  X2=model.matrix(~covs[,(n.par+1):(n.par+n.par2)])
  
    
  #simulate one dataset  
  set.seed(5454)
  y=X%*%beta+rnorm(n,0,stdev)
  
  hist(y)
  
  #Fit the simulated data with the generating model
  fit=lm(y~0+X)
  summary(fit)  

  #Fit the simulated data with the correlated variables
  fit2=lm(y~0+X2)
  summary(fit2)  
  
    
  #compare estimats with true beta coeficients
  cbind(fit$coefficients,fit2$coefficients,beta)
  
  #plot the data and predictions
  plot(y,predict(fit))
  points(y,predict(fit2),col=2)
  abline(0,1,lwd=2,col=4)
  
  
############################################
#Objective: simulate linear regression data   
#           with a categorical variable with multiple levels
###############################################
  
  #Coeficients, intercept and then offsets
  beta=c(10,-1,1)
  
  #sample size for one set of data
  n=100
  
  #define how much noise/error/variation to induce in the simulated data
  stdev=0.5
  
  #Number of covariates relevant to the generating model
  n.par=length(beta)-1
  
  covs <- sample(LETTERS[1:length(beta)], n, replace=TRUE, 
                  prob=c(0.33, 0.33, 0.33))
  covs=covs[order(covs)]
  
  #dummy coding - leads to offsets from the intercept
  X=model.matrix(~covs)
  
  #effect or deviation coding - leads to effect change from the grand mean
  X2=model.matrix(~covs,contrasts=list(covs=contr.sum))
  
  #simulate data using X
  set.seed(5454)
  y=X%*%beta+rnorm(n,0,stdev)
  
  
  #Fit the simulated data X
  fit=lm(y~0+X)
  summary(fit)  
  
  #Fit the simulated data with X2
  fit2=lm(y~0+X2)
  summary(fit2)  
  
  
  #compare estimats with true beta coeficients
  cbind(fit$coefficients,fit2$coefficients,beta)
  
  #plot the data and predictions
  plot(y,predict(fit))
  points(y,predict(fit2),col=2)

#INTEREPTING coefs using X - dummy coding  
  #The intercept represents A
  fit$coefficients
  #XcovsB is the difference in y from A
  #XcovsC is the difference in y from A
  
#INTEREPTING coefs using X2 - effect coding  
  #The intercept represents the grant mean across all categories
  fit2$coefficients
  #X2covs1 is the change in y from the grand mean of all groups with
  # the mean from A
  
  #Xcovs2 is the change in y from the grand mean of all groups with
  # the mean from B
  
#Equate the two types by way of predicting
  #prediction for group A
  fit$coefficients[1]
  fit2$coefficients[1]+fit2$coefficients[2]
  
  #  prediction for group B
  fit$coefficients[1]+fit$coefficients[2]
  fit2$coefficients[1]+fit2$coefficients[3]
  
  #  prediction for group C
  fit$coefficients[1]+fit$coefficients[3]
  fit2$coefficients[1]+abs(fit2$coefficients[2]+fit2$coefficients[3])
  
  