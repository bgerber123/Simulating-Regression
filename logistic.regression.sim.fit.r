############################################
#Objective: simulate logistic regression data 
#           and fit the generating model 
###############################################

#load library
  library(mvtnorm)

#Coeficients, intercept and then slopes
  beta=c(0,-1)

#sample size for one set of data
  n=100

#Number of covariates relevant to the generating model
  n.par=length(beta)-1

#Simulate covariate values  
  Sigma=diag(n.par)
  set.seed(54541)
  covs=rmvnorm(n, mean=rep(0,n.par),sigma=Sigma)

#Create design matrix for generating data
  X=model.matrix(~covs)

#simulate one dataset  
  set.seed(5454)
  linear.terms=X%*%beta

#Get probilities for each combination of linear term 
  prob=plogis(linear.terms)

#Simulate 0-1 data by flipping a weighted coin, where the weight
#is determined by each prob. Each row gets one coin flip to be a 0 or a 1

  y=rbinom(n,1,prob)
  
  y
    
#Fit the simulated data with the generating model
fit=glm(y~0+X,family="binomial")
summary(fit)  

#compare estimats with true beta coeficients
cbind(fit$coefficients,beta)

#plot the true probs and the predicted probs
plot(prob,predict(fit,type="response"))
abline(0,1,lwd=2,col=2)

#Plot probabilites in relation to the first covariate
plot(X[,2],predict(fit,type="response"))

#add in the true values
points(X[,2],prob,col=2)

