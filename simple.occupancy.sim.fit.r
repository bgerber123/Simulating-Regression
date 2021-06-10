############################################
#Objective: simulate single-season occuancy data
#           with only false-negatives. Use covariates
#           with site-level variation and opposite 
#           effects on detection and occupancy
###############################################

#load library
library(mvtnorm)
library(unmarked)

#OCCUPANCY Coeficients, intercept and then slopes
beta=c(2,3)

#DETECTION Coeficients, intercept and then slopes
alpha=c(-1,-2)

#How many sites are sampled
n.sites=100

#How many replicates/surveys per site
J=2

#Number of covariates relevant to the generating model
#This variable will apply to both occupancy and detection as
#a site-level covariate
n.par=length(beta)-1

#Simulate covariate values  
Sigma=diag(n.par)
set.seed(54541)
covs=rmvnorm(n.sites, mean=rep(0,n.par),sigma=Sigma)

#Create design matrix
X=model.matrix(~covs)

########################
#Get linear terms for occupancy probability
set.seed(15454)
linear.terms=X%*%beta
occ.prob=plogis(linear.terms)

#look at distrubtion of occupancy probabilites
hist(occ.prob)

#plot occupancy probability relationship
plot(covs,occ.prob,ylim=c(0,1))

#Simualte the true occurence at each site
Z=rbinom(n.sites,1,occ.prob)
Z
#A 0 indicates no occurence and a 1 indicates true occurence

#Now at each site the species occupies, see if the species is detected
# on each survey

#First determine site-level detection probabilties.
#If a site is not occupied, it can not be detected (thus *Z)
prob.det=plogis(X%*%alpha)*Z

#Now flip a coin J times for each site
detection.data=cbind(rbinom(n.sites,1,prob.det),
                     rbinom(n.sites,1,prob.det))

##############################################
#Let's ignore detection probability, combine the J surveys
#and fit a logistic regression model

#Combine surveys into y
y=apply(detection.data,1,sum)
y[which(y>0)]=1

#fit this naive data
fit=glm(y~0+X,family = binomial)

#plot predictions
plot(covs,predict(fit,type="response"),ylim=c(0,1))
#add in true occupancy probilities
points(covs,occ.prob,col=2)

######################################
######################################
#Use unmarked and fit simulated data with the generating model

#put data in unmarked framework
umf <- unmarkedFrameOccu(detection.data)

# add sitelevel covariates and call it sitevar1
siteCovs(umf) <- data.frame(sitevar1 = covs)

#fit the generating model
occ.model <- occu(~ sitevar1 ~ sitevar1, umf)

#Look at occupancy coefs compared to truth
cbind(coef(occ.model)[1:2],beta)

#Look at detection coefs compared to truth
cbind(coef(occ.model)[3:4],alpha)

#predict occupancy probs from the model
preds=predict(occ.model, type="state", newdata=data.frame(sitevar1=covs))
head(preds)

#plot occupany probs with CI
plot(covs,preds[,1])
points(covs,preds[,3],col=4)
points(covs,preds[,4],col=4)

#Add true probabilities
points(covs,occ.prob,col=2)

