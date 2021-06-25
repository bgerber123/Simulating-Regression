#Setup simple simulation without covariates for single-season, single species occupancy data
# Only has FALSE-NEGATIVES

library(unmarked)
rm(list=ls())

#How many sites are sampled
n.sites=100

#How many replicates/surveys per site
J=2

#Single detection probabilty for each site and survey
p=0.4

#Single occupancy probability for each site
psi=0.5

#Create a matrix to store probabilities that is n.sites by J
y=matrix(NA, nrow=n.sites,ncol=J)

#Start for loop to simulate 0 and 1's
for(i in 1:nrow(y)){
    set.seed(4341+i)
    z=rbinom(1,1,psi)
    y[i,]=rbinom(J,1,p*z)
}



#Fit the data using the generating model
umf <- unmarkedFrameOccu(y)

#fit the generating model
occ.model <- occu(~ 1 ~ 1, umf)
backTransform(occ.model,"state")@estimate
backTransform(occ.model,"det")@estimate

#True values
psi
p

###################################
#What are the unique combinations of detection histories
# 00    prob= psi*(1-p)^2+(1-psi) 
# 01    prob = psi*(1-p)*p
# 10    prob = psi*p*(1-p)
# 11    prob = psi*p^2


#Take the same data from above and convert it to the unique combinations of detection histories
det.hist=apply(y,1,paste,collapse=" ")
combn=table(det.hist)

#These are the frequencies of each detection history for this combination of psi and p and J and n.sites
combn

#Use these data to find the maximum likelihood estimates

#negative log-likelihood function. Want to minimize this, thus the negative value
likelihood=function(param,combn){
  psi=param[1]
  p=param[2]
  -1*(     combn[1]*log(psi*(1-p)^2+(1-psi))    +   combn[2]*log(psi*(1-p)*p)  +    combn[3]*log(psi*p*(1-p))   + combn[4]*log(psi*p^2))}

#Use optimization to minimze the likelihood function. 
#First output is psi and the second output is p
optim(par=c(0.3,0.2), fn=likelihood,combn=combn)$par

#Same as MLE values from unmarked

####################################################
####################################################
#Multinomial simulation based on detection history probabilities

#What are all the possible detection histories with J replicates

# 00
# 01
# 10
# 11

set.seed(4341)
y2=rmultinom(1,n.sites,prob=c(
                    psi*(1-p)^2+(1-psi),
                    psi*(1-p)*p,
                    psi*p*(1-p),
                    psi*p^2))


y2


