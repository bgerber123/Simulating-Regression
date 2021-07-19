# Simulation experiment to investigate occupancy estiates
# under two sample sizes of sites (10 and 100)


#load library
library(mvtnorm)
library(unmarked)

nsites=100
jsurveys=3
ydata=matrix(NA,nrow=nsites,ncol=jsurveys)
#p<-0.2
#psi<-0.5

#set alpha/beta parameters
a0<- 0.85
a1<- -2
a2<- 1
b0<- -0.85
b1<- -3
b2<- -1.5
######################################
#Create covariate- independent
IP=rnorm(nsites,0,1)

#Simulate covariate data with correlation for WD and veg den
means <- c(0,0)

Sigma=diag(2)
Sigma[2,1]=Sigma[1,2]=0.7
Sigma

covs=rmvnorm(nsites, mean=means,sigma=Sigma)
covs <- data.frame(covs)

colnames(covs) <- c("WD", "VegDen")
covs$IP <- IP

cor(covs)

######################################
#
lpsi<-a0+(covs$WD*a1)+(covs$IP*a2)
psi<-plogis(lpsi)
hist(psi)
plot(covs$WD, lpsi)
plot(covs$WD, psi)
plot(covs$IP, lpsi)
plot(covs$IP, psi)

lp=b0+(covs$WD*b1)+(covs$VegDen*b2)
p=plogis(lp)
plot(covs$WD,p)
hist(p)
pstar=1-(1-p)^jsurveys
hist(pstar)


#######################################
set.seed(54353+i)
z<-rbinom(n = nsites, size = 1, prob = psi)
hist(z*pstar)
plot(z,pstar)

#This is removing the unoccupied sites and then not plotting them
this=pstar*z
hist(this[this>0])

######################################
for(i in 1:nsites){
    ydata[i,]<-rbinom(n = jsurveys, size = 1, prob = p[i]*z[i])
}

umf <- unmarkedFrameOccu(ydata,siteCovs = covs)

#fit the generating model
occ.model <- occu(~WD+VegDen  ~WD+IP , umf)


