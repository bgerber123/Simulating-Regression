# Simulation experiment to investigate occupancy estiates
# under two sample sizes of sites (10 and 100)


#load library
library(mvtnorm)
library(unmarked)

nsites=500
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
set.seed(908)
IP=rnorm(nsites,0,1)

#Simulate covariate data with correlation for WD and veg den
means <- c(0,0)

Sigma=diag(2)
Sigma[2,1]=Sigma[1,2]=0.0
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
set.seed(54353)
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


################################
# 2021-08-02
# Simulate a bunch of times

n.sim <- 1000
z.save <- vector("list", n.sim)
dat.save <- vector("list", n.sim)

for(i in 1:n.sim){
  set.seed(372658 + i)
  z <- rbinom(n = nsites, size = 1, prob = psi)
  for(j in 1:nsites){
    set.seed(372658 + i + j)
    ydata[j,]<-rbinom(n = jsurveys, size = 1, prob = p[j]*z[j])
  }
  z.save[[i]] <- z
  dat.save[[i]] <- ydata
}
#######
results.save=vector("list",n.sim)
results.mat=matrix(data=NA,nrow = 4,ncol =2)

for(i in 1:n.sim){
umf <- unmarkedFrameOccu(dat.save[[i]],siteCovs = covs)
occ.model <- occu(~WD+VegDen  ~WD+IP , umf,engine="C")#,method="SANN",control=list(maxit=20000))
                  #starts=c(a0,a1,a2,b0,b1,b2),control=list(maxit=2000))

occ.model@opt$convergence

mle.psi=occ.model@estimates@estimates$state@estimates[-1]
mle.p=occ.model@estimates@estimates$det@estimates[-1]
results.mat[1,1]=mle.psi[1]
results.mat[2,1]=mle.psi[2]
results.mat[3,1]=mle.p[1]
results.mat[4,1]=mle.p[2]

covmat=occ.model@estimates@estimates$state@covMat
SE.psi=sqrt(c(covmat[2,2],covmat[3,3]))

covmat=occ.model@estimates@estimates$det@covMat
SE.p=sqrt(c(covmat[2,2],covmat[3,3]))
UCIpsi<-mle.psi+SE.psi*1.96
LCIpsi<-mle.psi-SE.psi*1.96
UCIp<-mle.p+SE.p*1.96
LCIp<-mle.p-SE.p*1.96
ifelse(LCIpsi[1]<=a1 & a1<=UCIpsi[1], results.mat[1,2]<- 1,results.mat[1,2]<-0)
ifelse(LCIpsi[2]<=a2 & a2<=UCIpsi[2], results.mat[2,2]<- 1,results.mat[2,2]<-0)
ifelse(LCIp[1]<b1 & b1<=UCIp[1], results.mat[3,2]<- 1,results.mat[3,2]<-0)
ifelse(LCIp[2]<b2 & b2<=UCIp[2], results.mat[4,2]<- 1,results.mat[4,2]<-0)
results.save[[i]]<-results.mat
print(i)
}

out=lapply(results.save, FUN=function(x){
  which(is.na(x))
})

unlist(out)


out1=lapply(results.save, FUN=function(x){
  x[,1]
})

coefs=matrix(unlist(out1),ncol=4,byrow=TRUE)
dim(coefs)
head(coefs)

hist(coefs[,2])

index=which(coefs[,1]< -10)

coefs[index,1]

mean(coefs[,1])-a1

hist(coefs[-index,1],xlim=c(-10,5),breaks=50)

length(which(coefs[-index,1]<0))/length(coefs[-index,1])
