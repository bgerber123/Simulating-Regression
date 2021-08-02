library(moveHMM)
library(circular)
#simulate 1 individual using 1 behavioral state based on turning angles
#and step lengths

stepPar <- c(20,1) # mean1, sd1, 
anglePar <- c(0,0.999) # mean1, k1,

#Distribution of steps
par(ask=FALSE)
hist(rgamma(1000,20,1))

#Distribution of turning angles
x=rwrappedcauchy(1000, mu = circular(0), rho = 0.9999)
hist(as.numeric(unlist(x)))

sim1=simData(nbAnimals = 1, nbStates = 1, stepDist = c("gamma"),
        angleDist = c("wrpcauchy"), stepPar = stepPar, anglePar = anglePar, 
        beta = NULL, covs = NULL,
        nbCovs = 0, zeroInflation = FALSE, obsPerAnimal = c(2000))

sim1
plot(sim1)

