### PARASOL UV-B HIERARCHICAL MODELING ♥ 

install.packages("coda")
install.packages("rjags")
library(coda)
library(rjags)

###################################### FULL DATASET #######################################################
setwd() # Set working directory
sylvestris <- read.csv("EurMeanPCA.csv", header=TRUE, sep = ";")
head(sylvestris)
summary(sylvestris)


ratio <- as.numeric(sylvestris$ratio)
n <- nrow(sylvestris) 
pCount <- as.numeric(sylvestris$nGrains)



sink("europemodel.txt")
cat("
    model{
    # Priors
    
    alpha ~ dnorm(0, 0.001)	              # Intercept
    beta ~ dnorm(0, 0.001)								# Slope
    
    shape ~ dunif(0, 250)                 # Shape to describe the variance of random variable in linear predictor
    shape.gc.m ~ dnorm(180, 1/ (30*30))		# Informed prior from previous laboratory analyses
    tau.grain <- 1/(0.5 * 0.5)						# Informed prior denoting the precision for the count model
    
    for (i in 1:n){
    
    # nGrains model
    nGrains[i] ~ dnorm(pCount[i], tau.grain )
    
    # Expectation/ process model
    mu[i] <- alpha + beta * uv[i]

    # The random variable
    mu.ratio[i] ~ dgamma(shape, shape/ exp(mu[i])) 
    
    # Multiply by the number of grains
    ratio.nGrains[i] <- mu.ratio[i] * round(nGrains[i]) 
    
    # Feed process model into machine uncertainty model
    ratio[i] ~ dgamma(shape.gc.m, shape.gc.m / ratio.nGrains[i])		
    }	

}

    ", fill = TRUE)
sink()



#One week
uv <- as.numeric(sylvestris$w1)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape") 

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesONEw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)

gelman.diag(paraSamplesONEw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesONEw)
plot(paraSamplesONEw)

s1 <- c(paraSamplesONEw[[1]][,2], paraSamplesONEw[[2]][,2], paraSamplesONEw[[3]][,2])
credInt1 <- quantile(s1, probs = c(0.09))  #Find where CRI cross zero
credInt1

dicSamplesONEw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesONEw, FUN = mean)


#Two weeks
uv <- as.numeric(sylvestris$w2)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesTWOw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesTWOw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesTWOw)
plot(paraSamplesTWOw)

s2 <- c(paraSamplesTWOw[[1]][,2], paraSamplesTWOw[[2]][,2], paraSamplesTWOw[[3]][,2])
credInt2 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt2

dicSamplesTWOw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesTWOw, FUN = mean)


#THREE weeks
uv <- as.numeric(sylvestris$w3)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesTHREEw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesTHREEw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesTHREEw)
plot(paraSamplesTHREEw)

s3 <- c(paraSamplesTHREEw[[1]][,2], paraSamplesTHREEw[[2]][,2], paraSamplesTHREEw[[3]][,2])
credInt3 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt3

dicSamplesTHREEw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesTHREEw, FUN = mean)


#FOUR weeks
uv <- as.numeric(sylvestris$w4)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesFOURw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesFOURw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesFOURw)
plot(paraSamplesFOURw)

s4 <- c(paraSamplesFOURw[[1]][,2], paraSamplesFOURw[[2]][,2], paraSamplesFOURw[[3]][,2])
credInt4 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt4

dicSamplesFOURw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesFOURw, FUN = mean)


#FIVE weeks
uv <- as.numeric(sylvestris$w5)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesFIVEw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesFIVEw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesFIVEw)
plot(paraSamplesFIVEw)

s5 <- c(paraSamplesFIVEw[[1]][,2], paraSamplesFIVEw[[2]][,2], paraSamplesFIVEw[[3]][,2])
credInt5 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt5

dicSamplesFIVEw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesFIVEw, FUN = mean)


#SIX weeks
uv <- as.numeric(sylvestris$w6)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesSIXw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesSIXw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesSIXw)
plot(paraSamplesSIXw)

s6 <- c(paraSamplesSIXw[[1]][,2], paraSamplesSIXw[[2]][,2], paraSamplesSIXw[[3]][,2])
credInt6 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt6

dicSamplesSIXw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesSIXw, FUN = mean)


#SEVEN weeks
uv <- as.numeric(sylvestris$w7)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesSEVENw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesSEVENw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesSEVENw)
plot(paraSamplesSEVENw)

s7 <- c(paraSamplesSEVENw[[1]][,2], paraSamplesSEVENw[[2]][,2], paraSamplesSEVENw[[3]][,2])
credInt7 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt7

dicSamplesSEVENw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesSEVENw, FUN = mean)


#EIGHT weeks
uv <- as.numeric(sylvestris$w8)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesEIGHTw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesEIGHTw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesEIGHTw)
plot(paraSamplesEIGHTw)

s8 <- c(paraSamplesEIGHTw[[1]][,2], paraSamplesEIGHTw[[2]][,2], paraSamplesEIGHTw[[3]][,2])
credInt8 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt8

dicSamplesEIGHTw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesEIGHTw, FUN = mean)


#NINE weeks
uv <- as.numeric(sylvestris$w9)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesNINEw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesNINEw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesNINEw)
plot(paraSamplesNINEw)

s9 <- c(paraSamplesNINEw[[1]][,2], paraSamplesNINEw[[2]][,2], paraSamplesNINEw[[3]][,2])
credInt9 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt9

dicSamplesNINEw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesNINEw, FUN = mean)


#TEN weeks
uv <- as.numeric(sylvestris$w10)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesTENw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesTENw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesTENw)
plot(paraSamplesTENw)

s10 <- c(paraSamplesTENw[[1]][,2], paraSamplesTENw[[2]][,2], paraSamplesTENw[[3]][,2])
credInt10 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt10

dicSamplesTENw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesTENw, FUN = mean)


#ELEVEN weeks
uv <- as.numeric(sylvestris$w11)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesELEVENw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesELEVENw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesELEVENw)
plot(paraSamplesELEVENw)

s11 <- c(paraSamplesELEVENw[[1]][,2], paraSamplesELEVENw[[2]][,2], paraSamplesELEVENw[[3]][,2])
credInt11 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt11

dicSamplesELEVENw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesELEVENw, FUN = mean)

#TWELVE weeks
uv <- as.numeric(sylvestris$w12)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesTWELVEw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesTWELVEw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesTWELVEw)
plot(paraSamplesTWELVEw)

s12 <- c(paraSamplesTWELVEw[[1]][,2], paraSamplesTWELVEw[[2]][,2], paraSamplesTWELVEw[[3]][,2])
credInt12 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt12

dicSamplesTWELVEw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesTWELVEw, FUN = mean)


#Growing season
uv <- as.numeric(sylvestris$gs_mean)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesGSw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesGSw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesGSw)
plot(paraSamplesGSw)

sGS <- c(paraSamplesGSw[[1]][,2], paraSamplesGSw[[2]][,2], paraSamplesGSw[[3]][,2])
credIntGS <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credIntGS

dicSamplesGSw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesGSw, FUN = mean)


#Annual Climatology
uv <- as.numeric(sylvestris$year)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesCLIMw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesCLIMw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesCLIMw)
plot(paraSamplesCLIMw)

sCLIM <- c(paraSamplesCLIMw[[1]][,2], paraSamplesCLIMw[[2]][,2], paraSamplesCLIMw[[3]][,2])
credIntCLIM <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credIntCLIM

dicSamplesCLIMw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesCLIMw, FUN = mean)

#Latitude
uv <- as.numeric(sylvestris$lat)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesLATw <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesLATw,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesLATw)
plot(paraSamplesLATw)

sLAT <- c(paraSamplesLATw[[1]][,2], paraSamplesLATw[[2]][,2], paraSamplesLATw[[3]][,2])
credIntLAT <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credIntLAT

dicSamplesLATw<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesLATw, FUN = mean)


##############################################################################################################
###################################### REDUCED DATASET #######################################################
setwd() # Set working directory

sylvestris <- read.csv("EurMeanPCAred.csv", header=TRUE, sep = ";") #New dataset without Benmore and Edinburgh
head(sylvestris)
summary(sylvestris)


ratio <- as.numeric(sylvestris$ratio)
n <- nrow(sylvestris) 
pCount <- as.numeric(sylvestris$nGrains)



sink("europemodel.txt")
cat("
    model{
    # Priors
    
    alpha ~ dnorm(0, 0.001)	              # intercept
    beta ~ dnorm(0, 0.001)								# slope
    
    shape ~ dunif(0, 250)                 # Shape to describe the variance of random variable in linear predictor
    shape.gc.m ~ dnorm(180, 1/ (30*30))		# Informed prior from previous laboratory analyses
    tau.grain <- 1/(0.5 * 0.5)						# Informed prior denoting the precision for the count model. Where did you get the data?
    
    for (i in 1:n){
    
    # nGrains model
    nGrains[i] ~ dnorm(pCount[i], tau.grain )
    
    # Expectation/ process model
    mu[i] <- alpha + beta * uv[i]

    # The random variable    
    mu.ratio[i] ~ dgamma(shape, shape/ exp(mu[i]))
    
    # Multiply by the number of grains
    ratio.nGrains[i] <- mu.ratio[i] * round(nGrains[i]) 
    
    # Feed process model into machine uncertainty model
    ratio[i] ~ dgamma(shape.gc.m, shape.gc.m / ratio.nGrains[i])		
    }	
}

    
    ", fill = TRUE)
sink()



#One week
uv <- as.numeric(sylvestris$w1)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesONEwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)

gelman.diag(paraSamplesONEwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesONEwo)
plot(paraSamplesONEwo)

s1 <- c(paraSamplesONEwo[[1]][,2], paraSamplesONEwo[[2]][,2], paraSamplesONEwo[[3]][,2])
credInt1 <- quantile(s1, probs = c(0.09))  #Find where CRI cross zero
credInt1

dicSamplesONEwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesONEwo, FUN = mean)


#Two weeks
uv <- as.numeric(sylvestris$w2)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesTWOwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesTWOwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesTWOwo)
plot(paraSamplesTWOwo)

s2 <- c(paraSamplesTWOwo[[1]][,2], paraSamplesTWOwo[[2]][,2], paraSamplesTWOwo[[3]][,2])
credInt2 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt2

dicSamplesTWOwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesTWOwo, FUN = mean)


#THREE weeks
uv <- as.numeric(sylvestris$w3)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesTHREEwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesTHREEwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesTHREEwo)
plot(paraSamplesTHREEwo)

s3 <- c(paraSamplesTHREEwo[[1]][,2], paraSamplesTHREEwo[[2]][,2], paraSamplesTHREEwo[[3]][,2])
credInt3 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt3

dicSamplesTHREEwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesTHREEwo, FUN = mean)


#FOUR weeks
uv <- as.numeric(sylvestris$w4)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesFOURwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesFOURwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesFOURwo)
plot(paraSamplesFOURwo)

s4 <- c(paraSamplesFOURwo[[1]][,2], paraSamplesFOURwo[[2]][,2], paraSamplesFOURwo[[3]][,2])
credInt4 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt4

dicSamplesFOURwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesFOURwo, FUN = mean)


#FIVE weeks
uv <- as.numeric(sylvestris$w5)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesFIVEwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesFIVEwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesFIVEwo)
plot(paraSamplesFIVEwo)

s5 <- c(paraSamplesFIVEwo[[1]][,2], paraSamplesFIVEwo[[2]][,2], paraSamplesFIVEwo[[3]][,2])
credInt5 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt5

dicSamplesFIVEwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesFIVEwo, FUN = mean)


#SIX weeks
uv <- as.numeric(sylvestris$w6)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesSIXwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesSIXwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesSIXwo)
plot(paraSamplesSIXwo)

s6 <- c(paraSamplesSIXwo[[1]][,2], paraSamplesSIXwo[[2]][,2], paraSamplesSIXwo[[3]][,2])
credInt6 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt6

dicSamplesSIXwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesSIXwo, FUN = mean)


#SEVEN weeks
uv <- as.numeric(sylvestris$w7)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesSEVENwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesSEVENwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesSEVENwo)
plot(paraSamplesSEVENwo)

s7 <- c(paraSamplesSEVENwo[[1]][,2], paraSamplesSEVENwo[[2]][,2], paraSamplesSEVENwo[[3]][,2])
credInt7 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt7

dicSamplesSEVENwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesSEVENwo, FUN = mean)


#EIGHT weeks
uv <- as.numeric(sylvestris$w8)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesEIGHTwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesEIGHTwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesEIGHTwo)
plot(paraSamplesEIGHTwo)

s8 <- c(paraSamplesEIGHTwo[[1]][,2], paraSamplesEIGHTwo[[2]][,2], paraSamplesEIGHTwo[[3]][,2])
credInt8 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt8

dicSamplesEIGHTwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesEIGHTwo, FUN = mean)


#NINE weeks
uv <- as.numeric(sylvestris$w9)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesNINEwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesNINEwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesNINEwo)
plot(paraSamplesNINEwo)

s9 <- c(paraSamplesNINEwo[[1]][,2], paraSamplesNINEwo[[2]][,2], paraSamplesNINEwo[[3]][,2])
credInt9 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt9

dicSamplesNINEwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesNINEwo, FUN = mean)


#TEN weeks
uv <- as.numeric(sylvestris$w10)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesTENwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesTENwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesTENwo)
plot(paraSamplesTENwo)

s10 <- c(paraSamplesTENwo[[1]][,2], paraSamplesTENwo[[2]][,2], paraSamplesTENwo[[3]][,2])
credInt10 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt10

dicSamplesTENwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesTENwo, FUN = mean)


#ELEVEN weeks
uv <- as.numeric(sylvestris$w11)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesELEVENwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesELEVENwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesELEVENwo)
plot(paraSamplesELEVENwo)

s11 <- c(paraSamplesELEVENwo[[1]][,2], paraSamplesELEVENwo[[2]][,2], paraSamplesELEVENwo[[3]][,2])
credInt11 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt11

dicSamplesELEVENwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesELEVENwo, FUN = mean)

#TWELVE weeks
uv <- as.numeric(sylvestris$w12)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesTWELVEwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesTWELVEwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesTWELVEwo)
plot(paraSamplesTWELVEwo)

s12 <- c(paraSamplesTWELVEwo[[1]][,2], paraSamplesTWELVEwo[[2]][,2], paraSamplesTWELVEwo[[3]][,2])
credInt12 <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credInt12

dicSamplesTWELVEwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesTWELVEwo, FUN = mean)


#Growing season
uv <- as.numeric(sylvestris$gs_mean)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesGSwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesGSwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesGSwo)
plot(paraSamplesGSwo)

sGS <- c(paraSamplesGSwo[[1]][,2], paraSamplesGSwo[[2]][,2], paraSamplesGSwo[[3]][,2])
credIntGS <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credIntGS

dicSamplesGSwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesGSwo, FUN = mean)


#Annual Climatology
uv <- as.numeric(sylvestris$year)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesCLIMwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesCLIMwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesCLIMwo)
plot(paraSamplesCLIMwo)

sCLIM <- c(paraSamplesCLIMwo[[1]][,2], paraSamplesCLIMwo[[2]][,2], paraSamplesCLIMwo[[3]][,2])
credIntCLIM <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credIntCLIM

dicSamplesCLIMwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesCLIMwo, FUN = mean)

#Latitude
uv <- as.numeric(sylvestris$lat)

# Bundle data
jags.data <- list(uv=scale(uv)[,1], ratio=ratio, n=n, pCount=pCount)

# Inits function
inits <- function(){list(alpha = rnorm(1), beta = rnorm(1))}

# Specify parameters to estimate
params <- c("alpha", "beta", "shape")

# MCMC settings
nc <- 3 # Number of chains
ni <- 30000 # Number of draws from posterior of each chain
nb <- 15000 # Number of draws to discard as burn in
nt <- 1 # Thinning rate

# Compile model and sampler
jagsModel <- jags.model(file="europemodel.txt", data = jags.data, inits = inits, n.chains=nc, n.adapt= nb)

paraSamplesLATwo <- coda.samples(jagsModel, variable.names= params, n.iter= ni)
gelman.diag(paraSamplesLATwo,confidence = 0.95, multivariate = FALSE)
summary(paraSamplesLATwo)
plot(paraSamplesLATwo)

sLAT <- c(paraSamplesLATwo[[1]][,2], paraSamplesLATwo[[2]][,2], paraSamplesLATwo[[3]][,2])
credIntLAT <- quantile(s2, probs = c(0.07))  #Find where CRI cross zero
credIntLAT

dicSamplesLATwo<-dic.samples(jagsModel, n.iter = ni, n.adapt= nb, type = "pD" )
print(dicSamplesLATwo, FUN = mean)