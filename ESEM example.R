############ ESEM implementation ############
# This script accompanies the article
# "Exploratory structural equation modeling for event-related potential data - 
#  an all-in-one approach?" by Scharf & Nestler (2018)
# The goal of this script is to demonstrate the implementation of an ESEM
# utilizing any basic SEM software. It contains some more detailed comments than the example script.
# We also demonstrate that it closely resembles the output of the respective MPlus model. 
############ 

########### Load required packages ###########
library(LaplacesDemon)
library(psych)
library(lavaan)
library(GPArotation)
library(msm)
library(MplusAutomation)
library(boot)
library(arrayhelpers)
library(abind)
###########

############ Generate example data ############ 

# generate an example data set as it is used in the paper (but with only 100 participants)
# reproducibility seed
set.seed(22)

#### utility function to generate time courses 
simLambda = function(samples, mean, sd, peaks){
  if(length(mean) != length(sd) || length(sd) != length(peaks)){stop("mean and sd must have equal length.")}
  m = length(mean) #extract number of components
  t = length(samples) #extract number of samples
  Lambda = sapply(1:m, function(iFactor){
    temp = dnorm(samples, mean[iFactor], sd = sd[iFactor])
    temp = peaks[iFactor]/max(temp) * temp
  })
  return(Lambda)
}
####

#### utility function to calculate the parameters for rmvnorm from the common factor model 
simMeanSigma = function(FacLoad, FacCov, ErrCov, elMeans, iN){
  
  # Population Covariance Matrix
  sigma = round(FacLoad %*% FacCov %*% t(FacLoad) + ErrCov,10)
  
  # Population Correlation Matrix
  rho = diag(diag(sigma)^(-0.5))%*%sigma%*%diag(diag(sigma)^(-0.5))
  
  
  #Sanity Check Plot
  #barplot(height = elMeans[1,], names.arg = 1:20) #P3
  #barplot(height = elMeans[2,], names.arg = 1:20) #N2
  
  mu = FacLoad%*%elMeans # Grand Average for all electrodes in all conditions
  
  # replicate mu as often as we have participants (assume each participant is a realisation of the same gavrs)
  mu = matrix(rep(mu,iN), ncol = dim(FacLoad)[1],byrow =TRUE)
  #sanity check plot
  #plot(x,mu[41,])
  
  #Sanity check plot 
  #plot(x,mu[,1], col = "red", lwd = 2, type = "l", ylim = c(-4,4))
  #lines(x,mu[,11], col = "blue", lwd = 2)
  
  list(mu = mu, sigma = sigma, rho = rho)  
}
####

#Sampling Points
x = seq(1,450,length.out = 50)

# generate loading pattern
FacLoad = simLambda(x, mean = c(300,120), sd = c(40,40), peaks = c(1,0.8))
#FacLoad = simLambda(x, mean = c(200,120), sd = c(40,40), peaks = c(1,0.8)) #more temporal overlap

# Sanity check plot:
plot(x,FacLoad[,1],ylim=c(-1,2), type = "l")
lines(x,FacLoad[,2], lty = 2)

# Population factor and error correlation matrices between participants
FacCov = matrix(c(1, +0.3, +0.3, 1),byrow=TRUE,nrow=2) #moderate between participants correlation
ErrCov = diag(0.4, nrow = length(x)) #uncorrelated errors, error variance (= SNR) constant over time

#Expected Value of Y based on Factor Scores (i.e. based on condition differences)
#note: this is a model for the "average" population person
#these scores refer to the electrode with the maximum peak
means = matrix(c(2.5,-1.5,3.5,-2.5),nrow=2) # condition means: note that these are the Factor Scores for the Average Population Person 

nElectrodes = 10 # number of electrodes to simulate
# components with strong overlap:
elWeights = cbind(seq(0.1,1,length.out = nElectrodes), seq(1,-0.5,length.out = nElectrodes)) # generate a 'parietal' topography for our 'P3' and a 'frontal' topography for our 'N2'
# orthogonal components:
#elWeights = cbind(c(seq(0.1,1,length.out = nElectrodes/2),seq(1,0.1,length.out = nElectrodes/2)), seq(1,-0.5,length.out = nElectrodes)) # generate a 'parietal' topography for our 'P3' and a 'frontal' topography for our 'N2'


# we multiply the elWeights with the maximum scores 
elMeans = 
  rbind(c(t(outer(means[1,],elWeights[,1]))), # Factor 1 'P3'
        c(t(outer(means[2,],elWeights[,2]))) # Factor 2 'N2'
  )

# get parameters for rmatrixnorm
mvnormParam = simMeanSigma(FacLoad, FacCov, ErrCov, elMeans, iN = 100)


#Generate data from matrix normal distribution
rawData = rmatrixnorm(M = mvnormParam$mu, U = diag(1,nrow = dim(mvnormParam$mu)[1]), V = mvnormParam$sigma)
# for now we assume that U is diagonal (the covariance between participants, electrodes and conditions, is zero)  
# the ordering in rawData is managed by mu, the dependence within participants can be specified in U.

IDvars = expand.grid(list(1:nElectrodes,1:2,1:200))[,3:1]
rawData = data.frame(IDvars,rawData)
names(rawData)[1:3] = c("VP","Cond","El")

# prepare data set with dummy variables  
interactions = data.frame(apply(dummy.code(rawData$El), MARGIN = 2, FUN = function(iEl){iEl * dummy.code(rawData$Cond)[,1]}))
names(interactions) = paste("Cond_x_El.",1:10, sep = "")

rawDataMPLUS = data.frame(VP = rawData$VP, Cond = dummy.code(rawData$Cond)[,1], El = dummy.code(rawData$El), interactions ,rawData[,grep("X",names(rawData))])

# Write data to text file in order for Mplus to use it.
write.table(rawDataMPLUS,file = "ExampleData.txt",row.names = FALSE, col.names = FALSE)

############ 


############ Implementation with lavaan ############ 

# in order to run script: adapt path for you computer!
# Folder should contain all files from the "implementation" OSF folder.
setwd("...")


##### STEP I: initial unrotated model #####
# This function simply creates the initial SEM for an ESEM 
# from the number of factors and the indicator and predictor variables.
# It is a convenience function to simplify the model specification in the 
# presence of many variables (sampling points)
initialModel = function(nFactors, variables, predictors){
  factors = paste("f",1:nFactors,sep = "") #prepare factor labels
  loadings = matrix(paste("lambda_",expand.grid(1:length(variables), 1:nFactors)[,2],"_", expand.grid(1:length(variables), 1:nFactors)[,1], sep = "")
                    ,length(variables),nFactors) # prepare loading matrices
  
  # don't fix first loading to 1
  loadings[1,] = NA 
  # set upper triangle to zero (see Asparouhov & Muthen, 2009)
  loadings[upper.tri(loadings)] = 0
  
  #set restricions
  # sum of loadings > 0 per factor (to prefer positive loadings)
  # Note the factors can be sign-inverted any time, it is just for convenience (e.g., plotting)
  # that we prefer positive loadings, this procedure imitates the behaviour of Mplus
  # loadingConstraints = paste(apply(loadings[-1,], MARGIN = 2, function(x){
  #   paste(paste(x,collapse = " + "), " > 0", collapse = "")
  # }), collapse = "\n")
  
  loadings = apply(loadings,MARGIN = 2,FUN = function(x){paste(x,variables,sep = "*")})
  
  # These lines are only an automatized way to write the model definitions
  # for the huge number of variables.
  # In order to make this step more transparent, the model definition 
  # is saved in a document called "model.txt". We recommend you refer to this file
  # if you want to see the model specification in detail.
  measurementModels = sapply(1:nFactors,FUN = function(x){paste(factors[x],"=~",paste(loadings[,x],collapse = "+"),collapse = "\n")})
  obsIntercepts = sapply(variables,FUN = function(x){paste(x,"~ 0",collapse = "\n")})
  factorCombinations = expand.grid(factors,factors)
  test = expand.grid(1:nFactors,1:nFactors) #remove redundant covariances
  factorCombinations = factorCombinations[test[,1] <= test[,2],]
  factorCovariances = sapply(1:dim(factorCombinations)[1], function(x){ifelse(factorCombinations[x,1] == factorCombinations[x,2],paste(factorCombinations[x,1],"~~1*",factorCombinations[x,2],sep = ""),paste(factorCombinations[x,1],"~~0*",factorCombinations[x,2],sep = ""))})
  structuralModel = paste(expand.grid(factors,paste(predictors, collapse = " + "))[,1],expand.grid(factors,paste(predictors, collapse = " + "))[,2],sep = "~1+")
  model = c(measurementModels, obsIntercepts,factorCovariances,structuralModel) #, loadingConstraints)
  writeLines(model, "model.txt")
  return(model)
}

# Specify a model with 2 factors
data = rawDataMPLUS[,-grep("El.1$", names(rawDataMPLUS))] #remove redundant dummies
model = initialModel(nFactors = 2, variables = names(data)[grep("X",names(data))], predictors = names(data)[2:20])

# Estimate initial model
tStart = Sys.time()
fit = sem(model = model, data = data, int.ov.free = FALSE, int.lv.free = TRUE, meanstructure = TRUE, std.lv = FALSE, se="none")
runTime = Sys.time() - tStart
runTime 

# Parameters of the initial model
summary(fit) # standard errors were turned off to improve speed (and because they are not in the center of interest)
#####

##### Step II: Rotation of measurement model #####
nSamplingPoints = length(x) 

# Extract parameters and arrange them in a factor loading matrix Lambda
inital_estimates = coef(fit, type = "user")
# The zero is inserted in order to get the fixed to zero factor loading back into the matrix.
Lambda = cbind(inital_estimates[1:nSamplingPoints],c(inital_estimates[(nSamplingPoints+1):(2*nSamplingPoints)]))

# Apply oblique Geomin rotation to Lambda:
# Settings aim to imitate Mplus defaults.
fit_rot = geominQ(Lambda, normalize = FALSE, delta = 0.0001)

# extract rotated factor loadings
Lambda_rot = fit_rot$loadings
# extract rotation matrix
H = t(fit_rot$Th) # Note: the geominQ function returns the transposed rotation matrix

# check if any factor has mainly negative loadings
turnFactor = apply(Lambda_rot, 2, function(x){sum(x)<0})
# advantage: if this is taken care of right here, it does not need to be 
# accounted for in a separate future step

# if so, turn this factor
Lambda_rot[,turnFactor] = -Lambda_rot[,turnFactor]
H[turnFactor,] = -H[turnFactor,]

# recalculate factor correlation matrix
Phi = H%*%t(H)

#####

##### Step III: rotation of the other model parameters #####
# Extract structural coefficients
B = rbind(parameterEstimates(fit)$est[parameterEstimates(fit)$op == "~" & parameterEstimates(fit)$lh == "f1"],
          parameterEstimates(fit)$est[parameterEstimates(fit)$op == "~" & parameterEstimates(fit)$lh == "f2"]) 


# Rotate structural coefficients
B_rot = H %*% B 
B_rot

# Extract factor intercepts
alpha = parameterEstimates(fit)$est[parameterEstimates(fit)$lh == "f1" & parameterEstimates(fit)$op == "~1" 
                                    | parameterEstimates(fit)$lh == "f2" & parameterEstimates(fit)$op == "~1"]

# Rotate factor intercepts
alpha_rot = H %*% alpha
alpha_rot

#####
############ 

############ Implementation with Mplus ############ 
# In order to demonstrate the equivalence (up to numerical imprecisions) of
# the estimates, we here provide the respective Mplus output.
# If you do not own an Mplus license, you may start with the provided 
# Mplus output file.

# run estimation - if you have Mplus installed
runModels(showOutput = TRUE)

# read output (from here, it works without Mplus)
Mplus_fit = readModels()
print(Mplus_fit$parameters)
############ 

############ Illustrate the results ############ 

##### factor loadings #####
plot(x,FacLoad[,1], ylim = c(-0.2,1.2),type = "l", lty = 2, lwd = 4, col = "grey",xlab = "Time (ms)", ylab = "Factor Loadings", main = "Factor loadings estimates")
lines(x,FacLoad[,2], lty = 1, lwd = 4, col = "grey")
abline(h = 0)

# Mplus
lines(x,Mplus_fit$parameters$unstandardized$est[1:nSamplingPoints],lty = 1, lwd = 4, col = "red")
lines(x,Mplus_fit$parameters$unstandardized$est[(nSamplingPoints + 1):(2*nSamplingPoints)],lty = 2, lwd = 4, col = "red")

# Lavaan implementation
lines(x,Lambda_rot[,1],lty = 1, lwd = 4, col = "blue")
lines(x,Lambda_rot[,2],lty = 2, lwd = 4, col = "blue")
legend("topleft",legend = c("Population","Sample_Lavaan","Sample_Mplus"), col = c("grey","blue","red"), lty = 1, lwd = 3, bty = "n", cex = 1.2)

# the estimates are almost perfectly correlated!
cor(Mplus_fit$parameters$unstandardized$est[1:nSamplingPoints],Lambda_rot[,1])
cor(Mplus_fit$parameters$unstandardized$est[(nSamplingPoints + 1):(2*nSamplingPoints)], Lambda_rot[,2])
#####

##### Structural coefficients #####
print(Mplus_fit$parameters$unstandardized[(2*nSamplingPoints + 1):(2*nSamplingPoints + 19),])
B_rot[1,]
# all coefficients are equal if rounded to 3 digits
all.equal(Mplus_fit$parameters$unstandardized$est[(2*nSamplingPoints + 1):(2*nSamplingPoints + 19)], round(B_rot[1,],3))

print(Mplus_fit$parameters$unstandardized[(2*nSamplingPoints + 20):(2*nSamplingPoints + 38),])
B_rot[2,]
all.equal(Mplus_fit$parameters$unstandardized$est[(2*nSamplingPoints + 20):(2*nSamplingPoints + 38)],round(B_rot[2,],3))
#####

##### Intercepts of the structural model #####
alpha_rot
Mplus_fit$parameters$unstandardized$est[Mplus_fit$parameters$unstandardized$paramHeader == "Intercepts" & grepl("F",Mplus_fit$parameters$unstandardized$param)]
##### 

##### (residual) factor correlation #####
Phi[1,2]
Mplus_fit$parameters$unstandardized$est[139]
##### 

############ 

############ Step IV: Estimate standard errors by bootstrapping ############ 
# Up to this point, we were able to replicate the Mplus parameter estimates.
# In the following, we want to provide an example how ESEM standard errors can be estimated
# using the bootstrap.

# The bootstrapping procedure may be summarized as follows: 
# 1)	Draw a random bootstrap sample (of original sample size) with replacement from the original sample. The sample should be drawn participant-wisely, that is, all data from the participant are drawn as a fixed block if that participant is (randomly) chosen. (This is called a block bootstrap.)
# 2)	Estimate the ESEM for the new sample.
# 3)	Repeat 1) and 2) many times (e.g., 1000 times) and collect the ESEM parameters each time.
# 4)	Obtain confidence intervals for all parameters by determining the 2.5% and the 97.5% quantiles of the parameter distributions across all bootstrap samples.
# Due to the blocked resampling in step 1) this procedure preserves the correlations between the repeated measures and enables valid statistical inferences. 
# We verified the validity of this approach for our simulated data in a supplementary simulation (II) that is available from the OSF. 
# We found, that the bootstrap confidence intervals yield correct type 1 error rates and consistent estimates of the empirical across-sample variation of the model parameters. 


# This function summarizes the steps outlined above in one line of code.
esem = function(model, data, int.ov.free = FALSE, int.lv.free = TRUE, meanstructure = TRUE, std.lv = FALSE, se = "none", delta = .0001, nSamplingPoints = length(grep("X", names(data)))){
  # initial model
  fit = sem(model = model, data = data, int.ov.free = int.ov.free, int.lv.free = int.lv.free, meanstructure = meanstructure, std.lv = std.lv, se = se)
  # extract parameters
  inital_estimates = inspect(fit, what = "est")
  Lambda = inital_estimates$lambda[grepl("X", row.names(inital_estimates$lambda)),c("f1", "f2")]
  
  # Geomin rotation
  fit_rot = geominQ(Lambda, normalize = FALSE, delta = delta)
  # Extract rotated factor loadings and rotation matrix
  Lambda_rot = fit_rot$loadings
  H = t(fit_rot$Th)
  
  # check if any factor has mainly negative loadings
  turnFactor = apply(Lambda_rot, 2, function(x){sum(x)<0})
  # advantage: if this is taken care of right here, it does not need to be 
  # accounted for in a separate future step
  
  # if so, turn this factor
  Lambda_rot[,turnFactor] = - Lambda_rot[,turnFactor]
  H[turnFactor,] = -H[turnFactor,]
  
  # recalculate factor correlation matrix
  Phi = H%*%t(H)
  
  # Extract and rotate condition effect estimates
  B = inital_estimates$beta[c("f1", "f2"),!grepl("f", colnames(inital_estimates$beta))]
  
  B_rot = H %*% B
  row.names(B_rot) = row.names(B)
  
  # Extract and rotate intercepts of the factors
  
  alpha = inital_estimates$alpha[c("f1", "f2"),]
  
  alpha_rot = H %*% alpha
  rownames(alpha_rot) = names(alpha)
  
  # Return the parameters
  return(list(fit = fit, H = H, alpha_rot = alpha_rot, B_rot = B_rot, Lambda_rot = Lambda_rot, Phi = Phi))
}

# estimate ESEM for data set
esemFit = esem(model, data, delta = 0.0001)

# just to validate that the esem-function works:
lines(x,esemFit$Lambda_rot[,1],lty = 1, lwd = 4, col = "green")
lines(x,esemFit$Lambda_rot[,2],lty = 2, lwd = 4, col = "green")

###### das Bootstrapping sollte die richtigen Effekte ausgeben - testen ####

# function that defines what is done for each bootstrap sample
bs = function(data, indices, model = readLines("model.txt"), esemFit_ref = esemFit){
  
  # draw random sample of participants (with replacement)
  indices = sample(unique(data$VP), replace = TRUE)
  # draw the respective data from these participants and rearrange them to fit the original form of the data matrix
  data_bs = lapply(indices, function(i){data[data$VP == i, ]})
  data_bs = do.call("rbind", data_bs)
  names(data_bs) = names(data)
  
  # estimate ESEM for the bootstrap sample
  esemFit_bs = esem(model, data_bs, delta = 0.0001)
  
  # align factors by pattern similarity and assure mainly positive factor loadings
  alignment = order(abs(cor(esemFit_bs$Lambda_rot, esemFit_ref$Lambda_rot))[,1], decreasing = TRUE)
  esemFit_bs$B_rot = esemFit_bs$B_rot[alignment,]
  esemFit_bs$alpha_rot = esemFit_bs$alpha_rot[alignment,]
  esemFit_bs$Lambda_rot = esemFit_bs$Lambda_rot[,alignment]
  
  # return effect sizes
  out = c(t(esemFit_bs$B_rot))
  out = c(out, esemFit_bs$Phi[1,2], esemFit_bs$alpha_rot)
  
  # Parameter names
  structParamHeaders = expand.grid(colnames(esemFit_bs$B_rot), rownames(esemFit_bs$B_rot))[,2:1]
  structParamHeaders = paste(structParamHeaders[,1], structParamHeaders[,2], sep = "~")
  
  names(out) = c(structParamHeaders, 
                 "F1.WITH.F2", paste0(names(esemFit_bs$alpha_rot),"~1"))
  return(out)
}

# apply the bootstrap - attention: can last several hours (check with a smaller number of bootstraps first)
tStart = Sys.time()
nBootSamples = 1000
bootFit = boot(data, statistic = bs, R = nBootSamples)
runTime = Sys.time() - tStart
runTime

# Name parameters
colnames(bootFit$t) = names(bootFit$t0)

# save files
save(file = paste("bootstrap_", nBootSamples,"_", Sys.Date(),".Rdata", sep = ""), list = ls())


# alternatively, you can load our bootstrap results to get an impression
load("bootstrap_1000_2018-08-30.Rdata")

# parameters from each bootstrap sample
# get estimates and SEs from our implementation
res = t(apply(bootFit$t, 2, function(x){c(mean(x), sd(x))}))
res_mplus = Mplus_fit$parameters$unstandardized[c(101:139, 190:191),]

# compare results (first MPLUS, then our implementation in the last two columns)
res_mplus[,1:4]
round(res,3)

# The suggested bootstrap approach leads to very similar standard error estimates as the Mplus method when the rows
# of the data matrix are independent and is still consistent if not. Please see supplementary simulation II for a
# verification of the consistency across samples.
############ 
