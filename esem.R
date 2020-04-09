install.packages("haven")
install.packages("lavaan")
install.packages("GPArotation")
install.packages("psychTools")

library(lavaan)
library(haven)
library(GPArotation)
library(psych)
library(psychTools)


rm(list=ls())

#data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/K43/factor43-24major-deleteK40.dta")
# Use this data because K43 CTT has some students of other intakes

data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/K43/factor43-24major-3109.dta")
# this data eliminate some students studying 2 majors
#data1 <- sapply(data,as.numeric)
data1 <- as.data.frame(data)
data2 <- sapply(data1[,c(2:10)],as.numeric) #3109

# data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta")
# data1 <- sapply(data,as.numeric)
# data1 <- as.data.frame(data1)
# data2 <- data1[,4:12]


# ------- First, we perform a CFA and look at the fit measures and loadings -----

b5.model <- "
MATH =~ ECO501001 + ECO501002 + MAT508001 + ACC507001
VERBAL =~  PML510001 + PML510002 + LAW511001
LANG =~ ENG513001 + ENG513002
"
b5.model.modi <- "
MATH =~ ECO501001 + ECO501002 + MAT508001 + ACC507001
VERBAL =~  PML510001 + PML510002 + LAW511001
LANG =~ ENG513001 + ENG513002
LAW511001 ~~ ENG513001
"

corPlot(data2,numbers = TRUE)
pairs.panels(data2)

b5.cfa <- cfa(b5.model, data = data1, estimator = "MLR", std.lv=TRUE)
b5.cfa.modi <- cfa(b5.model.modi, data = data1, estimator = "MLR", std.lv=TRUE)

# Marsh et al. (2013) fit measures:
# CFI: .761, TLI: .687, RMSEA: .076
# Our fit measures:
a <- fitmeasures(b5.cfa, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
b <- fitmeasures(b5.cfa.modi, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
rbind(a,b)

summary(b5.cfa)
modindices(b5.cfa, sort = TRUE, minimum.value = 5) 
summary(b5.cfa, fit.measures = TRUE,
        standardized = TRUE)

b5.cfa3 <- cfa(b5.model, data = data1, estimator = "MLR", 
              std.lv=TRUE, orthogonal = TRUE)
anova(b5.cfa, b5.cfa3) 
# The model allowing covariances among the three latent ability factors 
# fits the data significantly better than a model treating the latent 
# factors as independent.

modelone <- "f  =~ ECO501001 + ECO501002 + MAT508001 + ACC507001 
+ PML510001 + PML510002 + LAW511001 + ENG513001 + ENG513002"

b5.cfa1 <- cfa(modelone, data = data1, estimator = "MLR", 
               std.lv=TRUE)
anova(b5.cfa, b5.cfa1) 
# Yes, 3 factors is significantly better than 1 factors

# ------- let's try ESEM! First, we conduct an exploratory FA on the dataset -------
# Try with K43 data  
parallel <- fa.parallel(data2, fm = 'pa', fa = 'fa')

b5.efa <- fa(data2, nfact = 3, rotate = "geominQ", fm = "ml")
b5.efa <- fa(data2, nfact = 3, rotate = "Promax", fm = "pa")

b5.efa <- fa(data2, nfact = 3, rotate = "promax", fm = "pa")
b5.efa <- fa(data2, nfact = 3, rotate = "promax", fm = "ml")
b5.efa <- factanal(data2,factors = 3,rotation = "promax")

b5.efa

print(b5.efa$loadings,cutoff = 0.15)
fa.diagram(b5.efa, digits=3)

# Try with K42 data
dt <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta")
dt1 <- subset(dt,highqualtymajor==0 & specialmajor==0) # 2809
dt2 <- subset(dt1, MSSV/10000000 >= 3116 ) #2799
dt3 <- dt2[,4:12]

corPlot(dt3,numbers = TRUE)
pairs.panels(dt3)

parallel <- fa.parallel(dt3, fm = 'pa', fa = 'fa')
b5.efa <- fa(dt3, nfact = 3, rotate = "promax", fm = "pa")
b5.efa

print(b5.efa$loadings,cutoff = 0.29) # results are nearly similar to stata
fa.diagram(b5.efa, digits=3)


# Create model for ESEM of K43

b5.loadmat <- zapsmall(matrix(round(b5.efa$loadings, 3), nrow = 9, ncol = 3))

# Main code: Use this model with coefficients of STATA
b5.loadmat <- read.csv("C:/Users/Vu/Google Drive/Ph.D/LMS/Loadings.csv",
                       header = FALSE,fileEncoding="UTF-8-BOM")
b5.loadmat <- zapsmall(round(b5.loadmat, 3))
b5.loadmat <- as.matrix(b5.loadmat)

rownames(b5.loadmat) <- colnames(data2[,1:9])
b5.loadmat

# This will turn the loading matrix into a lavaan-compatible equation set. 

terms <- vector()
for (i in 1:3) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(b5.loadmat[,i]), "*", names(b5.loadmat[,1]), collapse = "+"))
}

# the expression y1 ~~ y5 allows the residual variances of the
# two observed variables to be correlated
terms[4] <- "MAT508001 ~~ ACC507001"
" ENG513001 ~~ LAW511001"

"A1 ~~ C2+E3+N3\n C2 ~~ E3+N3\n E3 ~~ N3"

b5.esem <- paste(terms, collapse = "\n")
b5.esem

b5.cfa2 <- cfa(b5.esem, data = data1, verbose = F, estimator = "MLR")

d <- fitmeasures(b5.cfa2, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
fa.diagram(b5.cfa2)
d # fit index is very good, RMSEA is smaller than 0.06

summary(b5.cfa2)
modindices(b5.cfa2, sort = TRUE, minimum.value = 5) 
summary(b5.cfa2, fit.measures = TRUE,
        standardized = TRUE) #H null: RMSEA <= 0.05: p-value=0.406 -> accept
# Normally, the test is affectd by test size, so we should use Robust
# estimation instead of sample estimation. Here, p-value of sample is 
# 0.001, but p-value of robust is 0.406. 

#--------------------- Check Invariance of Gender ---------------------
  
# No equality constraints (configural invariance)
a <- fitmeasures(cfa(
    model = b5.esem,
    data = data1,
    group = "Gender",
    estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings (metric invariance)
b <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "Gender",
  group.equal = c("loadings"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings and intercepts (scalar invariance)
c <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "Gender",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

e <- rbind(d,a,b,c)
e
diff(e)

# As we can see, the model fit does not decrease substantially. 
# The proposed cut-off values for rejecting measurement invariance 
# are .01 for the CFI and .015 for the RMSEA. In this case, CFI and TLI 
# only drop by .007 and the RMSEA increases by .003.

#------------- Check invariance for AreaID------------

# No equality constraints (configural invariance)
x <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "AreaID",
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings (metric invariance) for AreaID
y <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "AreaID",
  group.equal = c("loadings"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings and intercepts (scalar invariance)
z <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "AreaID",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

e <- rbind(d,x,y,z)
e
diff(e)


#-------------- Group Invariance for Party --------------------

# No equality constraints (configural invariance)
k <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "Party",
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings (metric invariance)
l <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "Party",
  group.equal = c("loadings"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings and intercepts (scalar invariance)
m <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "Party",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

e <- rbind(d,k,l,m)
e
diff(e)

############ Group Invariance for Religion
data1$religion <- ifelse(data1$ReligionID==0,0,
                         ifelse(data1$ReligionID=="PG","PG",
                                ifelse(data1$ReligionID=="TC","TC","Khac"
                                )))

# No equality constraints (configural invariance)
g <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "religion",
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings (metric invariance)
h <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "religion",
  group.equal = c("loadings"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings and intercepts (scalar invariance)
i <- fitmeasures(cfa(
  model = b5.esem,
  data = data1,
  group = "religion",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

e <- rbind(d,g,h,i)
e
diff(e)

### Input loadings from FA of K42: confirm these loadings #####
# Try with K42 data
dt <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta")
dt1 <- subset(dt,highqualtymajor==0 & specialmajor==0) # 2809
dt2 <- subset(dt1, MSSV/10000000 >= 3116 ) #2799
dt3 <- dt2[,4:12]
dt3 <- as.numeric(dt3)

corPlot(dt3,numbers = TRUE)
pairs.panels(dt3)

parallel <- fa.parallel(dt3, fm = 'pa', fa = 'fa')
b5.efa <- fa(dt3, nfact = 3, rotate = "promax", fm = "pa")
b5.efa

print(b5.efa$loadings,cutoff = 0.29) # results are nearly similar to stata
fa.diagram(b5.efa, digits=3)


# Create model for ESEM of K42

b5.loadmat <- zapsmall(matrix(round(b5.efa$loadings, 3), nrow = 9, ncol = 3))

# Try code: Use this model with coefficients of STATA command
b5.loadmat <- read.csv("C:/Users/Vu/Google Drive/Ph.D/LMS/LoadingsK42.csv",
                       header = FALSE,fileEncoding="UTF-8-BOM")
b5.loadmat <- zapsmall(round(b5.loadmat, 3))
b5.loadmat <- as.matrix(b5.loadmat)

rownames(b5.loadmat) <- colnames(data2[,1:9])
b5.loadmat

# This will turn the loading matrix into a lavaan-compatible equation set. 

terms <- vector()
for (i in 1:3) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(b5.loadmat[,i]), "*", names(b5.loadmat[,1]), collapse = "+"))
}

# the expression y1 ~~ y5 allows the residual variances of the
# two observed variables to be correlated
terms[4] <- " ENG513001 ~~ ENG513002"

"A1 ~~ C2+E3+N3\n C2 ~~ E3+N3\n E3 ~~ N3"

b5.esem <- paste(terms, collapse = "\n")
b5.esem

b5.cfa2 <- cfa(b5.esem, data = dt3, verbose = F, estimator = "MLR")
# infact, each sample will have different loadings, so if we take loadings
# of K42 and test it in K43 sample, diffinitely the results can not be good
# here, CFA should be understood in the way that we test it in the same sample,
# or we split the sample. Split will ensure the test is unique.
# Note: the EFA we run at fisrt is just the way we obtain loadings for this
# sample, and then we use CFA function of R to run ESEM, it does not mean
# we test the loaings of EFA. The procedure of ESEM is:
# 1. Run EFA to obtain loadings
# 2. set the model based on the loadings 
# 3. run CFA function
# 4. check modification and re-define model if needed.

b5.cfa2
inspect(b5.cfa2,"cor.lv")

d <- fitmeasures(b5.cfa2, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
fa.diagram(b5.cfa2)
d
show(b5.cfa2)
summary(b5.cfa2)
modindices(b5.cfa2, sort = TRUE, minimum.value = 5) 
