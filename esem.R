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
data1 <- sapply(data,as.numeric)
data1 <- as.data.frame(data1)
data2 <- data1[,c(2:10)] #3109

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

b5.cfa <- cfa(b5.model, data = data2, estimator = "MLR")
# Marsh et al. (2013) fit measures:
# CFI: .761, TLI: .687, RMSEA: .076
# Our fit measures:
fitmeasures(b5.cfa, c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# ------- let's try ESEM! First, we conduct an exploratory FA on the dataset -------
# Try with K43 data  
parallel <- fa.parallel(data2, fm = 'pa', fa = 'fa')

b5.efa <- fa(data2, nfact = 3, rotate = "geominQ", fm = "ml")
b5.efa <- fa(data2, nfact = 3, rotate = "Promax", fm = "pa")
b5.efa <- fa(data2, nfact = 3, rotate = "promax", fm = "ml")
b5.efa <- factanal(data2,factors = 3,rotation = "promax")
b5.efa

print(b5.efa$loadings,cutoff = 0.15)
fa.diagram(b5.efa)

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
fa.diagram(b5.efa)


#

b5.loadmat <- zapsmall(matrix(round(b5.efa$loadings, 2), nrow = 9, ncol = 3))


b5.loadmat <- read.csv("C:/Users/Vu/Google Drive/Ph.D/LMS/Loadings.csv",
                       header = FALSE,fileEncoding="UTF-8-BOM")
b5.loadmat <- zapsmall(round(b5.loadmat, 2))
b5.loadmat <- as.matrix(b5.loadmat)

rownames(b5.loadmat) <- colnames(data2[,1:9])

# This will turn the loading matrix into a lavaan-compatible equation set. 

terms <- vector()
for (i in 1:3) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(b5.loadmat[,i]), "*", names(b5.loadmat[,1]), collapse = "+"))
}

# "Correlated uniqueness"
terms[6] <- "A1 ~~ C2+E3+N3\n C2 ~~ E3+N3\n E3 ~~ N3"

b5.esem <- paste(terms, collapse = "\n")
b5.esem

b5.cfa2 <- cfa(b5.esem, data = data2, verbose = F, estimator = "MLR")

d <- fitmeasures(b5.cfa2, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
fa.diagram(b5.cfa2)


####################################
  
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

# As we can see, the model fit does not decrease substantially. 
# The proposed cut-off values for rejecting measurement invariance 
# are .01 for the CFI and .015 for the RMSEA. In this case, CFI and TLI 
# only drop by .007 and the RMSEA increases by .003.



