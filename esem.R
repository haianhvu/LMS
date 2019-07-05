install.packages("haven")
install.packages("lavaan")
install.packages("GPArotation")

library(lavaan)
library(haven)
library(GPArotation)


data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/K43/factor43-24major.dta")
data1 <- sapply(data,as.numeric)
data1 <- as.data.frame(data1)
data2 <- data1[,c(2:10)]

# data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta")
# data1 <- sapply(data,as.numeric)
# data1 <- as.data.frame(data1)
# data2 <- data1[,4:12]


b5.model <- "
MATH =~ ECO501001 + ECO501002 + MAT508001 + ACC507001
VERBAL =~  PML510001 + PML510002 + LAW511001
LANG =~ ENG513001 + ENG513002
"

b5.cfa <- lavaan::cfa(b5.model, data = data2, estimator = "MLR")
# Marsh et al. (2013) fit measures:
# CFI: .761, TLI: .687, RMSEA: .076
# Our fit measures:
fitmeasures(b5.cfa, c("cfi.robust","tli.robust","rmsea.robust","srmr"))

###################
  
b5.efa <- fa(data2, nfact = 3, rotate = "geominQ", fm = "ml")
b5.efa
fa.diagram(b5.efa)


###############

b5.loadmat <- zapsmall(matrix(round(b5.efa$loadings, 2), nrow = 15, ncol = 5))
rownames(b5.loadmat) <- colnames(b5[,1:15])

# This will turn the loading matrix into a lavaan-compatible equation set. 

terms <- vector()
for (i in 1:5) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(b5.loadmat[,i]), "*", names(b5.loadmat[,1]), collapse = "+"))
}

# "Correlated uniqueness"
terms[6] <- "A1 ~~ C2+E3+N3\n C2 ~~ E3+N3\n E3 ~~ N3"

b5.esem <- paste(terms, collapse = "\n")
b5.esem

------------------------------
b5.cfa2 <- lavaan::cfa(b5.esem, data = b5, verbose = F, estimator = "MLR")

fitmeasures(b5.cfa2, c("cfi.robust","tli.robust","rmsea.robust","srmr"))


####################################
  
# No equality constraints (configural invariance)
fitmeasures(cfa(
    model = b5.esem,
    data = b5,
    group = "Gender",
    estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings (metric invariance)
fitmeasures(cfa(
  model = b5.esem,
  data = b5,
  group = "Gender",
  group.equal = c("loadings"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# Force equal loadings and intercepts (scalar invariance)
fitmeasures(cfa(
  model = b5.esem,
  data = b5,
  group = "Gender",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR"), c("cfi.robust","tli.robust","rmsea.robust","srmr"))
