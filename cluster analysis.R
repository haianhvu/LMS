install.packages("readr")
install.packages("dplyr")
install.packages("foreign")
install.packages("haven")
install.packages("reshape2")
install.packages("NbClust")

library("readr")
library("dplyr")
library("foreign")
library("haven")
library(reshape2)
library(NbClust)


######## Use file "factorK42-24major.dta" in the Script factoranalysis.R
######## because this is the main analysing file of the study.

data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta")
nb <- NbClust(data[,4:12], distance = "euclidean",
              min.nc=2, max.nc=10, method = "kmeans", index="all")
# , diss="NULL"  : do not work if we put this argument into the command
# max.nc=15 is the default,

nb$All.index
nb$All.CriticalValues
nb$Best.partition
nb$Best.nc -> a

barplot(table(a[1,]))

