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
library(cluster)
library(fpc)


######## Use file "factorK42-24major.dta" in the Script factoranalysis.R
######## because this is the main analysing file of the study.

data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta")
data1 <- data[data$specialmajor==0 & data$highqualtymajor==0,]
d <- data1[,4:12]
dat <- na.omit(d)

# nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "ward.D2", index="all")
# , diss="NULL"  : do not work if we put this argument into the command
# max.nc=15 is the default,

nb$All.index
nb$All.CriticalValues
nb$Best.partition

nb$Best.nc -> a
table(a[1,])
barplot(table(a[1,])

nb <- NbClust(dat, distance = "euclidean",
       min.nc=2, max.nc=10, method = "ward.D2", index="all", ) # best is 4

nb <- NbClust(dat, distance = "euclidean",
      min.nc=2, max.nc=10, method = "ward.D", index="all") # 3

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "kmeans", index="all") # 2

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "complete", index="all") # 2, 4

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "single", index="all") # 3

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "median", index="all") # 2

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "centroid", index="all") # 2

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "average", index="all") # 2

cluster1 <- kmeans(d, 3)  # error due to missing values?


