install.packages("psych")

library("readr")
library("dplyr")
library("foreign")
library("haven")
library(reshape2)
library(NbClust)
library(cluster)
library(fpc)
library(factoextra)
library(psych)

data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta")
data1 <- data[data$specialmajor==0 & data$highqualtymajor==0,]
d <- data1[,4:12]
dat <- na.omit(d)

fa.parallel(dat)
