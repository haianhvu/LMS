install.packages("readr")
install.packages("dplyr")
install.packages("foreign")
install.packages("haven")
install.packages("reshape2")
install.packages("NbClust")
install.packages("factoextra")


library("readr")
library("dplyr")
library("foreign")
library("haven")
library(reshape2)
library(NbClust)
library(cluster)
library(fpc)
library(factoextra)


######## Use file "factorK42-24major.dta" in the Script factoranalysis.R
######## because this is the main analysing file of the study.

data <- read_dta("C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta")
data1 <- data[data$specialmajor==0 & data$highqualtymajor==0,]
d <- data1[,4:12]
dat <- na.omit(d) ## need to omit NA for running cluster analysis 

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
       min.nc=2, max.nc=10, method = "ward.D2", index="all", ) 
# 8 for 2, and 6 for 4

nb <- NbClust(dat, distance = "euclidean",
      min.nc=2, max.nc=10, method = "ward.D", index="all") 
# 8 indices vote for 2 and 7 indices support 4

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "kmeans", index="all") 
# 10 for 2

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "complete", index="all") 
# 10 for 2

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "single", index="all") 
# 10 for 2

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "median", index="all") 
# 9 for 2

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "centroid", index="all") 
# 10 for 2

nb <- NbClust(dat, distance = "euclidean",
              min.nc=2, max.nc=10, method = "average", index="all") 
# 9 for 2, and 9 for 3


distance <- get_dist(dat)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(dat, centers = 2, nstart = 25)
k2
fviz_cluster(k2, data = dat)

dat %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster, name=dat$LAW511001) %>%
  ggplot(aes(ENG513001, ENG513002 , color = factor(cluster), 
         label = name)) +
  geom_text()


k3 <- kmeans(dat, centers = 3, nstart = 25)
k4 <- kmeans(dat, centers = 4, nstart = 25)
k5 <- kmeans(dat, centers = 5, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, geom = "point", data = dat) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = dat) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = dat) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = dat) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p2, p3, p4, p5, nrow = 2)

clusplot(dat, k4$cluster, color=TRUE, shade=TRUE, 
         labels=1, lines=0)
