install.packages("readr")
install.packages("dplyr")
install.packages("foreign")
install.packages("haven")

library("readr")
library("dplyr")
library("foreign")
library("haven")

company <- read.spss("C:/Users/Vu/Desktop/data/Newfolder/Du-lieu-Doanh-nghiep_26-2-2019.sav", to.data.frame=TRUE)
labor <- read.spss("C:/Users/Vu/Desktop/data/Newfolder/Du-lieu-Lao-dong_19.2.2019.sav", to.data.frame=TRUE)

a <- unique(company$VAR00001)
length(a) #101 company
b <- unique(labor$VAR00002)
length(b) #107 company
setdiff(b,a) #50,51,53,54,55,91

c <- labor[labor$VAR00002 %in% setdiff(b,a),]
d <- setdiff(labor,c)

full <- merge(d,company,by.x="VAR00002",by.y="VAR00001")
