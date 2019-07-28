
library("readr")
library("dplyr")

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTT")

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/")
# Here, general courses only have 11 courses including 2 courses of FRE
dc <- read_csv("daicuong2.csv")
names(dc)[2:4] <- c("MaHP","Name","weight")
weight <- dc[!grepl("FRE", dc$MaHP),c(2,4)]
gc <- weight[,1]
# Note: in general stage, K42 only studied 9 subjects (not including STA, HCM, ENG part 3)
# Note: some of them learn French (not really)
gc2 <- dc[,2]


#Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTT", pattern="*.csv")
Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTT24", pattern="*.csv")
test <- lapply(Files, function(x) read.csv(x)) # Xem tren test de biet bao nhieu file

# Creat 27 score datasets for each of Majors
for (i in 1:27) {
  assign(paste("score",i, sep=""), data.frame(test[i])[-1,])
  assign(paste("zname",i, sep=""), data.frame(test[i])[1,])
}

# Combine 26 dataset into 1 dataset, there are 3215 students
score <- score1
for (i in 2:27) {
  a <- data.frame(test[i])[-1,]
  score <- full_join(score,a)
}

# Check mode of variables and change it to numeric
mode <- data.frame(sapply(score, mode))
score[, 7:ncol(score)] <- sapply(score[, 7:ncol(score)], as.character)
score[, 7:ncol(score)] <- sapply(score[, 7:ncol(score)], as.numeric)
score[,5] <- as.numeric(score[,5])
# mode1 <- data.frame(sapply(score, mode))

# Change the names of variables to ignore the problem of Vietnamese
colnames(score)[1:6] <- c("STT","MSSV","Ho","Ten","DTB","XL")
# Only keep scores of general courses in dataset gc
# i.e. ignoring the French courses, 
# which means ignoring students studying French major
general_score <-  data.frame(score[,1:6],score[as.vector(gc$MaHP)]) # Trich xuat cot du lieu trong dataframe theo ten cot. 
# general_score2 <-  data.frame(score[,1:6],score[as.vector(gc2$MaHP)])

# # Choosing names of general classes 
# numberstudent <- data.frame(colSums(!is.na(score)))
# numberstudent$row<-rownames(numberstudent)
# # General classes shoud have more than 2500 students
# general_class <- numberstudent[numberstudent[1]>2500,]
# # There are totally 18 variables, so we will choose var 7 to var 18
# listclass <- general_class[7:18,2]


# final data frame for general classes, 3215 students, but 34 duplicated records
# Aim: delete observation having missing values.
final_score <- general_score[rowSums(is.na(general_score[, -c(1:6)]))==0,]
# final_score2 <- general_score[complete.cases(general_score[ , 7:15]),]
# We found 34 observations appearing 2 times in final_score
# ==> 17 students study 2 majors
test <- final_score[duplicated(final_score$MSSV) | duplicated(final_score$MSSV, fromLast = TRUE),]
test2 <- setdiff(final_score,test) # 2810
test3 <- test2[grepl(pattern = "3116",test2$MSSV),] #2800, nearly 2799

setdiff(test3$MSSV,dt2$MSSV) #many MSSV exist in test3, not dt2
# reason: some MSSV study 2 major, 1 HPR and 1 CTT, so they still appear in folder CTT24
# the best way is combine all CTT, HPR, CLC, and then eliminate duplicated
# so the correct data of K42 should be found in final.R
