install.packages("readr")
install.packages("dplyr")

library("readr")
library("dplyr")

rm(list = ls())
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")

# Import data into R
data <- read_delim(file = "Thong_ke_LMS_K42.csv", delim=',')

# Check R read file correctly or not by using Excel and import csv file
# into excel by Data -> From CSV/Text 
#write_delim(data,"aaa.csv",delim=',') 

# make sure there is no duplication: 31200
unique(data) -> data

# Extract MSSV from the first column
a <- substr(data$`Sinh viên`,1,11)
full_data <- cbind(a,data)
names(full_data)[1] <- "MSSV"

# import data into R
hkc2016 <- read_csv("HKC2016.csv",col_types = cols(MaHP = "c"))
hkc2017 <- read_csv("HKC2017.csv",col_types = cols(MaHP = "c"))
hkd2017 <- read_csv("HKd2017.csv",col_types = cols(MaHP = "c"))

# Rename variables 1 to 4 from Vietnamese to English
names(full_data)[2:5] <- c("student","course","classcode","download")

# Rename key variable of 3 hk dataset for combining and merging
names(hkc2016)[7] = "classcode"
names(hkc2017)[7] = "classcode"
names(hkd2017)[7] = "classcode"
hk <- rbind(hkc2016,hkc2017,hkd2017)
# Number of class schedule: 10089 = 2978 + 3761 + 3350
hk <- unique(hk)

# Combine LMS and Schedule datasets
# use full joint if we want to keep all of the observations of 2 datasets
# including both matched and unmatched values
# left_join(data,hk) -> fulldata
# Use merge when we want to keep the matched values only (like inner join)
# Note: merge can be used for right, left, and full joint
# full_data <- merge(full_data,hk,by="classcode")
fulldata<-unique(full_data)


# # Extract the classes of general stage
# # Do not have Statistics and Hochiminh
# # Load data and check loading process by writing it again
# dc <- read_csv("DC.csv")
# #write_delim(dc,"daicuong.csv",delim=",")

# # Change the variable name to match with other datasets before merging
# names(dc)[2] <- "MaHP"
# # Merge Schedule data with dc data to create Schedule for general classes
# merge(dc,hk,by.y="MaHP",by.x="MaHP") -> abc

# # Check duplicated observations, it means the first value is 0
# # and the next duplicated value is 1, 2, 3, ..
# abc$control<-ifelse(duplicated(abc$MaCBGD),1,0)
# # Extract the Lecturer for general classes
# abc2<-abc[abc$control==0,]
# # abc3<-abc[!duplicated(abc$MaCBGD),] # Check again by another method
# 
# # Selecting some variables that are used to collect more data of Lecturer
# # Minh Thanh will do this task
# abc2 <- subset(abc2,select=c("MaHP","Khoa","BoMon","MaCBGD","TenCBGD"))
# # write_delim(abc2,"giangvien.csv",delim=",")


# Bo sung Statistics va HCM
# stahcm <- hk[hk$MaHP=="STA508005" | hk$MaHP == "HCM510004",]
# gv <- stahcm[!duplicated(stahcm$MaCBGD),]
# gv <- subset(gv,select=c("MaHP","Khoa","BoMon","MaCBGD","TenCBGD"))
# gv[29,4] <- "a"
# write_delim(gv,"giangvienbosung.csv",delim=",")


# # Check duplicated observation of student in fulldata dataset
# fulldata$nastudent<-ifelse(is.na(fulldata$student),1,0)
# sum(fulldata$nastudent)
# sum(is.na(fulldata$student))
# # Results are 0, means no duplicated data
# 
# #Number of student: 4587
# data$count=1
# no_student <- aggregate(data$count ~ data$student,data=data,sum)
# #no_student2 <- unique(data$student)

# Create class cluster id
full_data <- transform(fulldata,id=as.numeric(factor(classcode)))


#remove object in R
#rm(list = ls(pattern = "score*"))
#remove(list=ls())
#rm(list = apropos("score_"))



#------ Some descriptive statistics for the datasets --------

# # number of time each course was given in the data student
# unique(data$course) -> courselist
# length(courselist)
# dim(courselist)=c(61,1)
# table(data$course)->tab
# data.frame(tab) -> times_for_course
# dim(times_for_course)
# 
# # number of times each course was given for each students
# data$count<-1
# course_for_student <- aggregate(count ~ student + course,data=data,sum)
# dim(course_for_student)
# # sort the data based on count varialbe from highest to lowest#
# course_for_student[order(-course_for_student$count),] -> course_for_student
# 
# # Number of coursres duplicated in the hk data: 1582
# duplicated(hk$classcode) -> a
# sum(a)
# 
# # Number of class in hk data: 8507
# hk$count=1
# aggregate(count ~ classcode, data=hk,FUN =sum) -> no_class
# 
# # Number of class being duplicated: 1130
# sum(no_class[2]>=2)
# 
# #Number of coursrs in the data: 8507
# #and number of times of each course in hk data
# data.frame(table(hk$classcode)) -> hk_courselist
# hk_courselist[order(-hk_courselist[,2]),] -> hk_courselist
# 
# 
# # number of class of each lecturer
# hk$lms_hk <- ifelse(is.na(hk$LMS),0,1)
# aggregate(count ~ MaCBGD,data=hk, FUN=sum) -> lmsclass_lecturer
# 
# 
# # Number of Lecturer in hk data and how many classes they teach:
# # do not take into account the duplicated class
# table(hk$MaCBGD) -> lecturer
# lecturer <- data.frame(lecturer)
# 
# 
# #Number of course in data for each Falculty:
# aggregate(count ~ Khoa,data=hk, FUN=sum) -> faculty_class
# aggregate(count ~ Khoa + lms_hk,data=hk, FUN=sum) -> faculty_lms_class
# faculty_lms_class <- faculty_lms_class[order(faculty_lms_class$Khoa),]


# ---- Thong Tin Giang Vien va Lich Day---------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/")

# Load data into R
giangvien1 <- read.csv("11.06-giangvienDC.csv", stringsAsFactors=FALSE)
names(giangvien1$ï..MaHP) <- c("MaHP") # Not work
names(giangvien1[,1]) <- "MaHP" # Not work
names(giangvien1)[1]<-"MaHP"
#sapply(giangvien1,as.character)
#as.numeric(giangvien1$NamSinh)

# We should drop the variable MaHP in giangvien1 becaus
# this is not necessary for merging. Only MaCBGD is enough
#giangvien1 <- giangvien1[,-1]
giangvien1$MaHP<-NULL

# Merging Lecturers' information with hk data, 4062 observations
# It should be equal to the no. of obersvation of general classes in hk data
# because giangvien1 is the data of general classes
# full_hk_test <- full_join(giangvien1,hk, by=c("Khoa","BoMon","MaHP","MaCBGD"))
# Merging above is wrong
full_hk <- merge(hk, giangvien1, by.x = c("Khoa","BoMon","MaCBGD"), by.y = c("Khoa","BoMon","MaCBGD") )

# Keeping only lecturers of general classes, 4062.
# Infact, the no. of observation of this dataset should equal the full_hk dataset 
full_general_hk <- full_hk[rowSums(is.na(full_hk[, c("TrinhDo","ChucDanh","LanhDao","NamSinh","GioiTinh","X")]))!=6,]
remove(full_hk)

# # Count no. of general classes using LMS
# full_general_hk$lms_hk <- ifelse(is.na(full_general_hk$LMS),0,1)
# aggregate(lms_hk ~ Khoa,data=full_general_hk, FUN=sum) -> lmsclass_khoa

# ------ Extract the weight of each classes (the number of credits of each class) ------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/")
dc <- read_csv("daicuong2.csv")
names(dc)[2:4] <- c("MaHP","Name","weight")
weight <- dc[!grepl("FRE", dc$MaHP),c(2,4)]
gc <- weight[,1]
# Note: in general stage, K42 only studied 9 subjects (not including STA, HCM, ENG part 3)
# Note: some of them learn French
gc2 <- dc[,2]


#------- Diem Sinh Vien Chuong Trinh Tien Tien ---------------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTT")

Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTT", pattern="*.csv")
test <- lapply(Files, function(x) read.csv(x))

# Creat 26 score datasets for each of Majors
for (i in 1:26) {
  assign(paste("score",i, sep=""), data.frame(test[i])[-1,])
  assign(paste("zname",i, sep=""), data.frame(test[i])[1,])
}

# Combine 26 dataset into 1 dataset, there are 3070 students
score <- score1
for (i in 2:26) {
  a <- data.frame(test[i])[-1,]
  score <- full_join(score,a)
}

# Check mode of variables and change it to numeric
mode <- data.frame(sapply(score, mode))
score[, 7:505] <- sapply(score[, 7:505], as.character)
score[, 7:505] <- sapply(score[, 7:505], as.numeric)
score[,5] <- as.numeric(score[,5])
# mode1 <- data.frame(sapply(score, mode))

# Change the names of variables to ignore the problem of Vietnamese
colnames(score)[1:6] <- c("STT","MSSV","Ho","Ten","DTB","XL")
# Only keep scores of general courses in dataset gc
# i.e. ignoring the French courses, 
# which means ignoring students studying French major
general_score <-  data.frame(score[,1:6],score[as.vector(gc$MaHP)])
# general_score2 <-  data.frame(score[,1:6],score[as.vector(gc2$MaHP)])

# # Choosing names of general classes 
# numberstudent <- data.frame(colSums(!is.na(score)))
# numberstudent$row<-rownames(numberstudent)
# # General classes shoud have more than 2500 students
# general_class <- numberstudent[numberstudent[1]>2500,]
# # There are totally 18 variables, so we will choose var 7 to var 18
# listclass <- general_class[7:18,2]


# final data frame for general classes, 2842 (2922) students, but 24 (28)duplicated records
# Aim: delete observation having missing values.
final_score <- general_score[rowSums(is.na(general_score[, -c(1:6)]))==0,]
# final_score2 <- general_score[complete.cases(general_score[ , 7:15]),]
# We found 28 duplicated observations in final_score
# ==> 14 students study 2 majors
test <- final_score[duplicated(final_score$MSSV) | duplicated(final_score$MSSV, fromLast = TRUE),]

# -----------Chuong Trình Riêng --------------------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTR")

Files2 <- list.files(pattern="*.csv")
test <- lapply(Files2, function(x) read.csv(x))

# Creat 5 score datasets for each of Majors
for (i in 1:5) {
  assign(paste("score",i, sep=""), data.frame(test[i])[-1,])
  assign(paste("yname",i, sep=""), data.frame(test[i])[1,])
}

# Combine 5 dataset into 1 dataset, there are 334 students
score <- score1
for (i in 2:5) {
  a <- data.frame(test[i])[-1,]
  score <- full_join(score,a)
}

# Check mode of variables and change it to numeric
mode <- data.frame(sapply(score, mode))
# There are 166 variables
score[, 7:166] <- sapply(score[, 7:166], as.character)
score[, 7:166] <- sapply(score[, 7:166], as.numeric)
score[,5] <- as.numeric(score[,5])
# mode2 <- data.frame(sapply(score, mode))

# # Choosing names of general classes 
# numberstudent2 <- data.frame(colSums(!is.na(score)))
# numberstudent2$row<-rownames(numberstudent2)
# # General classes shoud have more than 200 students
# general_class2<- numberstudent2[numberstudent2[1]>200,]
# # There are totally 20 variables, so we choose var 7 to var 20
# listclass2 <- general_class2[7:20,2]
# # Note: MAT508003, MAT508004 are not general classes, so removing them
# listclass2 <- listclass2[-c(1,14)]

# Change the names of variables to ignore the problem of Vietnamese
colnames(score)[1:6] <- c("STT","MSSV","Ho","Ten","DTB","XL")
general_score2 <-  data.frame(score[,1:6],score[as.vector(gc$MaHP)])

# Create dummy variable 1 for theses classes
general_score2$specialmajor <- 1

# final data frame for general classes, 238 (301) students
# Aim: deleting observations having missing values
final_score2 <- general_score2[rowSums(is.na(general_score2[, -c(1:6)]))==0,]
#final_score2 <- general_score[complete.cases(general_score[ , 7:18]),]

# 2 duplicated observations ==> 1 students studies 2 majors
test2 <- final_score2[duplicated(final_score2$MSSV) | duplicated(final_score2$MSSV, fromLast = TRUE),]


# --------- Chuong Trinh Chat Luong Cao ------------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CLC")


Files3 <- list.files(pattern="*.csv")
test <- lapply(Files3, function(x) read.csv(x))

# Creat 6 score datasets for each of Majors
for (i in 1:6) {
  assign(paste("score",i, sep=""), data.frame(test[i])[-1,])
  assign(paste("xname",i, sep=""), data.frame(test[i])[1,])
}

# Combine 6 dataset into 1 dataset, there are 1297 students
score <- score1
for (i in 2:6) {
  a <- data.frame(test[i])[-1,]
  score <- full_join(score,a)
}

# Check mode of variables and change it to numeric
mode <- data.frame(sapply(score, mode))
# There are 166 variables
score[, 7:166] <- sapply(score[, 7:166], as.character)
score[, 7:166] <- sapply(score[, 7:166], as.numeric)
score[,5] <- as.numeric(score[,5])
# mode3 <- data.frame(sapply(score, mode))

# # Choosing names of general classes 
# numberstudent3 <- data.frame(colSums(!is.na(score)))
# numberstudent3$row<-rownames(numberstudent3)
# # General classes shoud have more than 800 students
# general_class3<- numberstudent3[numberstudent3[1]>800,]
# # There are totally 18 variables, so we choose var 7 to var 19
# listclass3 <- general_class3[7:18,2]

# Change the names of variables to ignore the problem of Vietnamese
colnames(score)[1:6] <- c("STT","MSSV","Ho","Ten","DTB","XL")
general_score3 <-  data.frame(score[,1:6],score[as.vector(gc$MaHP)])

# Create dummy variable 1 for theses classes
general_score3$highqualtymajor <- 1

# final data frame for general classes, 898 (925) students
final_score3 <- general_score3[rowSums(is.na(general_score3[, -c(1:6)]))==0,]
#final_score2 <- general_score[complete.cases(general_score[ , 7:18]),]

# 0 duplicated observations ==> 0 students study 2 majors
test3 <- final_score3[duplicated(final_score3$MSSV) | duplicated(final_score3$MSSV, fromLast = TRUE),]

## Create full data set of scores of all majors: 2922 + 301 + 925 = 4148 records
full_general_score <- full_join(final_score,full_join(final_score2,final_score3))

## Create dummy 0 for variables 19 and 20
full_general_score$specialmajor<-ifelse(is.na(full_general_score$specialmajor),0,1)
full_general_score$highqualtymajor<-ifelse(is.na(full_general_score$highqualtymajor),0,1)

# Droping some variables that make record become unique
full_general_score[,c(1,5:6)] <- NULL

# 66 duplicated observations ==> 33 students study 2 majors.
# This is bigger than 14 + 1 because some of them study CTTT and CTCLC or CTR
dup <- full_general_score[duplicated(full_general_score$MSSV) | duplicated(full_general_score$MSSV, fromLast = TRUE),]
dup$no.major <- 2
dup <- dup[order(dup$MSSV),]
dup1 <- dup[seq(1,66,by=2),]
dup2 <- dup[seq(2,66,by=2),]
dup1 <- data.frame(dup1,dup2[,13:14])
dup1$specialmajor<-ifelse((dup1$specialmajor==dup1$specialmajor.1)&(dup1$specialmajor==0),0,1)
dup1$highqualtymajor<-ifelse((dup1$highqualtymajor==dup1$highqualtymajor.1)&(dup1$highqualtymajor==0),0,1)

# Create dup_non data set that only has students learning 1 major
dup_non <- full_general_score[!(duplicated(full_general_score$MSSV) | duplicated(full_general_score$MSSV, fromLast = TRUE)),]
dup_non$no.major <- 1

# Merge dup and dup_non to create data with unique records: 4115 = 4148 - 33
full_general_score <- full_join(dup_non,dup1[,-c(19:20)])


# #Check
# sum(full_general_score[full_general_score$specialmajor==1,19])
# sum(full_general_score[full_general_score$highqualtymajor==1,20])
# sum(ifelse(full_general_score$highqualtymajor==0 & full_general_score$specialmajor==0,1,0))


#-------------- Student Info ---------------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/")

# Import data
studentinfo <- read.csv("Student-K42-Info.csv", stringsAsFactors=FALSE)
names(studentinfo)[1] <- "MSSV"

# Match student info to score dataset of full_general_score
# It should have the same observation with full_general_score
full_info_score_student <- merge(full_general_score, studentinfo, by = "MSSV")

# Change False True of Gender to 0 (Nu), 1 (Nam)
full_info_score_student$Gender[full_info_score_student$Gender==FALSE] <- 0
full_info_score_student$Gender[full_info_score_student$Gender==TRUE] <- 1

# Note: some K40, K41 students received their Degree, need to delete?


#--------- Keep some objects important -------

rm(list=setdiff(ls(), c("full_data", "full_general_hk","full_info_score_student","weight","gc","gc2","dc")))

# Change the values of LMS variable
full_general_hk$LMS <- ifelse(is.na(full_general_hk$LMS),0,1)

#df <- subset(df, select = -c(a, c)) # a,c is a name of column

#Keeping only classes of K42 students: 1923 classes in Thoi Khoa Bieu data
full_general_hk <- full_general_hk[grep("K42", full_general_hk$KhoaHoc),]
#The below command only takes observation having the string K42 at the end
#while the above takes all of observation having the string K42, even at the middle
#test1 <- subset(full_general_hk, substring(full_general_hk$KhoaHoc,nchar(full_general_hk$KhoaHoc)-3+1)=="K42")

# #Extract duplicated classes having at least 1 duplicate: 286 
# test <- full_general_hk[duplicated(full_general_hk$classcode),]
# #Extract duplicated classes having 2 or 3 duplicates:
# test2 <- test[duplicated(test$classcode),]

#Sum all of duplicated classes
#ddply(df,"x",numcolwise(sum))
t <- aggregate(cbind( LMS , TietLMS) ~ classcode, data = full_general_hk, sum)

#Extract unique classes: 1445
#Its number of observations should be equal to dataframe t
t1 <- full_general_hk[!duplicated(full_general_hk$classcode),]
#t2 <- full_general_hk %>% distinct(classcode, TenCBGD.x, .keep_all = T)

#Combine two variables of t to t1 datasets
#Aim: keeping only 1 record for one class and finding the LMS teaching hours
#and create full final hk data
names(t) <- c("classcode","dupclass","TimeLMS")
full_hk <- full_join(t1,t)

# # Check and find that Statistics and HCM is missing.
# # Minh Thanh is filling it
# names(full_info_score_student)[4:15] -> a
# full_general_hk[full_general_hk$MaHP %in% a,] -> b
# a
# table(b$MaHP)

# Note: there are some cases having LMS but not using LMS teaching hours

# Rename data sets
full_data -> lms
full_hk -> hk
full_info_score_student -> info
info[,c(16,17)] <- NULL

# Check if name of student is different for matching: no.
# a <- ifelse(info$Ho == info$Há.. & info$Ten == info$TÃªn,0,1)
# sum(a)

rm(list=setdiff(ls(), c("lms", "hk","info","weight","gc","gc2","dc")))
# write_delim(info,"info2.csv",delim=',')


# Input data of student code and class code:
# Year 2016:
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/Course-Student-code")

Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/Course-Student-code", pattern="*2016.csv")
# Files <- list.files(pattern="*2016.csv")
test <- lapply(Files, function(x) read.csv(x))

# Creat 3 score datasets for each of files
for (i in 1:3) {
  assign(paste("name",i, sep=""), data.frame(test[i]))
  #assign(paste("zname",i, sep=""), data.frame(test[i]))
}

# Combine 3 dataset into 1 dataset, there are 24172 = 6503 + 1235 + 16434 students
codecourse2016 <- full_join(name1,full_join(name2,name3))
# Check duplicated observations: no duplicated
check <- distinct(codecourse2016)

# Year 2017:
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/Course-Student-code")

Files2 <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/Course-Student-code", pattern="*2017*.csv")
# Files <- list.files(pattern="*2017*.csv")
test <- lapply(Files2, function(x) read.csv(x))

# Creat 3 score datasets for each of files
for (i in 1:3) {
  assign(paste("name",i, sep=""), data.frame(test[i]))
  #assign(paste("zname",i, sep=""), data.frame(test[i]))
}

# Combine 3 dataset into 1 dataset, there are 25967 = 2048 + 23388 + 531 students
codecourse2017 <- full_join(name1,full_join(name2,name3))
# Check duplicated observations: no duplicated
check2 <- distinct(codecourse2017)

# Keep some important data sets
rm(list=setdiff(ls(), c("weight","gc","gc2","codecourse2016","codecourse2017","info","lms","hk","dc")))

# Combine codecourse of 2016 and 2017
codecourse <- full_join(codecourse2016,codecourse2017)
# Change the name of variable
names(codecourse)[9] <- "classcode"
# Create the course code. Before doing that, need to know the its length
# nchar(codecourse$classcode) < 15 -> codecourse$a
# There are sime IC3 classes that only have 12 characters in classcode
# But it is not a problem if we only take the letters from 5th to 13th
# codecourse$MaHP <- substr(codecourse$classcode,5,13)


#------ Some analysis -------

# Convert score data from wide to long
names(info)[4:12] -> MaHP
info <- info[,-c(16,17)]
names(info)[-c(4:12)] -> id
# Score is the new variable capturing the grade of courses, 
# MaHP is the code of courses.
# Long format for all 9 genral courses
info_l<- reshape(info, 
             varying = MaHP,
             v.names = "score",
             timevar = "MaHP", 
             times = MaHP, 
             direction = "long")

# l <- reshape(hsb2, 
#              varying = c("read", "write", "math", "science", "socst"), 
#              v.names = "score",
#              timevar = "subj", 
#              times = c("read", "write", "math", "science", "socst"), 
#              new.row.names = 1:1000,
#              direction = "long")

# l <- reshape(hsb2, 
#              varying = c("read", "write", "math", "science", "socst"), 
#              v.names = "score",
#              timevar = "subj", 
#              times = c("read", "write", "math", "science", "socst"), 
#              new.row.names = 1:1000,
#              direction = "long")

# Merge codecourse to hk data to find the class code of courses of each student
# hk_coursecode <- merge(hk,codecourse,by=c("MSSV","MaHP"))
hk_coursecode <- merge(codecourse,hk,by="classcode")
# by.x = "classcode",by.y = "classcode")
# by = intersect(names(hk), names(codecourse)))
# by="classcode")

# Merge codecourse to info_l data 
final_info_l <- merge(info_l,hk_coursecode,by=c("MSSV","MaHP"),all.x = TRUE)
# Note: keeping all courses that do not appear in coursecode data
# i.e. students have grade but their names do not appear in exam list
# maybe they take the exam later, or they cover their English certificate

# Having some students who do not having class information: 
# 4029 full info, 4115 - 4029 = 86 students lacking class info
# final <- merge(info_l,hk_coursecode,by=c("MSSV","MaHP"))
# final$count <- 1
# aggregate(count ~ MSSV, data = final, sum) -> c
# sum(c == 9)

# Deleting 86 students having lack of inoformation
final <- merge(info_l,hk_coursecode,by=c("MSSV","MaHP"))
final$count <- 1
aggregate(count ~ MSSV, data = final, sum) -> c
c <- c[c$count==9,-2]
final_info_l <- final_info_l[final_info_l$MSSV %in% c,]
# Note: 4029 students * 9 = 36925 rows.


# Nam = 1, Nu = 0
final_info_l <- final_info_l[,-c(12,8,16,18,57,62,65,71,72,84,85,96:100,102:105)]
write_delim(final_info_l,"totalfinal.csv",delim=',')






# Lam thu voi Microeconomics ECO501001
micro <- info_l[info_l$MaHP=="ECO501001",] 

# Merge lms and hk to find the classcode. 12566 records
length(unique(hk$classcode)) # No duplicated classes
nrow(unique(lms[,c(1,4)])) # use nrow because it is a data frame. No duplicated
lmshk <- merge(lms,hk,by="classcode") # Only keeping general classes
nrow(unique(lmshk[,c("classcode","MSSV")]))

final_micro <- merge(micro,lmshk,by=c("MSSV","MaHP"),all.x=TRUE)
# Check duplicated values: 72 students use LMS two times
a <- final_micro[duplicated(final_micro$MSSV) | duplicated(final_micro$MSSV, fromLast=TRUE),]
# d <- unique(a[,1:4])
b <- a[duplicated(a$MSSV),]
c <- b[duplicated(b$MSSV),]



info$father <- ifelse(is.na(info$TÃªn.cha),0,1)
info$mother <- ifelse(is.na(info$MotherName),0,1)
info$chame <- ifelse(info$father==info$mother & info$mother==1,1,0)
table(info$chame)


#------ PCA and Factor Analysis
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(info[,4:15], cor=FALSE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

info[,4:15] -> data
# write_delim(data,"micro.csv",delim=',')
fit.2 <- factanal(data,factors=2,rotation="varimax")
fit.2

fit.3 <- factanal(data,factors=3,rotation="promax")
fit.3







merge(hk_courselist,hk,by.y="classcode",by.x="Var1") -> abc
abc[,c("Var1","Freq","TenTA")] -> hk_courselist
hk_courselist[order(-hk_courselist[,2]),] -> hk_courselist
duplicated_courselist<-unique(hk_courselist)

names(duplicated_courselist)[1] <- "classcode"
left_join(data,duplicated_courselist) -> fulldata2

hk_courselist2 <- hk_courselist[hk_courselist$TenTA!="Security and Defence Education",]


# Number of Lecturer in hk data:
table(hk$MaCBGD) -> lecturer
lecturer <- data.frame(lecturer)

# Number of class duplicated and not duplicated in hk data:
aggregate(count ~ classcode + MaHP,data=hk,FUN=sum) -> duplicated_class
sum(duplicated_class$count!=1)   # Splited class: 1008

# table of duplicated class based on the time of duplication
u <- duplicated_class[duplicated_class$count!=1,]
u <- u[order(-u$count),]
table(u$count) -> table_duplicated_class
u6 <- u[u$count==6,]
u12 <- u[u$count==12,]
u4 <- u[u$count==4,]
u3 <- u[u$count==3,]


a <- hk[hk$MaCBGD=="000393",]
a <- hk[hk$Khoa=="KTPT",]