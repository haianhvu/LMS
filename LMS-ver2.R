install.packages("readr")
install.packages("dplyr")
install.packages("foreign")
install.packages("haven")

library("readr")
library("dplyr")
library("foreign")
library("haven")
rm(list = ls())


setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")

# Import data into R: LMS from 10/2016 to 10/2017
data <- read_delim(file = "Thong_ke_LMS_K42.csv", delim=',')

# Check R read file correctly or not by using Excel and import csv file
# into excel by Data -> From CSV/Text 
#write_delim(data,"aaa.csv",delim=',') 

# make sure there is no duplication: 31200
unique(data) -> data

# Extract MSSV from the first column
a <- substr(data$`Sinh vi�n`,1,11)
full_data <- cbind(a,data)
names(full_data)[1] <- "MSSV"

# Before continuing, we only keep 9 General Classes of K42 ChinhQuy
dc <- read_csv("daicuong2.csv")
names(dc)[2:4] <- c("MaHP","Name","weight")
weight <- dc[!grepl("FRE", dc$MaHP),c(2,4)]
gc <- weight[,1]
chooselist <- gc[[1]]
names(full_data)[4] <- "MaHP"
a <- full_data[ grepl(paste(chooselist, collapse="|"), full_data$MaHP), ]
full_data <- a      #10187 observation: only 9 general courses

fulldata<-unique(full_data)    # fulldata is just data of LMS only
# Rename variables 1 to 4 from Vietnamese to English
names(fulldata)[2:5] <- c("student","course","classcode","download")

# import data into R: schdule of 3 semester: 10/2016 - 3/2017
hkc2016 <- read_csv("HKC2016.csv",col_types = cols(MaHP = "c"))
hkc2017 <- read_csv("HKC2017.csv",col_types = cols(MaHP = "c"))
hkd2017 <- read_csv("HKd2017.csv",col_types = cols(MaHP = "c"))


# Rename key variable of 3 hk dataset for combining and merging
names(hkc2016)[7] = "classcode"
names(hkc2017)[7] = "classcode"
names(hkd2017)[7] = "classcode"
hk <- rbind(hkc2016,hkc2017,hkd2017)
# Number of class schedule: 10089 = 2978 + 3761 + 3350
hk <- unique(hk)


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

# Create class cluster id (group id): 531 classes using LMS
full_data <- transform(fulldata,id=as.numeric(factor(classcode)))
# fulldata is just data of LMS only


#remove object in R
#rm(list = ls(pattern = "score*"))
#remove(list=ls())
#rm(list = apropos("score_"))



#------ Some descriptive statistics for the datasets --------

# # number of time each course was given in the data student
# unique(full_data$course) -> courselist    
# length(courselist)                          # 61 courses
# dim(courselist)=c(61,1)
# table(full_data$course) -> tab              # number of appearence of each course
# data.frame(tab) -> times_for_course
# dim(times_for_course)
# 
# Number of classes of each course:
# a <- table(unique(full_data[,3:4])[1])
# view(a)
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
# # number of class of each lecturer
# hk$lms_hk <- ifelse(is.na(hk$LMS),0,1)
# aggregate(count ~ MaCBGD,data=hk, FUN=sum) -> lmsclass_lecturer
# 
# # Number of Lecturer in hk data and how many classes they teach:
# # do not take into account the duplicated class
# table(hk$MaCBGD) -> lecturer
# lecturer <- data.frame(lecturer)
# 
# # Number of course in data for each Falculty:
# aggregate(count ~ Khoa,data=hk, FUN=sum) -> faculty_class
# aggregate(count ~ Khoa + lms_hk,data=hk, FUN=sum) -> faculty_lms_class
# faculty_lms_class <- faculty_lms_class[order(faculty_lms_class$Khoa),]


# ---- Thong Tin Giang Vien va Lich Day---------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/")

# Load data into R: data giangvien created by Thanh 
# (16 courses includes 4 course of Physics, so only 12 general courses)
giangvien1 <- read.csv("11.06-giangvienDC.csv", stringsAsFactors=FALSE)
# giangvien1 is the data of teachers of general_class.
# at the beginning, I think general_class is 12 classes. (but it is only 9)
names(giangvien1$�..MaHP) <- c("MaHP") # Not work
names(giangvien1[,1]) <- "MaHP" # Not work because this is just a vector, not dataframe
names(giangvien1)[1]<-"MaHP"
sapply(giangvien1,as.character)
as.numeric(giangvien1$NamSinh)

# We should drop the variable MaHP in giangvien1 becaus
# this is not necessary for merging. Only MaCBGD is enough
#giangvien1 <- giangvien1[,-1]
giangvien1$MaHP<-NULL

# Merging Lecturers' information with hk data, 4301 observations
# It should be equal to the no. of obersvation of general classes in hk data
# because giangvien1 is the data of general classes
# full_hk_test <- full_join(giangvien1,hk, by=c("Khoa","BoMon","MaHP","MaCBGD"))
# Merging above is wrong
test <- merge(hk,giangvien1,by=c("Khoa","BoMon","MaCBGD"),all=TRUE) # 10089 observations
# Note: here, we only keep matched data, it means only data of 
# teacher and HK of general courses (12 courses). So only 4301 obs
full_hk <- merge(hk, giangvien1, by.x = c("Khoa","BoMon","MaCBGD"), by.y = c("Khoa","BoMon","MaCBGD") )
# 4301 observations: characteristics of general teachers and classes

# Keeping only lecturers of general classes, 4301.
full_general_hk <- full_hk[rowSums(is.na(full_hk[, c("TrinhDo","ChucDanh","LanhDao","NamSinh","GioiTinh","X")]))!=6,]
# Infact, the no. of observation of this dataset should equal the full_hk dataset
# because information of TrinhDo, ChucDanh, ... only appear for 
# general course teacher. Major course do not have this information
#remove(full_hk)

# # Count no. of general classes using LMS
# full_general_hk$lms_hk <- ifelse(is.na(full_general_hk$LMS),0,1)
# aggregate(lms_hk ~ Khoa,data=full_general_hk, FUN=sum) -> lmsclass_khoa

# ------ Extract the weight of each classes (the number of credits of each class) ------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/")
# Here, general courses only have 11 courses including 2 courses of FRE
dc <- read_csv("daicuong2.csv")
names(dc)[2:4] <- c("MaHP","Name","weight")
weight <- dc[!grepl("FRE", dc$MaHP),c(2,4)]
gc <- weight[,1]
# Note: in general stage, K42 only studied 9 subjects (not including STA, HCM, ENG part 3)
# Note: some of them learn French (not really)
gc2 <- dc[,2]

# full_general_hk has information of classes (classize, time, ...) and information
# of teachers. So we can now merge this data with LMS data named full_data
# Note: only general classes and general teachers are included 
# in full_general_hk. So lmsfull only for general course.
# Note: keep all of observation of full_general_hk because full_data
# only has LMS of LMS class while not all of classes has LMS
lmsfull <- merge(full_data, full_general_hk, by = "classcode", all = TRUE)
# 17550 observations of lms and hk of general course
table(lmsfull$KhoaHoc)    # 17550 DHCQ K42
# note: general courses here maybe taught for CQ, VB2, CLC, HPR, HPC, ...
# reason: the hk data is data of all types of students, not CTTT of K42
lmsfull <- lmsfull[lmsfull$KhoaHoc == "DHCQK42",] #11705
# check observations with all NA values: 184 records (184 + 11521 = 11705)
# and delete them
a <- lmsfull[is.na(lmsfull$KhoaHoc),]  # 184 observations
a <- na.omit(a)     # 0 observation, so no any information for these 184 records
lmsfull <- lmsfull[!is.na(lmsfull$KhoaHoc),]  # so, only keep 11521

# NOTE: not all teacher has LMS, so in lmsfull data will have some
# records that have missing values in MSSV, Download, Quiz. 
# Therefore, must be carefull because some observations have 
# all classcode, MSSV, MaCBGD while some only have 2 of them, i.e. 
# classcode + MaCBGD (no MSSV) or MSSV + MaCBGD (no classcode) 
# or classcode + MSSV (no MaCBGD)

# Some classes has two different records due to change the lecture time
# but still having the same other characteristics: teacher, room, class size,...
# check whether having classes that change teachers?
a <- lmsfull[duplicated(lmsfull[,c(1,2,24)]),] # column 1,2,24:classcode,MSSV,Teacher ID
b <- lmsfull[duplicated(lmsfull[,c(1,2)]),]
setdiff(a,b)   # 0 observations, so no classes that change teachers
setdiff(b,a)   # 0 observations
identical(a,b)  # this means there is no class that changed teachers.

# note: some classes may be lectured by 2 teachers, but maybe we need to
# check, but now we skip it for simplicity.

# Therefore, we can drop 1 duplicated record of each observation
c <- lmsfull[duplicated(lmsfull[,c(1,2,24)]),]
d <- lmsfull[duplicated(lmsfull[,c(1,2,24)]) | duplicated(lmsfull[,c(1,2,24)], fromLast = TRUE),]  # 3350 
lmsfull1 <- lmsfull[!duplicated(lmsfull[,c(1,2,24)]),]   # 8354

# ------------------------------------------------------------------
# Input MSSV and classcode
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/Course-Student-code")

# Import data into R: HKC 2016 of CTT and HKD2017
Files4 <- list.files(pattern="*.csv")
test <- lapply(Files4, function(x) read.csv(x))

# Creat 6 score datasets for each of HK
for (i in 1:6) {
  assign(paste("code",i, sep=""), data.frame(test[i]))
  assign(paste("xname",i, sep=""), data.frame(test[i])[1,])
}

# Check existence of CTT: 
for (i in test) {
  data.frame(i$Khóa) -> z
  print(table(z))
}
# In code3, there are 249 CTT, but they are not general course, so do not include
# so, only include code2 and code6

# Combine 2 dataset into 1 dataset due to only CTT
code2$Ng�.nh.Chuyên.ng�.nh <- NULL
code <- full_join(code6,code2)
nrow(code) == nrow(code2) + nrow(code6) # good: 39822
# only keep CTT
code <- code[grepl("CTT", code$Khóa),] # 32855

# check
table(code$H�..c.ph�.n) -> a  # 30 classes

# If only keep 9 courses, we will loose other classes that can be
# used to analyse effect of LMS.
# But we need to analyse effect of LMS in 1 specific course in order to
# have enough number of observations.
code$MaHP <- substr(code$L�..p.h�..c.ph�.n,5,13)
generalcode <- code[ grepl(paste(chooselist, collapse="|"), code$MaHP), ]  #28307
# Having some students testing 14 courses:
b <- data.frame(table(generalcode$MSSV))
max(b$Freq)               
table(b$Freq)   # some tested 1 times, 2 times, ..., 9 times
# there are 2840 MSSV testing 9 times

#------- Diem Sinh Vien Chuong Trinh Tien Tien ---------------
# Note: the information includes all of score of students of HK1, HK2, HK3
# so we firstly imput all of data, and then we only use gc$MaHP to keep
# only 9 general courses. So the final data only has general_course

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTT")

Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTT", pattern="*.csv")
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
score[, 7:522] <- sapply(score[, 7:522], as.character)
score[, 7:522] <- sapply(score[, 7:522], as.numeric)
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

# -----------Chuong Tr�nh Ri�ng --------------------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTR")

Files2 <- list.files(pattern="*.csv")
test <- lapply(Files2, function(x) read.csv(x)) # Co ca thay 4 file trong test

# Creat 4 score datasets for each of Majors
for (i in 1:4) {
  assign(paste("score",i, sep=""), data.frame(test[i])[-1,])
  assign(paste("yname",i, sep=""), data.frame(test[i])[1,])
}

# Combine 4 datasets into 1 dataset, there are 334 students
score <- score1
for (i in 2:4) {
  a <- data.frame(test[i])[-1,]
  score <- full_join(score,a)
}

# Check mode of variables and change it to numeric
mode <- data.frame(sapply(score, mode))
# There are 142 variables
score[, 7:142] <- sapply(score[, 7:142], as.character)
score[, 7:142] <- sapply(score[, 7:142], as.numeric)
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

# final data frame for general classes, 189 students
# Aim: deleting observations having missing values
final_score2 <- general_score2[rowSums(is.na(general_score2[, -c(1:6)]))==0,]
#final_score2 <- general_score[complete.cases(general_score[ , 7:18]),]

# 2 duplicated observations ==> 0 students studies 2 majors
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
# There are 203 variables
score[, 7:203] <- sapply(score[, 7:203], as.character)
score[, 7:203] <- sapply(score[, 7:203], as.numeric)
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
# This is bigger than 17  because some of them study CTTT and CTCLC or CTR
dup <- full_general_score[duplicated(full_general_score$MSSV) | duplicated(full_general_score$MSSV, fromLast = TRUE),]
dup$nomajor <- 2
dup <- dup[order(dup$MSSV),]
dup1 <- dup[seq(1,66,by=2),]
dup2 <- dup[seq(2,66,by=2),]
dup1 <- data.frame(dup1,dup2[,13:14])
dup1$specialmajor<-ifelse((dup1$specialmajor==dup1$specialmajor.1)&(dup1$specialmajor==0),0,1)
dup1$highqualtymajor<-ifelse((dup1$highqualtymajor==dup1$highqualtymajor.1)&(dup1$highqualtymajor==0),0,1)

# Create dup_non data set that only has students learning 1 major
dup_non <- full_general_score[!(duplicated(full_general_score$MSSV) | duplicated(full_general_score$MSSV, fromLast = TRUE)),]
dup_non$nomajor <- 1

# Merge dup and dup_non to create data with unique records: 4115 = 4148 - 33
full_general_score <- full_join(dup_non,dup1[,-c(19:20)])
sum(complete.cases(full_general_score[,1:15])) # 4115 students have all 9 courses

# #Check
# sum(full_general_score[full_general_score$specialmajor==1,19])
# sum(full_general_score[full_general_score$highqualtymajor==1,20])
# sum(ifelse(full_general_score$highqualtymajor==0 & full_general_score$specialmajor==0,1,0))

reshape(full_general_score, direction = "long", 
        varying = list(names(full_general_score)[4:12]), 
        v.names = "score", idvar = c("MSSV"), 
        timevar = "coursecode",
        times = names(full_general_score)[4:12]) -> long_score
# 4115*9 = 37035, matched to the number of records in long format

#--------------------------------------------------
# Create var coursecode for lmsfull data
# Note: lmsfull data is just data inlcude all general classes and teachers.
# some of these classes had LMS some did not.
lmsfull <- lmsfull[ grepl(paste(chooselist, collapse="|"), lmsfull$MaHP), ]
# 10702 observ 
# it is different from 11705 due to some classes that are not general
# check:
# y <- setdiff(lmsfull1,lmsfull)

lmsfull$a <- sapply(lmsfull$classcode,nchar)
table(lmsfull$a)   # classcode length is: 13,15,16 characters 
# # check what course has code of 13,15,16 characters (note:15 is normal)
# table(lmsfull[lmsfull$a == 13, ]$TenHP)
# table(lmsfull[lmsfull$a == 15, ]$TenHP)
# table(lmsfull[lmsfull$a == 16, ]$TenHP)
table(lmsfull$KhoaHoc)    
a <- lmsfull[is.na(lmsfull$KhoaHoc),]    # 0 observations
b <- na.omit(a)                           # all of them have no any data
lmsfull <- lmsfull[!is.na(lmsfull$KhoaHoc),]

# Mon Tieng Anh was lectured in 100 classes, so classcode has 16 characters
# Others are course of IT, not taken into account in the calculation of AGP
# lmsfull$coursecode <- substr(lmsfull$classcode,5,13)
# sum(is.na(lmsfull$MaHP))     # 0 missing values
# sum(is.na(lmsfull$coursecode))  # 0 missing value
# check coursecode is the same as MaHP.
#sum(isTRUE(lmsfull$MaHP == lmsfull$coursecode)) 
# lmsfull$ma <- ifelse(lmsfull$MaHP == lmsfull$coursecode,0,1)
# a <- lmsfull[is.na(lmsfull$ma),]
# #Yes they are the sam, so we use MaHP and change the name
# names(lmsfull)[c(29,58)] <- c("coursecode","Coursecode")

# ------- Check LMS of hk data and LMS of IT-department data----------------
# chooselist <- gc[[1]]
# gchk <- hk[ grepl(paste(chooselist, collapse="|"), hk$MaHP), ]
# gchk$LMS <- ifelse(is.na(gchk$LMS),0,1)
# Idea: TietLMS = 0 means no LMS, download is NA means no LMS (in lmsfull)
# NOte: there may be that some class do not announce it has LMS although
# teachers register LMS section, so no student log in into LMS portal
lmsfull$itlms <- ifelse(is.na(lmsfull$download),0,1)
lmsfull <- lmsfull[,c(1:47,49:58,48)]
# Eliminiate duplicated observations of lmsfull because of NA in MSSVl
lmsfull <- lmsfull[!duplicated(lmsfull[,c("MaCBGD","classcode","MSSV")]),]
# 7579 observations
# b <- lmsfull[duplicated(lmsfull[,c("MaCBGD","classcode","MSSV")]) |
#     duplicated(lmsfull[,c("MaCBGD","classcode","MSSV")], fromLast = TRUE),]
# c <- lmsfull[duplicated(lmsfull[,c("MaCBGD","classcode","MSSV")]),]
# giam tu 11521 xuong con 8354

# test which class has TietLMs but not having download information
test <- lmsfull[lmsfull$itlms==0 & lmsfull$TietLMS!=0,] # 20 classes
levels(as.factor(test$TenHP))      # these 20 classes include: KT Vi Mo, ENG 1, ENG 2, Mac Lenin
# check: 20 too
b <- test[!duplicated(test[,c("MaCBGD","classcode","MSSV")]),] 

# Check classes using LMS but have no TietLMS
lmsfull[is.na(lmsfull$LMS),]$LMS <- 0
test1 <- lmsfull[lmsfull$LMS=="X" & lmsfull$TietLMS==0,] #0 observations
# note: maybe teachers registered LMS but do not use LMS to reduce lecture time

# nlevels(factor(test1$MSSV))  #3273 MSSV
# levels(factor(test1$TenTA))  # 18 courses

# check classes using download but have no TietLMS
test2 <- lmsfull[lmsfull$itlms!=0 & lmsfull$TietLMS==0,]  # 507 obser
levels(factor(test2$classcode))                           # 5 classes
test2a <- lmsfull[lmsfull$itlms!=0 & lmsfull$LMS==0,]     # the same 507
levels(factor(test2$classcode))                           # the same 5 classes

# Check classes that do not uses LMS but havign TietLMS
#test1 <- lmsfull[is.na(lmsfull$LMS) & ,]

# check complete observations of lmsfull data
#new_DF <- DF[rowSums(is.na(DF)) > 0,]
#Df[Df=='NA'] <- NA
test3 <- lmsfull[rowSums(!is.na(lmsfull)) == ncol(lmsfull),] # 7258 completed observations

# why lmsfull1: 8354, lmsull:7579
setdiff(lmsfull1[,1:36],lmsfull[,1:36]) -> a
levels(factor(a$TenMH))        # all of them are not general course, except Micro, Macro (EN), so no problem
# Reason: some of these class are duplicated. They are general courses,
# but duplicated, so we eliminted them to get 7579.

#---------------------------------
# merge long_score and code
names(long_score)[9] <- "MaHP"
a <- aggregate(MaHP ~ MSSV, data=long_score, NROW)  #4115 MSSV in long_score
scorecode <- merge(long_score,generalcode,by=c("MSSV","MaHP"),all=TRUE)
# 38629: long_score has 4115 MSSV, but generalcode only have CTT and others major
# e.g: Anh Van Thuong Mai (CTT), HPR, ... (code2 still includes them) 
# and some students do not exist in long_score

# check no. of students of each course
table(scorecode$MaHP)
# only keep students that appears 9 times
a <- aggregate(MaHP ~ MSSV, data=scorecode, NROW)
table(a$MaHP) #4164 studetns took 9 tests
scorecode2 <- merge(scorecode,a,by="MSSV")
scoredoe3 <- scorecode2[scorecode2$MaHP.y==9,]

# some of studetns do not have all classcode, check and why
scorecode$b <- ifelse(is.na(scorecode$�..STT),0,1)   # i...STT is of class schedule
# so if it is NA in scorecode, it means there is 
# no class matched to this student. Lack of this class for this student

a <- aggregate(b ~ MSSV, data=scorecode, sum)
scorecode2 <- merge(scorecode,a,by="MSSV")
scorecode3 <- scorecode2[scorecode2$b.y==9,]    # 25560 records: 2840 students
# Reasons: because we only keep some classes of code2 and code6 and
# some students have MSSV 3110, 3111, 3112, ... not 3116xxxx (of K42)
# they are re-test student, who is not K42. K42 was born in 1998, 
# these missing-value students were born in 1992,1993, ...
# note: all LMS are 3116 student (k42 student)

# Vai sinh vien cua Khoa truoc 3115 se chuyen diem xuong K42 3116, 
# nen co diem trong long_Score, nhung khong co danh sach thi code1,2,3...
# hoac la trong file code, ta chi dung code2 va code6, cac code khac bo
# nen danh sach lop thi va MSSV khong gom cac sinh vien nay
# kiem tra bang cach xem scorecode2, hai bien b.x=0 va b.y=0
# how many students are there in this situation?
data.frame(unique(scorecode2[scorecode2$b.y==0,]$MSSV)) -> a #1066 students

# Check data appear in generalcode but not in long_score:
scorecode$c <- ifelse(is.na(scorecode$Ho),0,1)
scorecode2 <- scorecode[scorecode$c==0,]     #1594 observations
table(substr(scorecode2$MSSV,1,4))    #1346 observations of 3116

# scorecode3 has 25560 obser has 9 tests, but how is it for long_score?
scorecode3$c <- ifelse(is.na(scorecode3$Ho),0,1)
scorecode4 <- scorecode3[scorecode3$c==0,]    # 441 obsrvaions
length(unique(scorecode4$MSSV))               # 49 students
scorecode4 <- scorecode3[scorecode3$c==1,]    # 25119 = 2791 students

# check difference between long_score (4115 students) and scorecode4 (2840)
a <- unique(long_score[,1]) #4115 students
b <- unique(scorecode3[,1]) #2840 students
c <- unique(scorecode4[,1])
data.frame(setdiff(a,b)) -> d
data.frame(setdiff(a,c)) -> e

#------------------------------------------------------------------
# merge scorecode and lmsfull data
names(scorecode4)[17] <- "classcode"
fullfinal <- merge(scorecode4,lmsfull,by=c("MSSV","classcode"),all=TRUE) #26379
# scorecode4 = 25119, lmsfull=7579

fullfinal2 <- merge(scorecode4,lmsfull,by=c("MSSV","classcode"))
# lmsfull co mot vai sinh vien khong xuat hien, nhu 31151020256
# nhung sinh vien nay lai co trong scorecode4
# nhung lop cua sinh vien nay lai co trong lmsfull
# cho nen khi match, sinh vien nay khong co thong tin giao vien
# mac du cac ban sinh vien khac cung lop lai co 
# (do co nhung sinh vien khac hoc lop nay xuat hien trong lmsfull)

# cho nen tot nhat la lay scorecode4 merge voi general_hk va lms
general_hk <- full_general_hk[grepl(paste(chooselist, collapse="|"), full_general_hk$MaHP), ]
table(general_hk$KhoaHoc)
general_hk2 <- general_hk[which(general_hk$KhoaHoc=="DHCQK42"),] # keep only K42 CTT

score_hk <- merge(scorecode4,general_hk2, by=c("classcode"))
b <- aggregate(classcode~MSSV, data=score_hk, length)
table(b$classcode)

# can loai bo cac lop bi duplicated do doi gio giang
#check check LMS and Tiet LMS = 0
#wrong command due to NA value in LMS, R still keep it.
#a <- general_hk[general_hk$LMS=="X" & general_hk$TietLMS==0,]
a <- subset(general_hk2,general_hk$LMS=="X" & general_hk$TietLMS==0)
table(a$GioHoc) #GioHoc van the hien 5 Tiet. Khong biet la co cat LMS khong
# # Do not Sum Tiet LMS before deleting
# a <- aggregate(general_hk$TietMS ~ general_hk$classcode,general_hk,sum)

# # NOTE: do not sum TietLMS because it is just repeated, it does not mean
# # that each seperated classes has TietLMS by themself (eg 16C1ENG51300195)

# # Check class having same name but different LMS check
b <- aggregate(LMS ~ classcode + TietLMS, data=general_hk2, length)
sum(duplicated(general_hk2[,c(8,28)])) # results 192
sum(duplicated(general_hk2[,c(8)]))    # 192, 
# so we are sure that no seperated class have different LMS check.
sum(duplicated(general_hk2[,c(8,28,29)]))  #192
# so we are sure that no seperated class have different TietLMS.
# delete seperated classes
general_hk3 <- general_hk2[!duplicated(general_hk2[,c(8,28,29)]),] # 329

# merge score and schedule
score_hk <- merge(scorecode4,general_hk3, by=c("classcode")) # 25119
# scorecode4: 25119 (2791 students CTT K42)
sum(is.na(score_hk$MSSV)) + sum(is.na(score_hk$classcode))

# merge score, schedue and LMS
full_final <- merge(score_hk, full_data, by=c("MSSV","classcode")) #6319
# score_hk:25119, full_data:10187, and only 6319 matched
# of course, because full_data includes classes of LMS only, not all class
full_final <- merge(score_hk, full_data, by=c("MSSV","classcode"),all.x = TRUE)
# 25519 as we keep all of records of score_hk

# some initial checkt
full_final$a <- ifelse(is.na(full_final$student),0,1) 
full_final <- transform(full_final, classid=as.numeric(factor(classcode)), 
                            studentid=as.numeric(factor(MSSV))) #316 classes
full_final <- full_final[order(full_final$classid),]            # 2791 id
a <- aggregate(a ~ classid, data=full_final, mean)
table(a$a) # 231 classes no LMS, 58 class have all student using LMS

full_final$b <- ifelse(is.na(full_final$MaCBGD),0,1) 
b <- aggregate(b ~ studentid, data=full_final, mean)
table(b$b) # 2791 of number 1, means every records have class information

# make some analysis
# how many classes for each course
sum(full_final$MaHP.x == full_final$MaHP.y) #ok
a <- aggregate(classcode ~ MaHP.x, data=full_final, length) # 2791 / course

# LMS and no LMS for each course:
table(full_final$MaHP.x,full_final$LMS, exclude = NULL)
# so we can only use ECO1, ECO2, PML1, PML2, ENG1, ENG2 to analyse LMS
# note: all students have 9 scores

sum(is.na(full_final$specialmajor))
sum(is.na(full_final$highqualtymajor))
table(full_final$nomajor)

full_final$LMS <- ifelse(is.na(full_final$LMS),0,1)
full_final[,c("specialmajor.1","highqualtymajor.1")] <- NULL

# Count no. of classes of each courses with LMS and noLMS
a <- NULL
for (i in chooselist) {
  x <- full_final[full_final$MaHP.x == i,]
  y <- length(unique(x[x$LMS == 0,]$classid))
  z <- length(unique(x[x$LMS == 1,]$classid))
  a <- rbind(a,data.frame("course" = paste(i),"noLMS"=y,"LMS"=z,"total"=y+z))
}
View(a)

#


#------------------------------------------------------------------

# # merge long_score and lmsfull data
# lmsfinal <- merge(lmsfull,long_score, by = c("MSSV","MaHP"), all = TRUE)
# 
# # (due to they are general course)
# # merge and keep only matched data: 32891
# lmsfinal2 <- merge(lmsfull,long_score, by = c("MSSV","MaHP"))
# lmsfinal2[c("X","specialmajor.1","highqualtymajor.1")] <- NULL
# 
# # check duplicated class of teacher and students
# sum(duplicated(lmsfinal2[,c(1,3,25)]))  # 3055(98007). want to know which student and class
# a <- lmsfinal2[duplicated(lmsfinal2[,c(1,3,25)]) | duplicated(lmsfinal2[,c(1,3,25)], fromLast = TRUE),]
# # Note: 6xxx (101676) observations which inlcudes 1 original and some duplicated
# # e.g: appearing 3 times = 1 original + 2 duplicated
# # some students appear 2 times, some appear 3 times
# # reason: classes are seperated into 2 parts due to the university reinnovation
# 
# # Check the number of class each student study in lmsfinal2
# # They infact have 2 or 3 LMS class for the same course because they
# # have exercise class or because the schedule seperates one class into
# # 2 parts because of innovation.
# lmsfinal2 <- transform(lmsfinal2, group=as.numeric(factor(MSSV)))
# table(lmsfinal2$group) # students have different number of LMS class
# # Create the variable counting the number of LMS classes
# a <- aggregate(group ~ MSSV, data=lmsfinal2,
#                                 FUN = function(x){NROW(x)})
# names(a)[2] <- "totalLMSclass"
# lmsfinal2 <- merge(lmsfinal2,a,by="MSSV")
# 
# # Drop duplicated observation
# # But before that, need to determined TietLMS for duplicated classes.
# lmsfinal2 <- lmsfinal2[order(lmsfinal2$MSSV),]
# # infact, all of duplicated class have the same TietLMS?
# lmsfinal3 <- lmsfinal2[!duplicated(lmsfinal2[,c(1:3,25,49)]),] # 9004 observations
# 
# # Recode some variable:
# lmsfinal3$GioiTinh <- ifelse(lmsfinal3$GioiTinh == "Nam",0,1) # Male: 0  Female:1
# lmsfinal3$TrinhDo <- ifelse(lmsfinal3$TrinhDo == "Thac si",0,ifelse(lmsfinal3$TrinhDo == "Tien si",1,NA))

# How many observations do not have LMS class
sum(is.na(lmsfinal3$LMS)) # 719 no LMS
sum(!is.na(lmsfinal3$LMS))  # 8285 = 9004 - 719: having LMS

# Check score variable is missing and calculate Mean Difference
sum(is.na(lmsfinal3$score)) # 0 missing values
sum(is.na(lmsfinal3$TietLMS)) # 0 missing values
lmsfinal3$lms <- ifelse(is.na(lmsfinal3$LMS),0,1)
effect <- lm(score ~ lms , data=lmsfinal3)
effect
summary(effect)    # "naive estimator" is significant with negative sign

# check effect for each course
levels(factor(lmsfinal3$coursecode)) # only 7 courses, why?
# reason: we only keep the teacher and the score of general course
summary(lm(score ~ lms , data=lmsfinal3[lmsfinal3$coursecode == "ENG513001",])) # not sig
summary(lm(score ~ lms , data=lmsfinal3[lmsfinal3$coursecode == "ENG513002",])) # sig 5%, positive
summary(lm(score ~ lms , data=lmsfinal3[lmsfinal3$coursecode == "ECO501001",])) # sig 2%, negative
summary(lm(score ~ lms , data=lmsfinal3[lmsfinal3$coursecode == "ECO501002",])) # sig 0%, negative
summary(lm(score ~ lms , data=lmsfinal3[lmsfinal3$coursecode == "ACC507001",])) #
summary(lm(score ~ lms , data=lmsfinal3[lmsfinal3$coursecode == "PML510001",])) # sig 0%, negative
summary(lm(score ~ lms , data=lmsfinal3[lmsfinal3$coursecode == "PML510002",])) #

# check complete observation in data
sum(complete.cases(lmsfinal3))   # 8096 of 9004
# check treatment and control for PML510002 and ACC507001
table(lmsfinal3[lmsfinal3$coursecode == "PML510002",]["lms"])
table(lmsfinal3[lmsfinal3$coursecode == "ACC507001",]["lms"])

# Include some variables:
summary(lm(score ~ lms + download + Assignment + Quiz + Scorm
           + Forum + Chat + SoSV + TietLMS + TrinhDo + NamSinh + GioiTinh
           , data=lmsfinal3))

# Cluster
install.packages("lmtest")
install.packages("multiwayvcov")
library("lmtest")
library(multiwayvcov)

m1 <- lm(score ~ lms + download + Assignment + Quiz + Scorm
   + Forum + Chat + SoSV + TietLMS + TrinhDo + NamSinh + GioiTinh
   , data=lmsfinal3)

vcov_class <- cluster.vcov(m1, lmsfinal3$id) # here is class id
coeftest(m1, vcov_class) # not very significant

vcov_stu <- cluster.vcov(m1, lmsfinal3$MSSV)
coeftest(m1, vcov_stu) # very significant

vcov_both <- cluster.vcov(m1, cbind(lmsfinal3$MSSV,lmsfinal3$id))
coeftest(m1, vcov_both) # not very significant

# Here, we include all of courses, so they may be diffrent in difficulty and exam
# it is should seperated by course before run regression
# or by group of courses that are factored: 
r <- lmsfinal3[lmsfinal3$coursecode == "PML510001",]

unique(r$MSSV) 
unique(r$id)

m1 <- lm(score ~ lms + download + Assignment + Quiz + Scorm
         + Forum + Chat + SoSV + TietLMS + TrinhDo + NamSinh + GioiTinh
         , data=r)

vcov_class <- cluster.vcov(m1, r$id) # here is class id
coeftest(m1, vcov_class) # not very significant

vcov_stu <- cluster.vcov(m1, r$MSSV)
coeftest(m1, vcov_stu) # very significant

vcov_both <- cluster.vcov(m1, cbind(r$MSSV,r$id))
coeftest(m1, vcov_both) # very significant             
