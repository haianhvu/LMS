install.packages("readr")
install.packages("dplyr")
install.packages("foreign")
install.packages("haven")
install.packages("reshape2")

library("readr")
library("dplyr")
library("foreign")
library("haven")
library(reshape2)

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
a <- substr(data$`Sinh viÍn`,1,11)
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
full_data <- a      #10187 observation

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
names(giangvien1$Ô..MaHP) <- c("MaHP") # Not work
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
  data.frame(i$Kh√≥a) -> z
  print(table(z))
}
# In code3, there are 249 CTT, but they are not general course, so do not include
# so, only include code2 and code6

# Combine 2 dataset into 1 dataset due to only CTT
code2$Ng√.nh.Chuy√™n.ng√.nh <- NULL
code <- full_join(code6,code2)
nrow(code) == nrow(code2) + nrow(code6) # good: 39822
# only keep CTT
code <- code[grepl("CTT", code$Kh√≥a),] # 32855

# check
table(code$H·..c.ph·∫.n) -> a  # 30 classes

# If only keep 9 courses, we will loose other classes that can be
# used to analyse effect of LMS.
# But we need to analyse effect of LMS in 1 specific course in order to
# have enough number of observations.
code$MaHP <- substr(code$L·..p.h·..c.ph·∫.n,5,13)
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
#Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/K42/CTT24", pattern="*.csv")
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

# -----------Chuong TrÏnh RiÍng --------------------

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
score[, 7:ncol(score)] <- sapply(score[, 7:ncol(score)], as.character)
score[, 7:ncol(score)] <- sapply(score[, 7:ncol(score)], as.numeric)
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
score[, 7:ncol(score)] <- sapply(score[, 7:ncol(score)], as.character)
score[, 7:ncol(score)] <- sapply(score[, 7:ncol(score)], as.numeric)
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


# Export to stata
full_general_score <- full_general_score[,-c(16,17)] 
# 4115 for CTT and 3910 for CTT24
write_dta(full_general_score, "C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42.dta") 
#write_dta(full_general_score, "C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta") 
# file nay chinh la file factor da luu truoc do cho K42


################ CONFIRMATORY FACTOR ANALYSIS ###################

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K43")

#pattern <- "CTT_((HKD_2018)|(HKC_2017))" # If want to use CTT only, inclding both some major likes: TATM, BV, ...
pattern <- "_((HKD_2018)|(HKC_2017))"     # if want to use CTT, HPR, CLC

Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/K43", pattern=pattern)
data <- lapply(Files, function(x) read.csv(x, stringsAsFactors = FALSE)) # Xem tren data de biet bao nhieu file
#sum(sapply(data,length))

# Create 120 score datasets for each of Majors (all 3 semester) 80 (2 semester)
# or create 60 data set for 2 semester of CTT only
chooselist43 <- paste0(chooselist,".10.1")   # Names of columns need to keep

# Way 1:
# score43 <- data.frame(StudentID=NA)
# for (i in 1:(length(data))){
#   score <- data[[i]][-(1:2),] # Delete 2 rows: 1nd and 2rd rows
#   score <- score[,c(1:2, which(colnames(score) %in% chooselist43))]
#   score43 <- merge(score43, score, by="StudentID", all=TRUE)
# } #4938 students
# Row of dataframe is a dataframe, so unlist to use which()
# which(unlist(a[1,]) %in% chooselist)

# Way 2: chi giu lai cac mon can thiet, be thang dang long, roi cbind()

# Way 3: dung full_joint: 9584 (for 3 majors) (7271 for CTT only)
score43 <- data[[1]][-(1:2),c(1:3, which(colnames(data[[1]]) %in% chooselist43))] # Delete 2 rows: 1nd and 2rd rows
for (i in 2:(length(data))){
  score43 <- full_join(score43, data[[i]][-(1:2),
                      c(1:3,which(colnames(data[[i]]) %in% chooselist43))])
}
# 7271 observations

score43[score43 == ""] <- NA 

# # Way 1:
# score431 <- score43[duplicated(score43$StudentID),] #4800 (3602)
# score432 <- score43[!duplicated(score43$StudentID),] #4784 (3669)
# setdiff(score432$StudentID,score431$StudentID) # this is 20 (82) ID that only appear 1 time:
# 
# length(unique(score43$StudentID)) #4784 (3669)
# length(unique(score431$StudentID)) #4764: 4800 - 4764 = 36 cases duplicated 3 times ( or 20 )
# length(unique(score432$StudentID)) #4784 (3669)
# score431[duplicated(score431$StudentID),1] # 36 cases (or 20)
# unique(score431[duplicated(score431$StudentID),1]) # 8 MSSV of 36 cases (7)
# 
score43 <- unique(score43) #9506 (7218): reduce 9584 - 9506 = 78 observations ( 53 = 7271 - 7218)
score43 <- score43[rowSums(is.na(score43))!=9,] # 9402 (7151) not NA all informations
# # score43_1$na_count <- apply(score43_1, 1, function(x) sum(is.na(x)))
# # score43_1 <- score43_1[score43_1$na_count!=9,]
score431 <- score43[duplicated(score43$StudentID),] #4665 (3516)
score432 <- score43[!duplicated(score43$StudentID),] #4737 (3635)
# length(unique(score431$StudentID)) # check (equal with above result)
# length(unique(score432$StudentID))

# a <- setdiff(score432$StudentID,score431$StudentID) #72 MSSV appear 1 time
# 
# score432 <- score432[score432$StudentID %in% score431$StudentID,] # only keep MSSV which appears in score431
# 
# library(reshape2)

# create a K43 score data
var<-names(score431)[4:12]

test1 <- melt(score431,measure.vars = var,na.rm=TRUE)
test2 <- melt(score432,measure.vars = var,na.rm=TRUE)
DF <- rbind(test1,test2)
score43c <- dcast(DF,StudentID + LastName + FirstName ~ variable, value.var='value')
# Giu 3 bien StudentID, LastName, FirstName de lam ID
# Lay bien "variable" (trong data DF) de lam bien be ra thanh cot
# Gia tri cua cac cot moi thi lay trong bien "value"

score43f <- score43c[complete.cases(score43c),]  #4054 (3274) cases
names(score43f)[4:12] <- lapply(names(score43f)[4:12], function(x) substr(x,1,9))
# score43f[,12] <- as.numeric(score43f[,-c(1:3)])

write_dta(score43f[,-(2:3)], "C:/Users/Vu/Google Drive/Ph.D/LMS/factoranalysis43-4054.dta") 
#write_dta(score43f[,-(2:3)], "C:/Users/Vu/Google Drive/Ph.D/LMS/K43/factoranalysis43-3274.dta") 

#### Infor of K43
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K43")
info<- read.csv("Info/DS_K43_All.csv", stringsAsFactors=FALSE)

info <- info[,c(1,6,7,8,11,13,16,18,24,28,30,34:38,40)]
names(info)[1] <- "StudentID"
score43ff <- merge(score43f,info,by = "StudentID") # all obser of score43f is¥kept, good

write_dta(score43ff[,-(2:3)], "C:/Users/Vu/Google Drive/Ph.D/LMS/K43/factor43-4054.dta") 
#write_dta(score43ff[,-(2:3)], "C:/Users/Vu/Google Drive/Ph.D/LMS/K43/factor43-3274.dta") 

