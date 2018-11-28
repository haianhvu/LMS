install.packages("readr")
install.packages("dplyr")

library("readr")
library("dplyr")

rm(list = ls())
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")

# Import data into R
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")
KKHT <- read.csv(file = "Ket-qua-HB-KKHT-UEH-HKD-2017.csv", stringsAsFactors=FALSE)
HTHT <- read.csv(file = "Ket-qua-HB-HTHT-UEH-HKD-2017.csv", stringsAsFactors=FALSE)

# Change the format
names(HTHT) <- c("Stt","Khoa","MSSV","Lop","Ho","Ten","Ngay sinh","dtbhb","ren luyen","cmnd","%da cap","%HTHT","%KKHT","%HTHT+KKHT","Se nhan","so tien","chi nhan HTHT")
names(KKHT) <- c("Stt","Khoa","MSSV","Lop","Ho","Ten","Ngay sinh","dtbhb","ren luyen","cmnd","%da cap","%HTHT","%KKHT","%HTHT+KKHT","Se nhan","so tien","chi nhan KKHT")

KKHT$`ren luyen` <- as.numeric(sub(",", ".", KKHT$`ren luyen`, fixed = TRUE))
KKHT$dtbhb <- as.numeric(sub(",", ".", KKHT$dtbhb, fixed = TRUE))
HTHT$dtbhb <- as.numeric(sub(",", ".", HTHT$dtbhb, fixed = TRUE))

# KKHT$dtbhb <- ifelse(KKHT$dtbhb > 100, KKHT$dtbhb/100, KKHT$dtbhb/10)
# HTHT$dtbhb <- ifelse(HTHT$dtbhb > 100, HTHT$dtbhb/100, HTHT$dtbhb/10)

# Keep the K42 student only, 516 and 162
KKHT42 <- KKHT[KKHT$Khoa==42,]
length(KKHT42$MSSV)
HTHT42 <- HTHT[HTHT$Khoa==42,]
length(HTHT42$MSSV)

# Test how many k42 students receive 2 types of scholarship: 26 abd 18
length(which(KKHT42$`chi nhan KKHT`==0))
sum(HTHT42$`chi nhan HTHT`==0)      

# Retrieve k42 students having 2 scholarships
kk_sch <- KKHT42[KKHT42$`chi nhan KKHT`==0,][,c(3,11:14)] # 26 students
ht_sch <- HTHT42[HTHT42$`chi nhan HTHT`==0,][,c(3,11:14)] # 18 students

# Why 26 and 18? They should be equal. Answer: 100% KKHT and 100% HTHT
sch2 <- rbind(kk_sch,ht_sch)
#sch2 <- sch2[order(sch2$MSSV),]
#test <- sch2[!(duplicated(sch2$MSSV) | duplicated(sch2$MSSV, fromLast = TRUE)), ]
indDuplicatedVec <- duplicated(sch2$MSSV) | duplicated(sch2$MSSV, fromLast = TRUE)
xuathien2hb <- sch2[indDuplicatedVec,]
xuathien1hb <- sch2[!indDuplicatedVec,]


# ------------------- Diem thi dau vao -------------------

# # Input the data
# setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")
# diemthi <- read_delim(file = "K42-Diem.csv", delim=',')
# 
# # Input data student info
# studentinfo <- read.csv("Student-K42-Info.csv", stringsAsFactors=FALSE)
# names(studentinfo)[1:2] <- c("MSSV")
# names(studentinfo)[c(5,34:36)] <- c("NgaySinh","DM1","DM2","DM3")
# 
# # Merge 2 data sets
# fullstudent <- merge(diemthi, studentinfo, by = intersect(names(diemthi), names(studentinfo)), all=TRUE)
# 
# # Checked if students have the same Birthdate, DM1, DM2, DM3
# sum(duplicated(fullstudent[,1:5])) # 14 cases
# same <- fullstudent[(duplicated(fullstudent[,1:5]) | duplicated(fullstudent[,1:5], fromLast = TRUE)), ]
# # ==> some students applied to 2 Majors, i.e. their names appears twice in the list of successful applications 
# #and some students have the same Birthdate and DM1,DM2,DM3
# 
# # Checked if students in the list DiemThi appear in the student info data
# 
# # or vice versus
# no_entrance <- fullstudent[is.na(fullstudent$HoLot),c(1:4,11,14,43,52:54)] # 41 students
# sum(no_entrance$Nƒfm.Tuy·.fn.sinh..TS. < 2016)


# ------ Input the data K42 Diem Thi Dau Vao ------
# Test how many type of entrance exams: 
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")
diemthi <- read_delim(file = "K42-Diem2.csv", delim=',')
levels(as.factor(diemthi$`MÙn 1`)) ## Only Math
levels(as.factor(diemthi$`MÙn 2`)) ## Physics and Liter
levels(as.factor(diemthi$`MÙn 3`)) ## Chemistry and English
levels(as.factor(diemthi$Khoi))    ## 3 types
names(diemthi)[19] <- "MSSV"
names(diemthi)[16] <- "MaHS"
# Fix the problem
diemthi[diemthi$SoBaoDanh == "DCN006136", ][7] <- "16/01/1998" 
diemthi[diemthi$MSSV == "31161027023", ][7] <- "09/06/1997"
diemthi[diemthi$MSSV == "31161027008", ][7] <- "04/03/1996"

# Input data student info
studentinfo <- read.csv("Student-K42-Info.csv", stringsAsFactors=FALSE)
names(studentinfo)[1] <- c("MSSV")
names(studentinfo)[c(5,34:36)] <- c("NgaySinh","DM1","DM2","DM3")

# Merge 2 data sets
fullstudent <- merge(diemthi, studentinfo, by = "MSSV", all=TRUE)

# Diemthi 4943 while fullstudent 4983, why having more 40 cases?
sum(is.na(fullstudent[,2])) #  40 cases: 36 cases from K41, K40, but 4 cases from K42.
no_diemthi <- fullstudent[is.na(fullstudent[,2]), ]

# Check again using another merge
fullstudent2 <- merge(diemthi, studentinfo, by = intersect(names(diemthi), names(studentinfo)), all=TRUE)
sum(is.na(fullstudent2$STT)) # 43 cases do not appear in diemthi?
diff <- setdiff(fullstudent2[,c(5:8,13:15)],fullstudent[,c(1:4,10,12,14)])
length(diff$MSSV) # Diff is 0: no difference

# Test the reason again
test <- fullstudent2[is.na(fullstudent2$STT),] # 40 cases
sum(test[50] < 2016) #  36 cases from K41, 40
no_diemthi42 <- test[test[50] == 2016, ] # 4 cases from K40
# 7 cases with 2 of them is Du Bi, some of them appers 2 times in merged data
# e.g 31161027023 in fullstudent2 have problem in NgaySinh
sum(duplicated(fullstudent2$MSSV)) # 1 cases
a <- fullstudent2[duplicated(fullstudent2$MSSV) | duplicated(fullstudent2$MSSV, fromLast = TRUE),]
# only one cases who applied to two major

# Delete the DC major of this student
fullstudent2 <- fullstudent2[!(fullstudent2[5]=="31161022173" & fullstudent2[8]=="PT"), ]
sum(duplicated(fullstudent2$MSSV))


#-------------- Grade import -------------------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")

hk1 <- read.csv("Grade/K42_CTT_HKC_2016.csv", stringsAsFactors=FALSE)
hk2 <- read.csv("Grade/K42_CTT_HKD_2017.csv", stringsAsFactors=FALSE)
hk1 <- hk1[,-1]
hk2 <- hk2[,-1]

# How many type of credits the students take
levels(as.factor(hk1$S·...t√.n.ch·..)) ## All of them had 15 (maybe first semester)
levels(as.factor(hk2$S·...t√.n.ch·..)) ## All of them had 13. why?

# howmany students
length(unique(hk1$MSSV)) ### 3177 / 3434. 257 duplicated
sum(duplicated(hk1$MSSV))
length(unique(hk2$MSSV)) ### 3126 / 3385. 259 duplicated
sum(duplicated(hk2$MSSV))

# Drop duplicated data
hk1a <- unique(hk1) ### 3182 students. Still having 5 MSSV duplicated
sum(duplicated(hk1a$MSSV))
hk1a[duplicated(hk1a$MSSV),][,1]

hk2a <- unique(hk2) ### 3134. Still having 8 MSSV duplicated
sum(duplicated(hk2a$MSSV))
hk2a[duplicated(hk2a$MSSV),][,1]

# Delete Students of BI, AV, AE. Final having 2843
removelist <- c('AV', "AE", 'BI')
hk1final <- hk1a[ !grepl(paste(removelist, collapse="|"), hk1a$L·..p), ]
# This is the "Nhom 4" Scholarship: 7.77 / 79

# Delete student duplicated because of having 2 differents GPA
hk1final[duplicated(hk1final$MSSV),][,1] ## 2 students
hk1final <- hk1final[!duplicated(hk1final$MSSV)==1,]

###### Tabula
a <- table(hk1final[hk1final$ƒ.i·.fm.TBCHT >= 7.77,]$X·∫.p.lo·∫.i.r√.n.luy·..n)
b <- table(hk1final[hk1final$ƒ.i·.fm.r√.n.luy·..n >= 79,]$X·∫.p.lo·∫.i.h·..c.t·∫.p)
addmargins(a)
addmargins(b)

test <- hk1final[hk1final$ƒ.i·.fm.TBCHT >= 7.77 & hk1final$X·∫.p.lo·∫.i.r√.n.luy·..n >= 79,]




