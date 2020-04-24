library("readr")
library("dplyr")
library("foreign")
library("haven")

rm(list = ls())
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")

###### Import HB data into R #######
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

# Test how many k42 students receive 2 types of scholarship: 26 and 18
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
xuathien2hb <- sch2[indDuplicatedVec,]  #36
xuathien1hb <- sch2[!indDuplicatedVec,] #8

##### Input the data K42 Diem Thi Dau Vao #####
# Test how many type of entrance exams: 
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")
diemthi <- read_delim(file = "K42-Diem2.csv", delim=',')
levels(as.factor(diemthi$`Môn 1`)) ## Only Math
levels(as.factor(diemthi$`Môn 2`)) ## Physics and Liter
levels(as.factor(diemthi$`Môn 3`)) ## Chemistry and English
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

###### Merge 2 data sets: diemthi dau vao and studentinfo ######
fullstudent <- merge(diemthi, studentinfo, by = "MSSV", all=TRUE) #4983

# Diemthi 4943 while fullstudent 4983, why having more 40 cases?
sum(is.na(fullstudent[,2])) #  40 cases: 36 cases from K41, K40, but 4 cases from K42.
no_diemthi <- fullstudent[is.na(fullstudent[,2]), ]

# Check again using another merge
fullstudent2 <- merge(diemthi, studentinfo, by = intersect(names(diemthi), names(studentinfo)), all=TRUE)
sum(is.na(fullstudent2$STT)) # 40 cases do not appear in diemthi?
diff <- setdiff(fullstudent2[,c(5:8,13:15)],fullstudent[,c(1:4,10,12,14)])
length(diff$MSSV) # Diff is 0: no difference
setdiff(fullstudent2$MSSV,fullstudent$MSSV)
sum(duplicated(fullstudent$MSSV))
sum(duplicated(fullstudent2$MSSV))

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
finalstudent <- fullstudent2[!(fullstudent2[5]=="31161022173" & fullstudent2[8]=="PT"), ]
sum(duplicated(fullstudent2$MSSV))
sum(duplicated(finalstudent$MSSV))
#finalstudent is the main data after cleaning: 4982, info + diem thi dau vao
#write_csv(finalstudent, "info_and_entrance.csv")

##### Average Grade import - Import HK1 and HK2 average data ####

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")

# These two files only include AGP of CTT students for HK1 and HK2. Phong QLDTSV use CTT for both normal majors and 3 HPR majors that have 100% tuition fee.
hk1 <- read.csv("Grade/K42_CTT_HKC_2016.csv", stringsAsFactors=FALSE)
hk2 <- read.csv("Grade/K42_CTT_HKD_2017.csv", stringsAsFactors=FALSE)
hk1 <- hk1[,-1]
hk2 <- hk2[,-1]

names(hk1)[c(2:10)] <- c("ho","ten","ngaysinh","lop","sotinchi",
                         "tbht","xeploaihoctap","renluyen",
                         "xeploairenluyen")

names(hk2)[c(2:10)] <- paste0(c("ho","ten","ngaysinh","lop","sotinchi",
                         "tbht","xeploaihoctap","renluyen",
                         "xeploairenluyen"),"hk2")

# How many type of credits the students take
levels(as.factor(hk1$sotinchi)) ## All of them had 15 (maybe first semester)
levels(as.factor(hk2$sotinchi)) ## All of them had 13. why?

# how many students
length(unique(hk1$MSSV)) ### 3177 vs 3434: 257 duplicated
sum(duplicated(hk1$MSSV))
length(unique(hk2$MSSV)) ### 3126 vs 3385. 259 duplicated
sum(duplicated(hk2$MSSV))

# Drop duplicated data
hk1a <- unique(hk1) ### 3182 students. Still having 5 MSSV duplicated
sum(duplicated(hk1a$MSSV)) #Still having 5 MSSV duplicated
hk1a[duplicated(hk1a$MSSV),][,1]
hk1a <- hk1a[!(duplicated(hk1a$MSSV) | duplicated(hk1a$MSSV, fromLast = TRUE)),] #3434
# delete both duplicated and original record of duplicated (because do 
# not know which one should be kept)

hk2a <- unique(hk2) ### 3134. Only keep non-dupliate obser of HK2
sum(duplicated(hk2a$MSSV)) # but Still having 8 MSSV duplicated
hk2a[duplicated(hk2a$MSSV),][,1]
t <- hk2a %>% filter(duplicated(hk2a$MSSV) | duplicated(hk2a$MSSV, fromLast = T))
# form hk2a, 16 because 8 x 2, i.e. after unique of HK2, still have 8 MSSV duplicated
t1 <- hk2 %>% filter(duplicated(hk2$MSSV) | duplicated(hk2$MSSV, fromLast = T))
# from hk2, 318 duplicates
t2 <- t1 %>% count(MSSV) # 59 students have duplicates
t2 %>% count(n)   # some of them have 2 or 4 even 8 duplicates

# Delete Students of BI, AV, AE major: 2839 students.
# Delete them because in the folder CTT, we have 27 majors, but
# these 3 majors are still considered as HPR because students 
# choose them before enrolling to the University.
# If we choose folder CTT24, we may not need to delete.
# Note: because we are analyzing scholarship, we only keep Nhom 4 # to able to use threshold 7.77 and make sure they have equal 
# motivations
removelist <- c('AV', "AE", 'BI')
hk1final <- hk1a[ !grepl(paste(removelist, collapse="|"), hk1a$lop), ] #2839
# This is the "Nhom 4" Scholarship: 7.77 / 79 (65-79: Kha. 80-89: Tot)
# another way to get 2839 data:
hk1final1 <- hk1[ !grepl(paste(removelist, collapse="|"), hk1$lop), ]
t <- hk1final1 %>% filter(duplicated(MSSV) | 
                            duplicated(MSSV, fromLast = T)) # 236
t1 <- unique(hk1final1) # 2843
t2 <- t1 %>% filter(duplicated(MSSV) | 
                      duplicated(MSSV, fromLast = T)) 
# 4 obs: coincide with first way because 2843 - 4 = 2839
# conclude: delete all of duplicated data to make sure that everything is normal
# and there is nothing special. Therefore, use hk1final data
# hk1final data: AGP of HK1 and HK2 of 24 major students of CTT 

###### Tabula
table(hk1final[hk1final$tbht >= 7.77,]$xeploairenluyen)
# to see distribution of "xep loai ren luyen" for students having higher DTBHT of 7.77
table(hk1final[hk1final$renluyen >= 79,]$xeploaihoctap)
# to see distribution of "xep loai hoc tap" for students having higher DTBHT of 79
# addmargins(a)
# addmargins(b)

test <- hk1final[hk1final$tbht >= 7.77 & hk1final$renluyen >= 79,]   # 110

##### Merge Scholarship Receiver data with HK1 data #####
#Note: hk1final: 2839
final_kkht <- merge(hk1final, KKHT42, by = "MSSV", all.x=TRUE)
sum(final_kkht$Ä.iá.fm.TBCHT == final_kkht$dtbhb)
# test for data of 2 datasets coincide in AGP
final_kkht$a <- ifelse(is.na(final_kkht$Stt),-1,final_kkht$tbht == final_kkht$dtbhb)
test <- final_kkht[final_kkht$a==0,] 
# 1 cases where dtbhb is different from AGP of QLDTSV, but just typo
# Correct typo by AGP of P.DT&QLSV, i.e. take AGP from column of P.DTQLSV.
# Note: dtbhb is a score in list of scholarship, and 
# tbht is AGP of QLDTSV, dtbhb is AGP of scholarship list

# Sort data to check AGP higher than 7.77
final_kkht <- final_kkht[order(final_kkht$tbht, decreasing=TRUE),]
a <- final_kkht[c(1:4,14:16,5,13,7,9,17,18,21:24,26)]
h7.77 <- a[a$tbht >= 7.77,]   # 299 students have higher AGP of 7.77
no_scholarship3 <- h7.77[is.na(h7.77$`%HTHT+KKHT`),] 
# 12 do not receive any scholarship although their AGP is higher than 7.77

# There are 12 students having no scholarship, check them in scholarship HTHT
merge <- merge(no_scholarship3,HTHT42, by = "MSSV")
# only 1 student, but her AGP is quite different while DRL is not. 
# Reason: maybe she retake the test and other also did that
# Note: the student sample at this stage only has normal student, # not BI, AV, AE major 

# Eliminate all 12 students out of data, and we will have a good data that 
# ensures all students having AGP higher or equal 7.77 will receive the scholarship.
final_kkht2 <- final_kkht[! final_kkht$MSSV %in% no_scholarship3$MSSV,] # 2839 - 12 = 2827
final_kkht3 <- final_kkht2[c(1:4,14:16,6,7,9,10,17,18,5,13,20:27)]

# Remove student diffrent from Nhom 4:
# do it to make sure only having Nhom 4 (maybe we do not eliminate them on list of scholarship)
removelist <- c('ADC', "IBC", "FNC","KNC", "KIC", "AG","KC","TF","TD","AV", "AE", "BI")
test <- final_kkht3[ grepl(paste(removelist, collapse="|"), final_kkht3$lop), ]
# Have to remove 24 students, so only 2803 available
final_kkht4 <- setdiff(final_kkht3, test) # 2803 students
length(final_kkht4[final_kkht4$tbht >= 7.77,][,1])
# 287 students have scholarship of KKHT

final_kkht4$sotien <- substr(final_kkht4$`so tien`,1,10)
final_kkht4$sotien <- gsub(",", "", final_kkht4$sotien, fixed = TRUE)
final_kkht4$sotien <- as.numeric(final_kkht4$sotien)


# ----------------- Combine data ---------------
# Merge data: (info + diemthidauvao data) = finalstudent: 4982
# and final_kkht4 data = 2803: only keep NHOM 4 students
# Note:
# info + diemthidauvao = finalstudent: 4982 students
# info + diem dai cuong = 4115 students (see in the file final.R)
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")
x <- read.csv("info_and_general_course.csv")

# If use data "info + diemthidauvao = finalstudent: 4982 students" 
# to merge, we will have 2801 (all=T).
# If use data "info + diem dai cuong = 4115 students" 
# to merge, we will have 2721 (all=T). Here we use both 4982 student data and 4115 data
# 21/4/2020: remember 2721 only keeps students having all diem dai cuong.
# x is data of 4115 student. Kiem tra thi thay cac sinh vien khac biet giua
# 2721 va 2801 là do cac sinh vien co diem TB rat thatp, duong nhu la khong thi du cac mon Dai Cuong. Chon 2721

final_full_kkht2 <- merge(final_kkht4, finalstudent, by = "MSSV")  # 2801

final_full_kkht3 <- merge(final_kkht4, x, by = "MSSV")  # 2721
a <- setdiff(final_full_kkht2$MSSV,final_full_kkht3$MSSV) # MSSV khac biet
# kiem tra cac sinh vien nay. Cac sinh vien nay khong xuat hien trong data x. Co le ho k du het cac mon dai cuong.
b <- final_kkht %>% filter(MSSV %in% a) #wrong
b <- final_kkht %>% filter(grepl(paste(a,collapse = "|"),MSSV))
b <- final_kkht[final_kkht$MSSV %in% a,]

# Combine final_full_kkht3 with diemthi data beacuse x data only has info and diem dai cuong.
kkht_data <- merge(final_full_kkht3, diemthi, by = "MSSV") # 2711
setdiff(final_full_kkht3$MSSV,diemthi$MSSV) # 2721 - 2711
# 10 MSSV, 3 of 3114, 4 of 3115 and 3 of 3116 do not exist in diemthi

##### Eliminate all of student receive HTHT to create data for scholarship KKHT only #####
htht_id <- HTHT42$MSSV # Get MSSV of HTHT: 162 students
# some receive 100% of 2 types of HB, some receive 50% of each HB
# so need to keep them, not eliminate
twosch <- intersect(HTHT42$MSSV,KKHT$MSSV)# get 18  MSSV receiving 2 scholarsips
only_htht42 <- HTHT42[! HTHT42$MSSV %in% twosch,] # 162 -18 = 144 observations
# Delete observations receiving HTHT only
kkht_final_data <- kkht_data %>% filter(!MSSV %in% only_htht42$MSSV)
# 2584.

final <- kkht_final_data %>% select(1:4,8:20,22:24,27:38,46,
                                    50,52,55,63,65,67,69,
                                    151,159,161,163,164)

final_a <- merge(final,hk2a, by="MSSV")

final_b <- final_a %>% select(1:45,50,51,53)
final_c <- final_b %>% mutate(hocbong=ifelse(is.na(final_b$`%KKHT`),0,1))
# write.dta(final_c,"final_kkht_2584.dta") #2584


##### Eliminate all of student receiving KKHT to create data for scholarship HTHT only #####
kkht_id <- KKHT42$MSSV # Get MSSV of HTHT: 516 students
# some receive 100% of 2 types of HB, some receive 50% of each HB
# so need to keep them, not eliminate
# get 18  MSSV receiving 2 scholarsips
twosch <- intersect(HTHT42$MSSV,KKHT$MSSV) 
only_kkht42 <- KKHT42[! KKHT42$MSSV %in% twosch,] # 516 -18 = 498 observations
# Delete observations receiving HTHT only
htht_final_data <- kkht_data %>% filter(!MSSV %in% only_kkht42$MSSV)
# 2440

final <- htht_final_data %>% select(1:4,8:20,22:24,27:38,46,
                                    50,52,55,63,65,67,69,
                                    151,159,161,163,164)

final_a <- merge(final,hk2a, by="MSSV")

final_b <- final_a %>% select(1:45,50,51,53)
final_c <- final_b %>% mutate(hocbong=ifelse(is.na(final_b$`%HTHT`),0,1))

##### only keep SV lower than 6.0 and only SV receiving HTHT
a <- final_c %>% filter(tbht <= 6.0 )


write.dta(final_c,"final_htht_2440.dta") #2584
