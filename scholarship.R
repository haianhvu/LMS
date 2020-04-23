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
# sum(no_entrance$NÄfm.Tuyá.fn.sinh..TS. < 2016)


# ------ Input the data K42 Diem Thi Dau Vao ------
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

# Merge 2 data sets: diemthi and studentinfo
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
sum(duplicated(finalstudent$MSSV)) #finalstudent is the main data after cleaning: 4982, info + diem thi dau vao

# ------- Merge with data General Full Score 
# Finalstudent2: 4983 students, Full_gneral_score: 4115 students
#-------- Do some Factor analysis ----
# finalstudent <- merge(finalstudent, full_general_score, by = "MSSV" )
# sum(duplicated(finalstudent$MSSV))
# b <- data.frame(names(finalstudent))
# a <- finalstudent[,c(1:8,128:139)] 
# write_dta(a, "factor.dta")


# -------------- Average Grade import -------------------

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")

hk1 <- read.csv("Grade/K42_CTT_HKC_2016.csv", stringsAsFactors=FALSE)
hk2 <- read.csv("Grade/K42_CTT_HKD_2017.csv", stringsAsFactors=FALSE)
hk1 <- hk1[,-1]
hk2 <- hk2[,-1]

# How many type of credits the students take
levels(as.factor(hk1$Sá...tÃ.n.chá..)) ## All of them had 15 (maybe first semester)
levels(as.factor(hk2$Sá...tÃ.n.chá..)) ## All of them had 13. why?

# how many students
length(unique(hk1$MSSV)) ### 3177 vs 3434: 257 duplicated
sum(duplicated(hk1$MSSV))
length(unique(hk2$MSSV)) ### 3126 vs 3385. 259 duplicated
sum(duplicated(hk2$MSSV))

# Drop duplicated data
hk1a <- unique(hk1) ### 3182 students. Still having 5 MSSV duplicated
sum(duplicated(hk1a$MSSV)) #Still having 5 MSSV duplicated
hk1a[duplicated(hk1a$MSSV),][,1]
hk1a <- hk1a[!(duplicated(hk1a$MSSV) | duplicated(hk1a$MSSV, fromLast = TRUE)),]
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

# Delete Students of BI, AV, AE major: 2839 students
removelist <- c('AV', "AE", 'BI')
hk1final <- hk1a[ !grepl(paste(removelist, collapse="|"), hk1a$Lá..p), ]
# This is the "Nhom 4" Scholarship: 7.77 / 79 (65-79: Kha. 80-89: Tot)
# test with hk1:
hk1final1 <- hk1[ !grepl(paste(removelist, collapse="|"), hk1$Lá..p), ]
t <- hk1final1 %>% filter(duplicated(MSSV) | 
                            duplicated(MSSV, fromLast = T)) # 236
t1 <- unique(hk1final1) # 2843
t2 <- t1 %>% filter(duplicated(MSSV) | 
                             duplicated(MSSV, fromLast = T)) 
# 4 obs: coincide with first way because 2843 - 4 = 2839
# conclude: delete all of duplicated data to make sure that everything is normal
# and there is nothing special.

###### Tabula
table(hk1final[hk1final$Ä.iá.fm.TBCHT >= 7.77,]$Xáº.p.loáº.i.rÃ.n.luyá..n)
# to see distribution of "xep loai ren luyen" for students having higher DTBHT of 7.77
table(hk1final[hk1final$Ä.iá.fm.rÃ.n.luyá..n >= 79,]$Xáº.p.loáº.i.há..c.táº.p)
# to see distribution of "xep loai hoc tap" for students having higher DTBHT of 79
# addmargins(a)
# addmargins(b)

test <- hk1final[hk1final$Ä.iá.fm.TBCHT >= 7.77 & hk1final$Ä.iá.fm.rÃ.n.luyá..n >= 79,]
# 110

# Merge Scholarship Receiver data with Academic Achievement data: 
# hk1final: 2839
final_kkht <- merge(hk1final, KKHT42, by = "MSSV", all.x=TRUE)
sum(final_kkht$Ä.iá.fm.TBCHT == final_kkht$dtbhb)
# test for data of 2 datasets coincide in AGP
final_kkht$a <- ifelse(is.na(final_kkht$Stt),-1,final_kkht$Ä.iá.fm.TBCHT == final_kkht$dtbhb)
test <- final_kkht[final_kkht$a==0,] # 3 case has problem, but just typo
# Correct typo by AGP of P.DT&QLSV, i.e. take AGP from column of P.DTQLSV.

# Sort data to check AGP higher than 7.77
final_kkht <- final_kkht[order(final_kkht$Ä.iá.fm.TBCHT, decreasing=TRUE),]
a <- final_kkht[c(1:4,14:16,5,13,7,9,17,18,21:24,26)]
### Move column by names: df[c("g",names(df)[-7])]
### or: df <- subset(df, select=c(g,a:f))

# See that: 31151023710, 31151020956,... is K41 student, no scholarsip although >8.0
# 31161025367,31161026922,31161023496,.... no scholarship
h7.77 <- a[a$Ä.iá.fm.TBCHT >= 7.77,]   # 299 students have higher AGP of 7.77
no_scholarship3 <- h7.77[is.na(h7.77$`%HTHT+KKHT`),] 
# 12 do not receive any scholarship although their AGP is higher than 7.77

# There are 12 students having no scholarship, check them in scholarship HTHT
merge <- merge(no_scholarship3,HTHT42, by = "MSSV")
# only 1 student, but her AGP is quite different while DRL is not. 
# Reason: maybe she retake the test and other also did that
# Note: the student sample at this stage only has normal student, 
# not BI, AV, AE major 

# Eliminate all 12 students out of data, and we will have a good data that 
# ensures all students having AGP higher or equal 7.77 will receive the scholarship.
final_kkht2 <- final_kkht[! final_kkht$MSSV %in% no_scholarship3$MSSV,]
# 2839 - 12 = 2827

# Rearrange columns to be easier to analyze
final_kkht3 <- final_kkht2[c(1:4,14:16,6,7,9,10,17,18,5,13,20:27)]

# Check the Class Code difference
a <- subset(final_kkht3,Lá..p == Lop)
# 4 students have the same class code, all of them are HPR or CLC
# data should just include only Normal Program, why still have HPR or CLC?

# 1. Chuong trình ch???t lu???ng cao (5 chuong trinh): 
# Qu???n tri: ADC; Kinh doanh qu???c t???: IBC; Tài chính_FNC; K??? toán doanh nghi???p_KNC; Ki???m toán_KIC.(thu???ng chuong trình CLC có ký t??? C ??? cu???i mã )

# 2. Chuong trình h???c phí riêng: (Có 04 chuong trình) 
# Kinh t??? nông nghi???p_AG; Kinh t??? chính tr???_KC; Toán tài chính_TF; Th???ng kê kinh doanh_TD;

# 3. Chuong trình h???c phí bình thu???ng: Các chuong trình con lai, 
# con goi la CTT 

# 4. Trong Hoc Phi binh thuong thi: 3 chuong trinh la 100% hoc phi, tuyen sinh rieng
# Ti???ng anh thuong m???i_AV; Kinh t??? h???c ???ng d???ng_AE; H??? th???ng thông tin kinh doanh_BI 
# (Tru 03 chuyên ngành này là nhóm 4)

# Remove student diffrent from Nhom 4:
removelist <- c('ADC', "IBC", "FNC","KNC", "KIC", "AG","KC","TF","TD","AV", "AE", "BI")
test <- final_kkht3[ grepl(paste(removelist, collapse="|"), final_kkht3$Lá..p), ]
# Have to remove 24 students, so only 2803 available
final_kkht4 <- setdiff(final_kkht3, test) # 2803 students
length(final_kkht4[final_kkht4$Ä.iá.fm.TBCHT >= 7.77,][,1])
# 287 students have scholarship of KKHT

# Change comma in "so tien"
final_kkht4$`so tien` <- substr(final_kkht4$`so tien`,1,10)
final_kkht4$`so tien` <- gsub(",", "", final_kkht4$`so tien`, fixed = TRUE)
final_kkht4$`so tien` <- as.numeric(final_kkht4$`so tien`)

#change the name of variable
names(final_kkht4)[c(2:4,8:10,16:19,20:22)] <- c("ho","ten","ngaysinh","tinchi","tbht",
                                     "renluyen","dacap","htht","kkht","tongcong",
                                     "senhan","sotien","chikkht")
# Export to stata
#write_dta(final_kkht4, "finalkkht.dta")


# ----------------- Combine data ---------------
# Merge data: (info + diemthidauvao data) = finalstudent: 4982
# and final_kkht4 data = 2803: only keep NHOM 4 students
# Note:
# info + diemthidauvao = finalstudent: 4982 students
# info + diemthidauvao + diem dai cuong = 4115 students (see in the file final.R)
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")
x <- read.csv("info_and_general_course.csv")

# If use data "info + diemthidauvao = finalstudent: 4982 students" 
# to merge, we will have 2801 (all=T).
# If use data "info + diem dai cuong = 4115 students" 
# to merge, we will have 2721 (all=T). Here we use both 4982 student data and 4115 data
# 21/4/2020: remember 2721 only keeps students having all diem dai cuong.
# x is data of 4115 student. Kiem tra thi thay cac sinh vien khac biet giua
# 2721 va 2801 là do cac sinh vien co diem TB rat thatp, duong nhu
# la khong thi du cac mon Dai Cuong.

final_full_kkht1 <- merge(final_kkht4, finalstudent, by = "MSSV", all.x=TRUE)
final_full_kkht2 <- merge(final_kkht4, finalstudent, by = "MSSV")  # 2801
dif <- setdiff(final_full_kkht1,final_full_kkht2)
# Still having 2 students who do not exist in info and diemthi.
# Reason: they are DH41. So final_full_kkht2 is correct

final_full_kkht3 <- merge(final_kkht4, x, by = "MSSV")  # 2721
a <- setdiff(final_full_kkht2$MSSV,final_full_kkht3$MSSV) # MSSV khac biet
# kiem tra cac sinh vien nay
b <- final_kkht %>% filter(final_kkht$MSSV %in% a) #wrong
b <- final_kkht %>% filter(grepl(paste(a,collapse = "|"),MSSV))
b <- final_kkht[final_kkht$MSSV %in% a,]

# Note: DH41 means they are K41. MSSV is 3115 is K41 intake, 
# i.e. enrolled in K41 but did not study.
# They study with K42 students, so 3115 still have DH42

# Check name of student coincidence
final <- final_full_kkht2
final <- final_full_kkht3
final$hocbong <- ifelse(is.na(final$sotien),0,1)
sum(final$hocbong) # 287 KKHT scholarships: correct
final <- final[order(final$tbht, decreasing=TRUE),]

# check diemthi of 2801 (2721) students of final data, are they A1 exam group?
test1 <- merge(diemthi, final, by = "MSSV", all.y=TRUE)
test2 <- merge(diemthi, final, by = "MSSV")
test <- setdiff(test1,test2)
# Have difference: 3 students from K42 intake, 4 from K40, 11 from K41

# No, they are A00, A01, D01
table(test2$Khoi.x)

# final_full_kkht2 = info + diemthidauvao + GPA : 4813 students
#write_dta(final_full_kkht2, "final_full_kkht.dta")


# -------- Eliminate all of student receive HTHT ----------
htht_id <- HTHT42$MSSV # Get MSSV of HTHT: 162 students
# some receive 100% of 2 types of HB, some receive 50% of each HB
# so need to keep them, not eliminate
twosch <- intersect(HTHT42$MSSV,KKHT$MSSV)# get 18  MSSV receiving 2 scholarsips
only_htht42 <- HTHT42[! HTHT42$MSSV %in% twosch,] # 162 -18 = 144 observations
intersect(a$MSSV,KKHT$MSSV) # Check again, it is ok
# Delete observations receiving HTHT only
final <- final[! final$MSSV %in% only_htht42$MSSV,] 
# Final number is 2674 while 2801 - 144 = 2657. 
# Reason: some students among 144 students are eliminated early, e.g. HPR, CLC
# If merge with full_general_score as very above command, 2594 students.
# 2674 - 2594 = 80: may be this is a CTR or CLC ?

# if use 2721 data, final number is 2594.


#-------------- Some Analysis -------------
table(final$dacap) # How many students have received other scholarship: 0
table(final$Khoi) # A00: 1691, A01: 685, D01: 280. Maybe other reserach?

table(final$Khoi,final$hocbong) # Scholarship among KhoiThi.
hist(final$tbht)
hist(final[final$Khoi == "A00",]$tbht, breaks=20)
hist(final[final$Khoi == "A01",]$tbht, breaks = 20, freq = FALSE)
hist(final[final$Khoi == "D01",]$tbht, breaks = 20)

curve(dnorm(x, mean=mean(final[final$Khoi == "D01",]$tbht), sd=sd(final[final$Khoi == "D01",]$tbht)), add=TRUE, col="darkblue", lwd=2)

# Test mean of AGP different btw KhoiThi
sum(is.na(final$tbht)) # every students have tbht
sum(is.na(final[final$Khoi == "A00",]$tbht)) # 10, not everyone has KhoiThi
sum(is.na(final[final$Khoi == "A01",]$tbht)) # 10
sum(is.na(final[final$Khoi == "D01",]$tbht)) # 10, So only 18 students do not have KhoiThi

sum(is.na(final[final$Khoi == "A00",]$DM1)) # 16
#a <- final[final$Khoi == "A00" & is.na(final$DM1),]  # 7 studetns
#b <- final[is.na(final$Khoi),]                       # 18 students
sum(is.na(final[final$Khoi == "A01",]$DM1)) # 10
sum(is.na(final[final$Khoi == "D01",]$DM1)) # 10


# Final step to create final data

a <- setdiff(final$ngaysinh,final$NgaySinh)
b <- setdiff(final$ngaysinh,final$`Ngay sinh`)
levels(as.factor(final$Nganh))
names(final)[64] <- "doanvien"

delete <- c(5:7,11,12,14,30:32,34,39:41,43:45,47,49:50,52,59:62,65,69,74,76:81,87:93,95,98,99:107,114,116:143,148,149,162,163)
final <- final[, - delete]

names(final)[c(7:8,25:27,30,31,34,35,37,38,46,47,49,50)] <- c("renluyen1","renluyen2","mon1","mon2","mon3",
                             "noisinh", "gioitinh", "tongiao","thuongtru",
                             "tinh", "maQH","namtuyensinh","soBD","namtotnghiep",
                             "ngaynhaphoc")
names(final)[c(51:53,54,58:60,62,64,79)] <- c("MaLoaiSV","LoaiSV","matinhtrang","FatherOccupation",
                                              "TenKhoa", "MaNganh","NganhHoc","ChuongTrinh",
                                              "MaLopDC","nomajor")

# Merge with HK2 dataset
hk2a <- hk2a[,c(1,6,7,9)]
names(hk2a)[2:4] <- c("tinchi2","tbhthk2","renluyenhk2")
final_1<- merge(final,hk2a, by = "MSSV")
final_2 <- merge(final,hk2a, by = "MSSV", all.x=TRUE) # 42 students do not study HK2
setdiff <- setdiff(final_2,final_1) #order matters: bigger is first,

# Change some variables: gender (nu la 1, nam la 0), relegion, area, .....
final <- final_1

colnames(final)[(names(final) == "Phai")] <- "gender" #some missing values, so use var gioitinh
table(final$gioitinh) #Nu la 1, nam la 0
# final$gender <- as.factor(final$gioitinh)  
# table(final$gender)
final$gender <- ifelse(final$gioitinh == "Nam",0,1) # Nam = 0, Nu = 1

levels(as.factor(final$ReligionID))
levels(as.factor(final$tongiao))
table(final$ReligionID)
str(final$ReligionID)   # here, var Religion is still character, not factor
final$ReligionID <- as.factor(final$ReligionID) # convert to factor
levels(final$ReligionID) <- c("0","0",levels(final$ReligionID)[3:10]) # Combine factors

table(final$EthnicID)
final$EthnicID <- as.factor(final$EthnicID)
levels(final$EthnicID) <- c("KINH","KINH",levels(final$EthnicID)[3:19])

levels(as.factor(final$ProvinceID))
levels(as.factor(final$PriorityID))   # nhom uu tien 1 cong 2 diem, nhom uu tien 2 cong 1 diem
levels(as.factor(final$PriorityName)) # 3 la khong uu tien, 1 la uu tien 1 
levels(as.factor(final$AreaID)) # cong diem khu vuc, KV1: 1.5, KV2NT: 1, KV2: 0.5 


# Export to stata
write_dta(final, "final.dta")
# bao gom sinh vien k nhan hoc bong, sinh vien nhan KKHT only, 
# va sinh vien nhan ca KKHT va HTHT, loai bo sinh vien nhan HTHT.
# sinh vien o day la sinh vien CTT


###### merge in stata, find some MSSV do not exist in finalfull. Why?
merge <- read.csv("C:/Users/Vu/Google Drive/Ph.D/LMS/merge_is_1.csv")

a <- setdiff(merge$ï..MSSV,diemthi$MSSV)
# only 1 MSSV of merge data does not exist in diemthi, so diemthi is fine

a <- setdiff(merge$ï..MSSV,finalstudent$MSSV)
# no problem




