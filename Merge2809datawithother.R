library("readr")
library("dplyr")
library("foreign")
library("haven")

rm(list = ls())
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS")

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

# --------------- Merge 2 data sets: diemthi and studentinfo ------------
fullstudent <- merge(diemthi, studentinfo, by = "MSSV", all=TRUE)

# Diemthi 4943 while fullstudent 4983, why having more 40 cases?
# It means 40 MSSV existing in Student Info but in DiemThi
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
# this is the final data
sum(duplicated(fullstudent2$MSSV))
sum(duplicated(finalstudent$MSSV))

# So the final data "finalstudent" has 4982 observations 
test <- finalstudent[is.na(finalstudent$STT),] # 40 cases
# 40 cases from K40 and K41 and K42 do not have Diem Thi Dau Vao.
# K40 and K41 Diem Thi are stored in data of K40, K41 while this is the
# data of K42. So why dont some K42 students have Diem Thi?

# However, it is more general when we keep them in the data. We can eliminate
# these observations later. Final data is finalstudent


# -----------------  import Average Grade of CTT K42   -------------------
# Note that: maybe CTT here means both CTT and special CTT

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/Grade")

Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/Grade", pattern="*.csv")
test <- lapply(Files, read.csv) # Xem tren test de biet bao nhieu file

# Creat 6 score datasets for 2 semester of 3 programs: CTT, HPR, CLC
name <- c()
for (i in 1:length(test)) {
  name[i] <- strsplit(Files[i],'\\.')[[1]][1]
  assign(name[i], data.frame(test[[i]][,-1]))
}

# only 3434 and 3385 MSSV of CTT have GPA for HK1 and HK2 respectively

# How many type of credits the students take
for (i in name) {
  x <- get(i)
  print(levels(as.factor(x$Sá...tÃ.n.chá..)))
}

for (i in 1:length(test)) {
  test[[i]][,names(test[[i]])=='Sá...tÃ.n.chá..'] %>% 
    as.factor() %>% levels %>% print
}

## All of CTR, CLC, CTT had 15 for maybe first semester
## CLC has 16 for second semester, CTR and CTT has 13 for second semester

# Merge to create HK1 and HK2
a <- ls(pattern = "_HKC_") %>% lapply(get) #HKC 2016
HK1 <- do.call(rbind,a) #4915

a <- ls(pattern = "_HKD_") %>% lapply(get) #HKD 2017
HK2 <- do.call(rbind,a) #4849

# ?regrex
# subset(a, grepl("blue", a$x))
# grep(x=df$TextGridLabel, pattern="^.*0.+$")
# removelist <- c("ABC","AVD")
#hk1final <- hk1a[!grepl(paste(removelist, collapse="|"), hk1a$Lá..p), ]

# howmany students are duplicated?
HK1a <- unique(HK1) ### 4915 - 4562 = 353 duplicated
sum(duplicated(HK1)) ### 353
HK2a <- unique(HK2) ### 4849 - 4510 = 339 duplicated
sum(duplicated(HK2)) ### 339

a <- unique(HK1a$MSSV) ### 4562 - 4555 = 7 duplicated
sum(duplicated(HK1a$MSSV)) ### 7
a <- unique(HK2a$MSSV) ### 4510 - 4487 = 23 duplicated
sum(duplicated(HK2a$MSSV)) ### 23

# Duplicated observation
test1 <- HK1a[duplicated(HK1a$MSSV) | duplicated(HK1a$MSSV, fromLast = TRUE),]
# 7 duplicated * 2 = 14
test2 <- HK2a[duplicated(HK2a$MSSV) | duplicated(HK2a$MSSV, fromLast = TRUE),]
# 23 duplicated but 38 observation, not 46 
##--> some observations repeated 3 times

for (i in c("HK1a", "HK2a")) {
  assign(paste(i,"b",sep=""), get(i))
  assign(paste("test",i, sep=""), 
         get(i)[duplicated(get(i)$MSSV) | duplicated(get(i)$MSSV, fromLast = TRUE),])
} 

# Compare MSSV appearing in 2 duplicated set
setdiff(unique(test1$MSSV),unique(test2$MSSV)) # 0 MSSV
setdiff(unique(test2$MSSV),unique(test1$MSSV)) # 8 MSSV
# all of duplicated of HK1 is duplicated in HK2, but 8 MSSV duplicated
# in HK2 do not exist in HK1. Maybe they registered to study 2 majors lately

# Create variable controlling duplicated data
HK1a$dup2 <- ifelse(grepl(paste((test1$MSSV),collapse = "|"),HK1a$MSSV),1,0)
# Collapse means the way we combine all of final results after pasting 
# sep means the way combine elements of x, and elements of y
# paste(c(1,2),c("a","b"),sep="and")
# paste(test1$MSSV,test1$MSSV,sep="and",col="example")
sum(HK1a$dup2) # check, 14 is correct

HK1a$dup1 <- ifelse(HK1a$MSSV %in% test1$MSSV,1,0)
sum(HK1a$dup1 - HK1a$dup2) # check: 0 menas variable dup1 = varialbe dup2

HK2a$dup3 <- ifelse(HK2a$MSSV %in% test2$MSSV,1,0)
HK2a$dup4 <- ifelse(grepl(paste((test2$MSSV),collapse = "|"),HK2a$MSSV),1,0)

HK1a$term <- 1
HK2a$term <- 1
# list <- list(HK1a, HK2a)
# for (i in 1:2) {
#   list[[i]]$term <- 1
# }

setdiff(HK1a$MSSV,HK2a$MSSV) # 68 MSSV existing in HK1a, not in HK2a.
# this number different from (4562 - 4510) because repeated observation
setdiff(HK2a$MSSV,HK1a$MSSV) # 0: no new students

# create HK data with wide format
HKwide1 <- merge(HK1a, HK2a, by = "MSSV", all = TRUE) # 4594
HKwide2 <- merge(HK1a, HK2a, by = "MSSV") # 4526
# note: HK2a 4510, HK1a 4562.

setdiff(HK1a$MSSV, HKwide1$MSSV) 
# check and confirm: all MSSV of HKwide1 is HK1a.
# 4594 differ 4562 because duplicated observations


# Append HK1 and HK2 to create panel data (long)
a <- HK1a
b <- HK2a
a[setdiff(names(b), names(a))] <- NA
b[setdiff(names(a), names(b))] <- NA
HK <- rbind(a,b)  # 4562 + 4510 = 9072 # this is main data of HK (long)

a <- HK[which(HK$dup1==1 & HK$dup3==1),] # 0 observations
a <- subset(HK, dup3==1 & is.na(dup1)) # 38 observations = 38 duplication above
a <- subset(HK, dup1==1 & is.na(dup3)) # 14 observations = 14 duplication above

# ----------- Combine data of HK (AGP of HK1 and HK2) 
# and data of finalstudent (info and Entrance test) ----------------





# Delete Students of BI, AV, AE major: 2839 students
# removelist <- c('AV', "AE", 'BI')
# hk1final <- hk1a[ !grepl(paste(removelist, collapse="|"), hk1a$Lá..p), ]
# This is the "Nhom 4" Scholarship: 7.77 / 79
# hk1final <- hk1a

###### Tabula
table(hk1final[hk1final$Ä.iá.fm.TBCHT >= 7.77,]$Ä.iá.fm.rÃ.n.luyá..n)
table(hk1final[hk1final$Ä.iá.fm.rÃ.n.luyá..n >= 79,]$Xáº.p.loáº.i.há..c.táº.p)
# addmargins(table(hk1final[hk1final$Ä.iá.fm.TBCHT >= 7.77,]$Xáº.p.loáº.i.há..c.táº.p))
# addmargins(table(hk1final[hk1final$Ä.iá.fm.rÃ.n.luyá..n >= 79,]$Xáº.p.loáº.i.há..c.táº.p))

test <- hk1final[hk1final$Ä.iá.fm.TBCHT >= 7.77 & hk1final$Xáº.p.loáº.i.rÃ.n.luyá..n >= 79,]


# Merge Scholarship Receiver data with Academic Achievement data: 2839
final_kkht <- merge(hk1final, KKHT42, by = "MSSV", all.x=TRUE)
sum(final_kkht$Ä.iá.fm.TBCHT == final_kkht$dtbhb)
# test for data of 2 datasets coincide in AGP
final_kkht$a <- ifelse(is.na(final_kkht$Stt),-1,final_kkht$Ä.iá.fm.TBCHT == final_kkht$dtbhb)
test <- final_kkht[final_kkht$a==0,] #only one case has problem, but just typo
# Correct typo by AGP of P.DT&QLSV, i.e. take AGP from column of P.DTQLSV.

# Sort data to check AGP higher than 7.77
final_kkht <- final_kkht[order(final_kkht$Ä.iá.fm.TBCHT, decreasing=TRUE),]
a <- final_kkht[c(1:4,14:16,5,13,7,9,17,18,21:24,26)]
### Move column by names: df[c("g",names(df)[-7])]
### or: df <- subset(df, select=c(g,a:f))

# See that: 31151023710, 31151020956,... is K41 student, no scholarsip although >8.0
# 31161025367,31161026922,31161023496,.... no scholarship
h7.77 <- a[a$Ä.iá.fm.TBCHT >= 7.77,]
no_scholarship3 <- h7.77[is.na(h7.77$`%HTHT+KKHT`),]
no_scholarship1 <- a[a$Ä.iá.fm.TBCHT >= 7.77,][is.na(a$`%HTHT+KKHT`),] # 12 students have no scholarship
no_scholarship2 <- no_scholarship1[!is.na(no_scholarship1$MSSV),] # 12 students
setdiff(no_scholarship2,no_scholarship3) # Check lai xem 2 cach tren co khac nhau khong.

# There are 12 students having no scholarship, check them in scholarship HTHT
merge <- merge(no_scholarship2,HTHT42, by = "MSSV")
# only 1 student, but her AGP is quite different while DRL is not. 
# Reason: maybe she retake the test.

# Eliminate all 12 students out of data, and we will have a good data that 
# ensures all students having AGP higher or equal 7.77 will receive the scholarship.
final_kkht2 <- final_kkht[! final_kkht$MSSV %in% no_scholarship2$MSSV,]
# 2839 - 12 = 2827

# Rearrange columns to be easier to analyze
final_kkht3 <- final_kkht2[c(1:4,14:16,6,7,9,10,17,18,5,13,20:27)]

# Check the Class Code difference
a <- subset(final_kkht3,Lá..p == Lop)
# 4 students have the same class code, all of them are HPR or CLC
# data should just include only Normal Program, why still have HPR or CLC?

# 1. Chuong trình ch???t lu???ng cao: Qu???n tri: ADC; Kinh doanh qu???c t???: IBC; Tài chính_FNC; K??? toán doanh nghi???p_KNC; Ki???m toán_KIC.(thu???ng chuong trình CLC có ký t??? C ??? cu???i mã )
# 2. Chuong trình h???c phí riêng: (Có 04 chuong trình) Kinh t??? nông nghi???p_AG; Kinh t??? chính tr???_KC; Toán tài chính_TF; Th???ng kê kinh doanh_TD;
# 3. Chuong trình h???c phí bình thu???ng: Các chuong trình còn l???i. Trong dó: Ti???ng anh thuong m???i_AV; Kinh t??? h???c ???ng d???ng_AE; H??? th???ng thông tin kinh doanh_BI (Tr??? 03 chuyên ngành này ra s??? là nhóm 4)

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