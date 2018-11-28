install.packages("readr")
install.packages("dplyr")

library("readr")
library("dplyr")

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/")

# Load data into R
giangvien1 <- read.csv("16.05-giangvienDC.csv", stringsAsFactors=FALSE)
names(giangvien1$ï..MaHP) <- c("MaHP") # Not work
names(giangvien1[,1]) <- "MaHP" # Not work
names(giangvien1)[1]<-"MaHP"
#sapply(giangvien1,as.character)
#as.numeric(giangvien1$NamSinh)

# We should drop the variable MaHP in giangvien1 becaus
# this is not necessary for merging. Only MaCBGD is enough
#giangvien1 <- giangvien1[,-1]
giangvien1$MaHP<-NULL

# Merging Lecturers' information with hk data, 1243 observations
# It should be equal to the no. of obersvation of general classes in hk data
# because giangvien1 is the data of general classes
# full_hk_test <- full_join(giangvien1,hk, by=c("Khoa","BoMon","MaHP","MaCBGD"))
# Merging above is wrong
full_hk <- merge(hk, giangvien1, by.x = c("Khoa","BoMon","MaCBGD"), by.y = c("Khoa","BoMon","MaCBGD") )

# Keeping only lecturers of general classes, 1243.
# Infact, the no. of observation of this dataset should equal the full_hk dataset 
full_general_hk <- full_hk[rowSums(is.na(full_hk[, c("TrinhDo","ChucDanh","LanhDao","NamSinh","GioiTinh","X")]))!=6,]

# Count no. of general classes using LMS
full_general_hk$lms_hk <- ifelse(is.na(full_general_hk$LMS),0,1)
aggregate(lms_hk ~ Khoa,data=full_general_hk, FUN=sum) -> lmsclass_khoa



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
