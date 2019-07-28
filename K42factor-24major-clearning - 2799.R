
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

#------- Above is 3117 students appearing in CTT, but maybe ------
# they also study HPR or CLC. Check and eliminate these students

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K43")
# 6 CLC, 4 HPR for each semester

pattern <- "(CLC|CTR)_((HKD_2018)|(HKC_2017))"
Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/K43", pattern=pattern)
data <- lapply(Files, function(x) read.csv(x, stringsAsFactors = FALSE)) # Xem tren data de biet bao nhieu file
#sum(sapply(data,length))

MSSV <- sapply(data, function(x) (x[-c(1,2),1]))
MSSV1 <- as.data.frame(unlist(MSSV))
names(MSSV1) <- "MSSV"
MSSV2 <- MSSV1 %>% group_by(MSSV) %>% mutate(count = row_number(MSSV))
MSSV3 <- unique(MSSV1) #1125
table(MSSV2$count)

score43ff[score43ff$StudentID %in% MSSV3$MSSV,1] # 5 obs
intersect(MSSV3$MSSV,score43ff$StudentID)
MSSV3[MSSV3$MSSV %in% score43ff$StudentID,1] # 5 obs. Data with 1125 factor levels

score43fff <- score43ff[!score43ff$StudentID %in% MSSV3$MSSV,]
score43fff <- score43fff[grepl("3117",score43fff$StudentID),] #3109
write_dta(score43fff[,-(2:3)], "C:/Users/Vu/Google Drive/Ph.D/LMS/K43/factor43-24major-3109.dta") 


###### Try EFA with score43fff
data <- score43fff
data1 <- sapply(data,as.numeric)
data2 <- data1[,c(4:12)]

# ------- First, we perform a CFA and look at the fit measures and loadings -----

b5.model <- "
MATH =~ ECO501001 + ECO501002 + MAT508001 + ACC507001
VERBAL =~  PML510001 + PML510002 + LAW511001
LANG =~ ENG513001 + ENG513002
"

b5.cfa <- cfa(b5.model, data = data2, estimator = "MLR")
# Marsh et al. (2013) fit measures:
# CFI: .761, TLI: .687, RMSEA: .076
# Our fit measures:
fitmeasures(b5.cfa, c("cfi.robust","tli.robust","rmsea.robust","srmr"))

# ------- let's try ESEM! First, we conduct an exploratory FA on the dataset -------
# Try with K43 data  
parallel <- fa.parallel(data2, fm = 'pa', fa = 'fa')

b5.efa <- fa(data2, nfact = 3, rotate = "geominQ", fm = "ml")
b5.efa <- fa(data2, nfact = 3, rotate = "promax", fm = "pa")
b5.efa

print(b5.efa$loadings,cutoff = 0.1)
fa.diagram(b5.efa)

