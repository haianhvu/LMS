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


setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/")
dc <- read_csv("daicuong2.csv")
names(dc)[2:4] <- c("MaHP","Name","weight")
weight <- dc[!grepl("FRE", dc$MaHP),c(2,4)]
gc <- weight[,1]
chooselist <- gc[[1]]

setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K43/CTT")
#pattern <- "CTT_((HKD_2018)|(HKC_2017))"
pattern <- "_((HKD_2018)|(HKC_2017))"
Files <- list.files(path = "C:/Users/Vu/Google Drive/Ph.D/LMS/K43/CTT", pattern=pattern)
data <- lapply(Files, function(x) read.csv(x, stringsAsFactors = FALSE)) # Xem tren data de biet bao nhieu file
#sum(sapply(data,length))

# Create 72 score datasets for each of Majors (all 3 semester) 48 (2 semester) 
# of CTT only
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
# 6298 observations

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
score43 <- unique(score43) # 6298 - 6237 = 61 duplicated observations
score43 <- score43[rowSums(is.na(score43))!=9,] # 6230 not NA all informations
# # score43_1$na_count <- apply(score43_1, 1, function(x) sum(is.na(x)))
# # score43_1 <- score43_1[score43_1$na_count!=9,]
score431 <- score43[duplicated(score43$StudentID),] # 3067
score432 <- score43[!duplicated(score43$StudentID),] # 3163
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

score43f <- score43c[complete.cases(score43c),]  #3117 cases
names(score43f)[4:12] <- lapply(names(score43f)[4:12], function(x) substr(x,1,9))
# score43f[,12] <- as.numeric(score43f[,-c(1:3)])

# write_dta(score43f[,-(2:3)], "C:/Users/Vu/Google Drive/Ph.D/LMS/factoranalysis43.dta") 
write_dta(score43f[,-(2:3)], "C:/Users/Vu/Google Drive/Ph.D/LMS/K43/k43-24major.dta") 
# 3117 records

#### Infor of K43
setwd("C:/Users/Vu/Google Drive/Ph.D/LMS/K43")
info<- read.csv("Info/DS_K43_All.csv", stringsAsFactors=FALSE)

info <- info[,c(1,6,7,8,11,13,16,18,24,28,30,34:38,40)]
names(info)[1] <- "StudentID"
score43ff <- merge(score43f,info,by = "StudentID") # all obser of score43f is´kept, good

write_dta(score43ff[,-(2:3)], "C:/Users/Vu/Google Drive/Ph.D/LMS/K43/factor43-24major.dta") 


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
