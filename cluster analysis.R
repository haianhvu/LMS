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

######## Use file "factorK42-24major.dta" in the Script factoranalysis.R
######## because this is the main analysing file of the study.

write_dta(full_general_score, "C:/Users/Vu/Google Drive/Ph.D/LMS/factorK42-24major.dta") 
