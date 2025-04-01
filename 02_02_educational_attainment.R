# Perline Demange
# Get parental educational attainment 
# Adapted from code by Rosa Cheesman
# Start date: 23/11/2021
# Update new data completed 25-01-02

# Set up 
library(data.table)
library(tidyverse)

# 1. Get educational attainment from register #########
#edu <- fread("//tsd-evs/p805/data/durable/data/registers/original/csv/w19_0634_utd_1970_2023_ut.csv")
edu <- fread("N:/durable/data/registers/SSB/01_data/data_v5.0/csv/EDUCATION_BU_UTD.csv")
head(edu)

#First version looked at highest education reported in 2018. With new data we look at 2023
edu_dat <- edu[, c( "w19_0634_lnr", "BU_2023")]
edu_dat$edu_2023 = as.numeric(str_sub(edu_dat$BU_2023,1,1))
table(edu_dat$edu_2023)

# 1       2       3       4       5       6       7       8       9 
# 27452 2325082  953069 1447894  175874 1324480  561831   59232   18336
sum(is.na(edu_dat$BU_2023))
sum(is.na(edu_dat$edu_2023))
sum(is.na(edu_dat$w19_0634_lnr))

rm(edu)
 
# 2. Remove duplicated individuals #############
summary(duplicated(edu_dat$w19_0634_lnr))
dupli <- edu_dat[duplicated(edu_dat$w19_0634_lnr),]$w19_0634_lnr #0
# IN updated version there is no duplicated individuals
# head(edu_dat[edu_dat$w19_0634_ln %in% dupli,][order(edu_dat$w19_0634_lnr),])
# edu_dat_nodup <- edu_dat[!duplicated(edu_dat$w19_0634_lnr),] 
# head(edu_dat_nodup[edu_dat_nodup$w19_0634_ln %in% dupli,][order(edu_dat_nodup$w19_0634_lnr),])
# # this only removes one the two duplicated rows, randomly 
# #I will remove the individual entirely, as duplicated have different values
# edu_dat_nodup <- edu_dat[!(edu_dat$w19_0634_lnr %in% dupli),]
edu_dat_nodup <- edu_dat

# 3. Remove NAs ######

table(edu_dat_nodup$edu_2023)
# 1       2       3       4       5       6       7       8       9 
# 27452 2325082  953069 1447894  175874 1324480  561831   59232   18336 

# the labels are
# 0 "no edu" 1 "1-7gr" 2 "8-10gr" 3 "some high school" 4 "complete high school" 
# 5 "extra high school" 6 "BA" 7 "MA" 8 "PhD" 9 "unknown" 

sum(is.na(edu_dat_nodup$edu_2023)) #37795
edu_dat_nodup$edu_2023[edu_dat_nodup$edu_2023 == 9] <- NA 
sum(is.na(edu_dat_nodup$edu_2023)) #56131  # 37795+18336 
table(edu_dat_nodup$edu_2023) # no 9 left in the data 

sum(duplicated(edu_dat_nodup$w19_0634_lnr))

# 4. Convert these codes to ISCED 2011 ###########################
# the labels are
# 0 "no edu" 1 "1-7gr" 2 "8-10gr" 3 "some high school" 4 "complete high school" 
# 5 "extra high school" 6 "BA" 7 "MA" 8 "PhD" 9 "unknown" 

# 3-4 are combined together in ISCED  3
# while 5 is unclear if 4 or, here chose 5
x<-edu_dat_nodup$edu_2023
edu_dat_nodup$ISCED11_2023<-ifelse(is.na(x), NA,
                            ifelse(!is.na(x) & x == 0, 0, # combines ppl with edu up to age 2 & up to age 5
                            ifelse(!is.na(x) & x == 1, 1, # primary
                            ifelse(!is.na(x) & x == 2, 2, # secondary lower
                            ifelse(!is.na(x) & x == 4 | x == 3, 3, # secondary upper AND post secondary non tertiary shorter than 2 yrs
                            ifelse(!is.na(x) & x == 5, 5,  # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
                            ifelse(!is.na(x) & x == 6, 6, #undergrad
                            ifelse(!is.na(x) & x == 7, 7, #MSc/MA
                            ifelse(!is.na(x) & x == 8, 8, x))))))))) #PhD
table(edu_dat_nodup$ISCED11_2023)
# 1       2       3       5       6       7       8 
# 27452 2325082 2400963  175874 1324480  561831   59232  


# 5. Convert to US years of schooling equivalent ######
# PRE PRIMARY=1 ::0
# PRIMARY=7::1
# LOWER SECONDARY =10 :2
# UPPER SECONDARY 13 : 3
# POST SECONDARY NON TERITARY 15 : 4/5
# FIRST STAGE TERTIARY NOT LEADING TO ADVANCEED RESEARCH 19: ba: 6
# FIRST OR SECOND STAGE CANNOT DISTINGUISH 20 () ma: 7
# SECOND STAGE TERTIARY LEADING TO ADVANCED RESEARCH QUAL 22: 8
x<-edu_dat_nodup$ISCED11_2023
edu_dat_nodup$EduYears11_2023<-ifelse(is.na(x), NA,
                                      ifelse(!is.na(x) & x == 0, 1, # combines ppl with edu up to age 2 & up to age 5
                                      ifelse(!is.na(x) & x == 1, 7, # primary
                                      ifelse(!is.na(x) & x == 2, 10, # secondary lower
                                      ifelse(!is.na(x) & x == 3, 13,# secondary upper AND post secondary non tertiary shorter than 2 yrs
                                      ifelse(!is.na(x) & x == 5, 15,  # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
                                      ifelse(!is.na(x) & x == 6, 19, #undergrad
                                      ifelse(!is.na(x) & x == 7, 20, #MSc/MA
                                      ifelse(!is.na(x) & x == 8, 22, x))))))))) #PhD
table(edu_dat_nodup$EduYears11_2023)
# 7      10      13      15      19      20      22 
# 27452 2325082 2400963  175874 1324480  561831   59232 
hist(edu_dat_nodup$edu_2023)
hist(edu_dat_nodup$ISCED11_2023)
hist(edu_dat_nodup$EduYears11_2023)

sum(duplicated(edu_dat_nodup$w19_0634_lnr))


save(edu_dat_nodup, file="data/educational_attainement_250102.rda")

