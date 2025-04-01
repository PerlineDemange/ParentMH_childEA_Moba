# Perline Demange 
# Get MoBA mental health data 
# Start date: 21/10/2021
# Update new data 03-01-25

# Set up 
library(tidyverse)
library(haven)
shrd_dir="N:/durable/shared/"
work_dir="N:/durable/projects/Perline/2021_Children_siblings_MH/Parental psychopathology and school performance/"
#data_dir="N:/durable/data/moba/Original files/Delivery 05 2023/PDB2601_20230513/" #this folder does not have the phenotools names. 
data_dir="N:/durable/data/moba/Original files/sav_0523/" #data managers do not know who created this folder.....

#install.packages(paste0(shrd_dir,"phenotools_0.2.9.zip"), repos=NULL,  type = "binary")
library(phenotools)

# 1. In mothers ##############
## 1.1. Checking items/scales ############
# Documentation about Moba questionnaires is available their website. 
# Quick sumup of which items are used by phenotools, and check if this is what we expect 
# ASRS (ADHD)
# GG503 to GG508, coded 0 to 4 
# full scale is 0 to 24 
#
# Audit problematic use 
# full scale is 8 to 24
# NN352,353,354,355,356,357, has 1 to 3 (while scale in theory goes to 5)
# 358,359 have only NA (have you injured someone and did someone suggest you cut drinking, is No coded Na? )
# This is incorrect #Lawrence is going to correct next version of phenotools
#
# SCL anx 
# full scale is 0 to 12
# NN325,326,331,332, coded 0 to 3
# feeling fearful, nervousness or shakiness inside, feeling tense or keyed up, suddenly scared fro no reason
#
# SCL dep 
# full scale is 0 to 12
# NN327,328,329,330, coded 0 to 3
# feeling hopeless about future, feeling blue, worrying too much about things, feeling everything is an effort 
#
# Anxiety/depression pregnancy 15w 
# Items are AA1548 feeling fearful, AA1549 nervousness or shakiness inside, 
# AA1550 feeling hopeless about the future, AA1551 feeling blue, AA1552 worrying too much about things
# To match what is done at age 8 anxiety should be AA1548 and AA1549 and
# depression should be AA1550-1552
# two items is not enough so I will only look at full scale (and eventually depression)


## 1.2 Get all items for all scales (except eating disorder) ######################

items_m_SCL_Q1_full <- c("AA1548", "AA1549", "AA1550", "AA1551", "AA1552")
items_m_SCL_Q1_dep <- c( "AA1550", "AA1551", "AA1552")
items_m_SCL_Q8_full <- c("NN325","NN326","NN331","NN332", "NN327",
                         "NN328","NN329","NN330")
items_m_SCL_Q8_SCL5 <- c("NN325","NN326", "NN327","NN328","NN329")
items_m_SCL_Q8_anx <- c("NN325","NN326","NN331","NN332")
items_m_SCL_Q8_dep <- c("NN327","NN328","NN329","NN330")
items_m_ADHD_Q3 <- c("GG503", "GG504", "GG505", "GG506", "GG507", "GG508")
items_m_AUDIT_Q8 <- c("NN353","NN354","NN355","NN356","NN357","NN358","NN359")

items_mother <- c(items_m_ADHD_Q3, items_m_SCL_Q1_full,
                  items_m_SCL_Q8_full, items_m_AUDIT_Q8)

data_mother <- curate_dataset(variables_required= items_mother,
                                   moba_data_root_dir=paste0(data_dir),
                                   PDB="2601",
                                   moba_data_version=12,
                                   completion_threshold=0.5,
                                   return_items=T, # I request all items to check which items are used 
                                   consistent_items=F,
                                   out_format="merged_df")


## 1.3 Recode all data  ################
# (factors in the data from curate_dataset is not the correct way of coding the scales)

# remove dbl
data <- data_mother %>% 
  mutate_if(is.labelled, as_factor)

colnames(data)<-gsub("_raw","",colnames(data)) #remove suffixe
head(data)

# Recode all SCL items (anxiety and depression)
summary(data[,items_m_SCL_Q8_full])
data <- mutate_at(data, 
                  items_m_SCL_Q8_full, ~as.numeric(recode(.,
                                                          "Not bothered"=0, 
                                                          "A little bothered"=1, 
                                                          "Quite bothered"=2, 
                                                          "Very bothered"=3)))


summary(data[,items_m_SCL_Q1_full])
data <- mutate_at(data, 
                  items_m_SCL_Q1_full, ~as.numeric(recode(.,
                                                          "Not bothered"=0, 
                                                          "A little bothered"=1, 
                                                          "Quite bothered"=2, 
                                                          "Very bothered"=3)))


# Recode ASRS (ADHD)
summary(data[,items_m_ADHD_Q3])
data <- mutate_at(data, 
                  items_m_ADHD_Q3, ~as.numeric(recode(.,
                                                          "Never"=0, 
                                                          "Seldom"=1, 
                                                          "Sometimes"=2, 
                                                          "Often"=3, 
                                                          "Very often"=4)))

# Recode Audit-p (alcohol)
summary(data[,items_m_AUDIT_Q8])
data <- mutate_at(data, 
                  c("NN353", "NN354", "NN355",
                    "NN356", "NN357"), ~as.numeric(recode(.,
                                                      "Never"=0, 
                                                      "Less than monthly"=1, 
                                                      "Monthly"=2, 
                                                      "Weekly"=3, 
                                                      "Daily/almost daily"=4)))

# For NN358 and NN359, the questionnaire differentiates 
# before the last year or in the last year. 
# Looking up the scale documentation told us not in last year is 2 and last year is 4
data <- mutate_at(data, 
                  c("NN358", "NN359"), ~as.numeric(recode(.,
                                                       "No"=0, 
                                                       "Yes, but not in the last year"=2, 
                                                       "Yes, during the last year"=4)))



## 1.4 Create the sum scores ####################

scales <- list(items_m_SCL_Q1_full,items_m_SCL_Q1_dep ,
               items_m_SCL_Q8_full, items_m_SCL_Q8_SCL5,
               items_m_SCL_Q8_anx, items_m_SCL_Q8_dep,
               items_m_ADHD_Q3, items_m_AUDIT_Q8)
scalenames <- c("items_m_SCL_Q1_full","items_m_SCL_Q1_dep" ,
                "items_m_SCL_Q8_full", "items_m_SCL_Q8_SCL5",
                "items_m_SCL_Q8_anx", "items_m_SCL_Q8_dep", 
                "items_m_ADHD_Q3", "items_m_AUDIT_Q8")
completion_threshold <- 0.75

for (scale in 1:length(scales)) {
  data <- data %>% mutate(nonmissing = rowSums(!is.na(data[scales[[scale]]])))
  data <- data  %>% mutate(scale_score = 
                             ifelse(nonmissing >=
                                      completion_threshold*length(scales[[scale]]),
                                    rowSums(data[scales[[scale]]],
                                            na.rm = T),
                                    NA))
  names(data)[names(data) == "scale_score"] <- paste0("scale_score_",
                                                      scalenames[scale])
}

relevant_columns <- paste0("scale_score_", scalenames)
short_data <- data[, c("preg_id", "m_id",  "f_id", 
                       "BARN_NR", "birth_yr", relevant_columns)]
summary(short_data)

# Double check, Compare with complete scales from the phenotools 
phenotools_scl <- curate_dataset(variables_required=c("scl_full_m_8yr",
                                                      "scl_anx_m_8yr",
                                                      "scl_dep_m_8yr") ,
                                   moba_data_root_dir=paste0(data_dir),
                                   PDB="2601",
                                   moba_data_version=12,
                                   completion_threshold=0.5,
                                   return_items=F, 
                                   consistent_items=F,
                                   out_format="merged_df")

# > summary(short_data$scale_score_items_m_SCL_Q8_full)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    1.00    2.33    3.00   24.00   71755 
#   > summary(phenotools_scl$scl_full_m_8yr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    1.00    2.34    3.00   24.00   71712 

# 
# > summary(phenotools_scl$scl_anx_m_8yr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.0     0.0     0.0     0.9     1.0    12.0   71712 
#   > summary(short_data$scale_score_items_m_SCL_Q8_anx)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.0     0.0     0.0     0.9     1.0    12.0   71764 
#   > summary(phenotools_scl$scl_dep_m_8yr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    1.00    1.44    2.00   12.00   71718 
#   > summary(short_data$scale_score_items_m_SCL_Q8_dep)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    1.00    1.43    2.00   12.00   71762 

# More NAs with home made scales compared to phenotools, Mean is sometimes off by 0.01


## 1.5 Save data ##################

moba_data_mother <- short_data
save(moba_data_mother, file = "data/data_moba_mother_SCL_audit_adhd_250103.rda")

load("data/data_moba_mother_SCL_audit_adhd_250103.rda")

## 1.6 Eating disorder score ##################
### 1.6.1 Part 1: get eating disorders items and code them ---------
# same code as in get_ed_data.R

ED1_notshared <- c("AA1475", "AA1476", "AA1477")
ED1_shared_beforepregnancy <- c("AA1478", "AA1480", "AA1482",
                                "AA1484", "AA1486")
ED1_shared_now <- c("AA1479", "AA1481", "AA1483", "AA1485",
                    "AA1487", "AA1488")
ED8_notshared_Q42 <- c("NN285", "NN286", "NN287", "NN288",
                       "NN289", "NN290", "NN291", "NN292")
ED8_shared <- c("NN293", "NN367", "NN294", "NN295", "NN296",
                "NN297", "NN298", "NN299","NN300", "NN301")

ED_items <- c(ED1_notshared, ED1_shared_beforepregnancy,
              ED1_shared_now, ED8_notshared_Q42, ED8_shared)

ED_data <- curate_dataset(variables_required= ED_items,
                        moba_data_root_dir=paste0(data_dir),
                        PDB="2601",
                        moba_data_version=12,
                        completion_threshold=0.5,#single item so completion threshold doesnt matter 
                        return_items=T, # I request all items to check which items are used 
                        consistent_items=F,
                        out_format="merged_df")

head(ED_data)

# remove dbl
data <- ED_data %>% 
  mutate_if(is.labelled, as_factor)

colnames(data)<-gsub("_raw","",colnames(data)) #remove suffixe

# Code items 
summary(data[,ED1_notshared])

data <- mutate_at(data, 
                  "AA1475", ~as.numeric(recode(.,
                                                          "No"=0, 
                                                          "Yes, a little"=1, 
                                                          "Yes, a lot"=2)))
data <- mutate_at(data, 
                  "AA1476", ~as.numeric(recode(.,
                                               "No, not especially worried"=0, 
                                               "Somewhat worried"=1, 
                                               "Yes, very worried"=2)))
data <- mutate_at(data, 
                  "AA1477", ~as.numeric(recode(.,
                                               "No"=0, 
                                               "Yes, occasionally"=1, 
                                               "Yes, often"=2)))

summary(data[,ED1_shared_beforepregnancy])
summary(data[,ED1_shared_now])


data <- mutate_at(data, 
                  c("AA1478", "AA1479"), ~as.numeric(recode(.,
                                               "No"=0, 
                                               "Seldom"=1, 
                                               "Yes, at least once a week"=2)))

data <- mutate_at(data, 
                  c("AA1480", "AA1481", "AA1482", "AA1483",
                    "AA1484","AA1485", "AA1486", "AA1487"),
                  ~as.numeric(recode(.,
                                   "Seldom/never"=1, 
                                   "At least once a week"=2)))

data <- mutate_at(data, 
                  "AA1488", ~as.numeric(recode(.,
                                               "No, not especially important"=0, 
                                               "Yes, quite important"=1, 
                                               "Yes, very important"=2)))
             


summary(data[,ED8_notshared_Q42])

data <- mutate_at(data, 
                  "NN285", ~as.numeric(recode(.,
                                               "No"=0, 
                                               "Yes"=1)))

data <- mutate_at(data, 
                  c("NN289", "NN290"), ~as.numeric(recode(.,
                                              "Not at all"=0, 
                                              "A little"=1, 
                                              "Very much"=2)))
data <- mutate_at(data, 
                  "NN292", ~as.numeric(recode(.,
                                              "No"=0, 
                                              "Yes"=1)))

summary(data[,ED8_shared])
data <- mutate_at(data, 
                  "NN293", ~as.numeric(recode(.,
                                              "No"=0, 
                                              "Yes"=1)))
data <- mutate_at(data, 
                  "NN367", ~as.numeric(recode(.,
                                              "No"=0, 
                                              "Yes, but less often"=1, 
                                              "Yes, at least once a week"=2)))
data <- mutate_at(data, 
                  "NN294", ~as.numeric(recode(.,
                                              "No"=0, 
                                              "Yes, somewhat out of control"=1, 
                                              "Yes, absolutely out of control"=2)))
data <- mutate_at(data, 
                  "NN295", ~as.numeric(recode(.,
                                              "Not at all"=0, 
                                              "Some"=1, 
                                              "Very much"=2)))

data <- mutate_at(data, 
                  c("NN296", "NN297", "NN298", "NN299", "NN300"), ~as.numeric(recode(.,
                                                          "Never"=0, 
                                                          "A few times"=1, 
                                                          "Weekly"=2, 
                                                          "Several times a week"=3)))
data <- mutate_at(data, 
                  "NN301", ~as.numeric(recode(.,
                                              "Not important at all (1)"=0, 
                                              "(2)"=1, 
                                              "3"=2,
                                              "(4)"=3,
                                              "The most important (5)"=4)))

head(data)
summary(data)

save(data, file = "data/data_moba_mother_eatingdisorders_250103.rda")
write.csv2(data, "data/data_moba_mother_eatingdisorders_250103.csv")
load("data/data_moba_mother_eatingdisorders_250103.rda")


### 1.6.2. Part 2: further selection of items and IRT -----------
# This was done by Eivind Ystrom, code is IRT.do 

### 1.6.3. Part 3: Get IRT results with other parental traits results. -----------
ED <- as.data.frame(haven::read_dta("data/data_moba_mother_eatingdisorders_scores_250103.dta"))
head(ED)
head(ED[,50:56])
summary(ED$ED1_NRM_NR) # NR is all the sample
summary(ED$ED1_NRM) # this is the sample filtered on SE, SE=1 (which actually means the data was missing) are excluded 
summary(ED$ED8_NRM)
summary(ED$ED1_NRM_se)
hist(ED$ED1_NRM)
hist(ED$ED8_NRM)
hist(ED$ED1_NRM_se)

# get empirical reliability (Du toit 2003) 
var(ED$ED1_NRM, na.rm=T)/(var(ED$ED1_NRM, na.rm=T)+ mean((ED$ED1_NRM_se^2), na.rm=T))
var(ED$ED8_NRM, na.rm=T)/(var(ED$ED8_NRM, na.rm=T)+ mean((ED$ED8_NRM_se^2), na.rm=T))

ED_short <- ED[c("preg_id", "m_id", "f_id", "barn_nr", "ED1_NRM", "ED8_NRM")]
ED_short <- rename(ED_short, BARN_NR=barn_nr)

# Merge with moba_data_mother
head(ED_short) #114012
head(moba_data_mother[,1:5])
#There is a weird thing happening with ED -> the birth_yr has the zero removed, it is 29 instead of 2009
#I guess it might be a format conversion issue 
#Update: this error was not present in the update



ED_short$preg_id <- as.character(ED_short$preg_id)

moba_data_mother_all <- merge(moba_data_mother, ED_short,
                              by=c("preg_id", "m_id", "f_id",  "BARN_NR"))

moba_data_mother[!(moba_data_mother$preg_id %in% moba_data_mother_all$preg_id),][,1:6]
ED[!(ED$preg_id %in% moba_data_mother_all$preg_id),][,1:6]
# When merging I lose about 3 rows
# I can find two rows missing from the same pregnancy and it is a row with fully missing data 
# I am still missing why one additional row is missing 

#rename the ED columns to match name style of other traits 
moba_data_mother_all <- rename(moba_data_mother_all,
                               scale_score_items_m_ED_NRM_Q1 = ED1_NRM)
moba_data_mother_all <- rename(moba_data_mother_all,
                               scale_score_items_m_ED_NRM_Q8 = ED8_NRM)

save(moba_data_mother_all, file = "data/data_moba_mother_SCL_audit_adhd_ed_250103.rda")
load("data/data_moba_mother_SCL_audit_adhd_ed_250103.rda")

# 2. Fathers ##################
## 2.1 Describe mental health measures ####
# SCL-8 
# Anxiety 
# feeling fearful, nervousness or shakiness inside, feeling tense or keyed up, suddenly scared for no reason
# Depression 
# feeling hopeless about future, feeling blue, worrying too much about things, feeling everything is an effort 
#SCl12 is available in 2015 but we use scl8

# SCL-5
# Anxiety: feeling fearful,  nervousness or shakiness inside, 
# Depression: feeling hopeless about the future, feeling blue, worrying too much about things
# I get SCL-5 to compare with mothers 

# ADHD - ASRS scale
# FF535-FF540, 0-4

#Audit problematic use is the seven last items of the audit, 
# https://www.tandfonline.com/doi/full/10.1080/10826080601025532

## 2.2  Get items for the scale ###########
items_f_SCL_Q1_full <- c("FF251", "FF252", "FF253", 
                         "FF254", "FF255", "FF256", "FF257", "FF258")
items_f_SCL_Q1_dep <- c("FF253", "FF254","FF255", "FF256")
items_f_SCL_Q1_anx<- c("FF251", "FF252", "FF257", "FF258")
items_f_SCL_Q1_SCL5 <- c("FF251", "FF252", "FF253", "FF254", "FF255")
items_f_SCL_Q2015_full <- c("G_52_1", "G_52_2", "G_52_3", "G_52_4",
                            "G_52_5", "G_52_6", "G_52_7", "G_52_8")
items_f_SCL_Q2015_SCL5 <- c("G_52_1", "G_52_2", "G_52_3", "G_52_4", "G_52_5")
items_f_SCL_Q2015_anx <- c("G_52_1", "G_52_2","G_52_7", "G_52_8")
items_f_SCL_Q2015_dep <- c("G_52_3", "G_52_4", "G_52_5", "G_52_6")
items_f_ADHD_Q1 <- c("FF535", "FF536", "FF537", "FF538", "FF539", "FF540")
items_f_AUDIT_Q2015 <- c("G_32", "G_33", "G_34", "G_35", "G_36", "G_37", "G_38")  


items_father <- c(items_f_SCL_Q1_full, items_f_SCL_Q2015_full, 
                  items_f_ADHD_Q1, items_f_AUDIT_Q2015)

data_father <- curate_dataset(variables_required= items_father,
                              moba_data_root_dir=paste0(data_dir),
                              PDB="2601",
                              moba_data_version=12,
                              completion_threshold=0.5, #single items so doesnt matter 
                              return_items=T, # single items 
                              consistent_items=F,
                              out_format="merged_df")


## 2.3 Recode all data  ################
# (factors in the data from curate_dataset is not the correct way of coding the scales)

# remove dbl
data <- data_father %>% 
  mutate_if(is.labelled, as_factor)

colnames(data)<-gsub("_raw","",colnames(data)) #remove suffixe


# Recode all SCL items (anxiety and depression)
summary(data[,items_f_SCL_Q1_full])
data <- mutate_at(data, 
                  items_f_SCL_Q1_full, ~as.numeric(recode(.,
                                                          "Not bothered"=0, 
                                                          "A little bothered"=1, 
                                                          "Quite bothered"=2, 
                                                          "Very bothered"=3)))


summary(data[,items_f_SCL_Q2015_full])
data <- mutate_at(data, 
                  items_f_SCL_Q2015_full, ~as.numeric(recode(.,
                                                          "Ikke plaget"=0, 
                                                          "Litt plaget"=1, 
                                                          "Ganske mye plaget"=2, 
                                                          "Veldig mye plaget"=3)))



# Recode ASRS (ADHD)
summary(data[,items_f_ADHD_Q1])
data <- mutate_at(data, 
                  items_f_ADHD_Q1, ~as.numeric(recode(.,
                                                      "Never"=0, 
                                                      "Seldom"=1, 
                                                      "Sometimes"=2, 
                                                      "Often"=3, 
                                                      "Very often"=4)))

# Recode Audit-p (alcohol)
summary(data[,items_f_AUDIT_Q2015])
data <- mutate_at(data, 
                  c("G_32", "G_33", "G_34", "G_35", "G_36"),
                  ~as.numeric(recode(.,
                         "Aldri"=0, 
                         "Sjeldnere enn månedlig"=1, 
                         "Månedlig"=2, 
                         "Ukentlig"=3, 
                         "Dagli/nesten daglig"=4)))

# For NN358 and NN359, the questionnaire differentiates 
# before the last year or in the last year. 
# Looking up the scale told us not in last year is 2 and last year is 4

data <- mutate_at(data, 
                  c("G_37", "G_38"),
                  ~as.numeric(recode(.,
                              "Nei"=0, 
                              "Ja, men ikke i løpet av det siste året"=2, 
                              "Ja, i løpet av det siste året"=4)))



## 2.4 Create the sum scores ####################

scales <- list(items_f_SCL_Q1_full, 
               items_f_SCL_Q1_dep,
               items_f_SCL_Q1_anx, 
               items_f_SCL_Q1_SCL5, 
               items_f_SCL_Q2015_full, 
               items_f_SCL_Q2015_SCL5,
               items_f_SCL_Q2015_anx, 
               items_f_SCL_Q2015_dep, 
               items_f_ADHD_Q1,
               items_f_AUDIT_Q2015)
scalenames <- c("items_f_SCL_Q1_full", 
               "items_f_SCL_Q1_dep",
               "items_f_SCL_Q1_anx", 
               "items_f_SCL_Q1_SCL5", 
               "items_f_SCL_Q2015_full", 
               "items_f_SCL_Q2015_SCL5",
               "items_f_SCL_Q2015_anx", 
               "items_f_SCL_Q2015_dep", 
               "items_f_ADHD_Q1",
               "items_f_AUDIT_Q2015")
completion_threshold <- 0.75

for (scale in 1:length(scales)) {
  data <- data %>% 
    mutate(nonmissing = rowSums(!is.na(data[scales[[scale]]])))
  data <- data  %>% 
    mutate(scale_score = ifelse(nonmissing >= 
                                  completion_threshold*length(scales[[scale]]),
                                rowSums(data[scales[[scale]]], na.rm = T),
                                NA))
  names(data)[names(data) == "scale_score"] <- paste0("scale_score_",
                                                      scalenames[scale])
}

relevant_columns <- paste0("scale_score_", scalenames)
short_data <- data[, c("preg_id", "m_id",  "f_id", 
                       "BARN_NR", "birth_yr", relevant_columns)]
summary(short_data)


# Double check, Compare with complete scales from the phenotools 
# I get some issues with phenotools during the update, so I do not compare but only check the summary fo our data looks ok

# phenotools_scl <- curate_dataset(variables_required=c("scl_anx_f_far",
#                      "scl_dep_f_far",
#                      "scl_full_f_far2",
#                      "scl_anx_f_far2",
#                      "scl_dep_f_far2") ,
# moba_data_root_dir=paste0(data_dir),
# PDB="2601",
# moba_data_version=12,
# completion_threshold=0.75,
# return_items=F,
# consistent_items=F,
# out_format="merged_df")

# summary(short_data$scale_score_items_f_SCL_Q1_full)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    0.00    1.17    2.00   24.00   36731 
# 
# > summary(short_data$scale_score_items_f_SCL_Q2015_full)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    1.00    1.87    3.00   24.00   79055 
# > summary(short_data$scale_score_items_f_ADHD_Q1)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    6.00    8.00    8.22   10.00   24.00   79488 



## 2.5 Save data ##################
moba_data_father <- short_data
save(moba_data_father, file = "data/data_moba_father_SCL_audit_adhd_250103.rda")

load("data/data_moba_father_SCL_audit_adhd.rda")
