# Perline Demange 
# Merge siblings info with educational score and parents phenotypes in Moba 
# Start date: 03/11/2021

# Set up #######
library(tidyverse)
library(data.table)
set.seed(42)
rm(list=ls())


# 1. Load all relevant data ################

## 1.1 School performance ----------------
# This the school performance of everyone possible in Norway
# with the individual ID w19_0634_lnr
load("data/school_performance_211103.rda")
head(final_data_educ) # 700 247

## 1.2 Linkage equivalence between registry and Moba  --------
mobadir = "N:\\data\\durable\\data\\moba"
linkage <- fread(paste0(mobadir, "/linkage/PDB2601_kobling_SSB_v12.csv"))
head(linkage) # 306 489
# Contains PREG_ID_2601, barn_nr, rolle (can be child, mother or father), and w19_0634_lnr
linkage_wide <- spread(linkage,rolle,w19_0634_lnr)
linkage_child <- drop_na(linkage_wide, SU2PT_CHILD)
linkage_child$SU2PT_FATHER <- NULL
linkage_child$SU2PT_MOTHER <- NULL
linkage_mother <- drop_na(linkage_wide, SU2PT_MOTHER)
linkage_father<- drop_na(linkage_wide, SU2PT_FATHER)
linkage_mother$SU2PT_CHILD <- NULL
linkage_mother$barn_nr <- NULL
linkage_mother$SU2PT_FATHER <- NULL
linkage_father$SU2PT_CHILD <- NULL
linkage_father$barn_nr <- NULL
linkage_father$SU2PT_MOTHER <- NULL
linkage_parents <- full_join(linkage_mother, linkage_father)

#need to be careful with memory, remove unused dataframes regularly 
rm(linkage)
rm(linkage_mother)
rm(linkage_father)
rm(linkage_wide)

## 1.3 Pedigree (get parental ID) ----------
#each row contains individual w19_0634_lnr and the mother and parents IDs lnr 
# Only load it when needed, to be more careful with memory
load("data/pedigree_211020.rda")
head(kinship)
#parentsID <- kinship[, 1:3]

## 1.4 educational attainment for parents ----------------------------------------
load("data/educational_attainement.rda")
head(edu_dat_nodup)
sum(duplicated(edu_dat_nodup$w19_0634_lnr))

## 1.4 Mother's Moba data ------------------------------------------------------
#each row is a pregnancy ID and the mother data 
load("data/data_moba_mother_SCL_audit_adhd_ed.rda")
head(moba_data_mother_all) #114 625
moba_data_mother <- moba_data_mother_all
rm(moba_data_mother_all)

## 1.5 Father's Moba data -------------------------------------------------------
# Only load it when needed, to be more careful with memory
load("data/data_moba_father_SCL_audit_adhd.rda")
head(moba_data_father) #114628

# 2. Merge mothers' moba data with child education  ###########################
## 2.1 Get registry ID and pedigree info for parents in Moba -------------------------------------
names(linkage_parents)[names(linkage_parents) == "PREG_ID_2601"] <- "preg_id"
moba_data_mother$preg_id <- as.numeric(moba_data_mother$preg_id )
moba_data_mother_id <- inner_join(moba_data_mother, linkage_parents) 
#114 606 #weirdly loss of 19

# sum(is.na(moba_data_mother_id$SU2PT_FATHER))
# sum(is.na(moba_data_mother_id$SU2PT_MOTHER))
# moba_data_mother_id[which(is.na(moba_data_mother_id$SU2PT_MOTHER) == T), ]
# sum(is.na(moba_data_mother$preg_id))
# 
# linkage[which(linkage$PREG_ID_2601 == "11810"), ]
# linkage_parents[which(linkage_parents$preg_id == "11810"), ]

names(moba_data_mother_id)[names(moba_data_mother_id) == "SU2PT_MOTHER"] <- "mother_lnr"
names(moba_data_mother_id)[names(moba_data_mother_id) == "SU2PT_FATHER"] <- "father_lnr"

## 2.2 Get parental educational attainment ---- 
moba_data_mother_id_ea <- left_join(moba_data_mother_id, edu_dat_nodup, by = c("mother_lnr" = "w19_0634_lnr") )
head(moba_data_mother_id_ea)
colnames(moba_data_mother_id_ea)[18:21] <- paste("mother", colnames(moba_data_mother_id_ea)[18:21],  sep = "_")
moba_data_mother_id_ea <- left_join(moba_data_mother_id_ea, edu_dat_nodup, by = c("father_lnr" = "w19_0634_lnr") )
colnames(moba_data_mother_id_ea)[22:25] <- paste("father", colnames(moba_data_mother_id_ea)[22:25],  sep = "_")

rm(moba_data_mother)
rm(moba_data_mother_id)

## 2.3 Get parents ID, and others pedigree info in the school performance data ---- 
school_performance <- left_join(final_data_educ, kinship)
head(school_performance)

rm(kinship)


## 2.4 Merge education with mother, all kids -------------------------------------
school_mother_moba_allkids <- merge(moba_data_mother_id_ea, school_performance, by="mother_lnr")
head(school_mother_moba_allkids)
# This gives me all the kids of the mother in Moba, not only the kid relevant to the pregnancy 
# 213847


## 2.5 Merge education with mother, kids relevant to the pregnancy ---------------
head(linkage_child)
head(moba_data_mother_id_ea)
names(linkage_child)[names(linkage_child) == "PREG_ID_2601"] <- "preg_id"
names(linkage_child)[names(linkage_child) == "barn_nr"] <- "BARN_NR"
moba_data_mother_id_child <- inner_join(moba_data_mother_id_ea, linkage_child) 
# 106 455
names(moba_data_mother_id_child)[names(moba_data_mother_id_child) == "SU2PT_CHILD"] <- "w19_0634_lnr"
school_mother_moba_kidpreg <- merge(moba_data_mother_id_child, school_performance, by="w19_0634_lnr")
#98856
head(school_mother_moba_kidpreg)

## 2.6 Get birth year of parents -------------------------------------------------
# Not all birthyear of the parents are given by the parent_birthdate variable! 
school_mother_moba_allkids <- extract(school_mother_moba_allkids, mother_birthdate, into = c("mother_birthyear", "mother_birthmonth"), "(.{4})(.{2})", remove=FALSE)
school_mother_moba_allkids <- extract(school_mother_moba_allkids, father_birthdate, into = c("father_birthyear", "father_birthmonth"), "(.{4})(.{2})", remove=FALSE)
school_mother_moba_allkids$mother_birthyear <- as.numeric(school_mother_moba_allkids$mother_birthyear) 
school_mother_moba_allkids$father_birthyear <- as.numeric(school_mother_moba_allkids$father_birthyear) 

school_mother_moba_kidpreg <- extract(school_mother_moba_kidpreg, mother_birthdate, into = c("mother_birthyear", "mother_birthmonth"), "(.{4})(.{2})", remove=FALSE)
school_mother_moba_kidpreg <- extract(school_mother_moba_kidpreg, father_birthdate, into = c("father_birthyear", "father_birthmonth"), "(.{4})(.{2})", remove=FALSE)
school_mother_moba_kidpreg$mother_birthyear <- as.numeric(school_mother_moba_kidpreg$mother_birthyear) 
school_mother_moba_kidpreg$father_birthyear <- as.numeric(school_mother_moba_kidpreg$father_birthyear) 

## 2.7 Save data -----------------------------------------------------------------
save(school_mother_moba_allkids, file="data/school_mother_moba_allkids_211124.rda")
save(school_mother_moba_kidpreg, file="data/school_mother_moba_kidpreg_211124.rda")


## 2.8 Get only families with aunts (analysis sample) -----------------------------------------------

# Load maternal cousins list
load("data/maternal_grandparents_cousins_final_211103.rda")
head(maternal_grandparents_cousins_final) #2 123 112
maternal_grandparents_cousins_list <- maternal_grandparents_cousins_final$w19_0634_lnr

head(school_mother_moba_kidpreg)
school_mother_moba_cousins <- school_mother_moba_kidpreg[school_mother_moba_kidpreg$w19_0634_lnr %in% maternal_grandparents_cousins_list,] #82 480

head(school_mother_moba_cousins)
names(school_mother_moba_cousins)[names(school_mother_moba_cousins) == "mother_lnr.y"] <- "mother_lnr"
names(school_mother_moba_cousins)[names(school_mother_moba_cousins) == "father_lnr.y"] <- "father_lnr"

#remove incomplete families 
school_mother_moba_cousins <- school_mother_moba_cousins %>%
  group_by(maternal_grandparents) %>%
  filter(length(unique(mother_lnr))>1)
#11 447
table(duplicated(school_mother_moba_cousins$w19_0634_lnr)) #all false

save(school_mother_moba_cousins, file= "data/school_mother_moba_cousins_211124.rda")


# 3. Merge with father data ######################################################

## 3.1 Get registry ID and pedigree info for parents in Moba -------------------------------------
names(linkage_parents)[names(linkage_parents) == "PREG_ID_2601"] <- "preg_id"
moba_data_father$preg_id <- as.numeric(moba_data_father$preg_id )
moba_data_father_id <- inner_join(moba_data_father, linkage_parents) 
#114 609 #weirdly loss of 19

# sum(is.na(moba_data_father_id$SU2PT_FATHER))
# sum(is.na(moba_data_father_id$SU2PT_MOTHER))
# moba_data_father_id[which(is.na(moba_data_father_id$SU2PT_MOTHER) == T), ]
# sum(is.na(moba_data_father$preg_id))

names(moba_data_father_id)[names(moba_data_father_id) == "SU2PT_MOTHER"] <- "mother_lnr"
names(moba_data_father_id)[names(moba_data_father_id) == "SU2PT_FATHER"] <- "father_lnr"

## 3.2 Get parental educational attainment ---- 
moba_data_father_id_ea <- left_join(moba_data_father_id, edu_dat_nodup, by = c("father_lnr" = "w19_0634_lnr") )
head(moba_data_father_id_ea)
colnames(moba_data_father_id_ea)[18:21] <- paste("father", colnames(moba_data_father_id_ea)[18:21],  sep = "_")
moba_data_father_id_ea <- left_join(moba_data_father_id_ea, edu_dat_nodup, by = c("mother_lnr" = "w19_0634_lnr") )
colnames(moba_data_father_id_ea)[22:25] <- paste("mother", colnames(moba_data_father_id_ea)[22:25],  sep = "_")

rm(moba_data_father)
rm(moba_data_father_id)
rm(linkage_parents)
rm(edu_dat_nodup)

## 3.3 Get parents ID, and others pedigree info in the school performance data ---- 
school_performance <- left_join(final_data_educ, kinship)
head(school_performance)

rm(kinship)
rm(final_data_educ)


## 3.4 Merge education with father, all kids -------------------------------------
sum(is.na(moba_data_father_id_ea$father_lnr)) #25929  #114609
sum(is.na(school_performance$father_lnr))#14228  #700 247
#remove NAs, this seems to lead to issues
moba_data_father_id_ea <- moba_data_father_id_ea[!is.na(moba_data_father_id_ea$father_lnr),] #88680
school_performance <- school_performance[!is.na(school_performance$father_lnr),]

school_father_moba_allkids <- merge(moba_data_father_id_ea, school_performance, by="father_lnr")
head(school_father_moba_allkids)
# This gives me all the kids of the mother in Moba, not only the kid relevant to the pregnancy 
# 159 811


## 3.5 Merge education with father, kids relevant to the pregnancy ---------------
head(linkage_child)
head(moba_data_father_id_ea)
names(linkage_child)[names(linkage_child) == "PREG_ID_2601"] <- "preg_id"
names(linkage_child)[names(linkage_child) == "barn_nr"] <- "BARN_NR"
moba_data_father_id_child <- inner_join(moba_data_father_id_ea, linkage_child) 
# 86603
names(moba_data_father_id_child)[names(moba_data_father_id_child) == "SU2PT_CHILD"] <- "w19_0634_lnr"
school_father_moba_kidpreg <- merge(moba_data_father_id_child, school_performance, by="w19_0634_lnr")
#80424
head(school_father_moba_kidpreg)

## 3.6 Get birth year of parents -------------------------------------------------
school_father_moba_allkids <- extract(school_father_moba_allkids, mother_birthdate, into = c("mother_birthyear", "mother_birthmonth"), "(.{4})(.{2})", remove=FALSE)
school_father_moba_allkids <- extract(school_father_moba_allkids, father_birthdate, into = c("father_birthyear", "father_birthmonth"), "(.{4})(.{2})", remove=FALSE)
school_father_moba_allkids$mother_birthyear <- as.numeric(school_father_moba_allkids$mother_birthyear) 
school_father_moba_allkids$father_birthyear <- as.numeric(school_father_moba_allkids$father_birthyear) 

school_father_moba_kidpreg <- extract(school_father_moba_kidpreg, mother_birthdate, into = c("mother_birthyear", "mother_birthmonth"), "(.{4})(.{2})", remove=FALSE)
school_father_moba_kidpreg <- extract(school_father_moba_kidpreg, father_birthdate, into = c("father_birthyear", "father_birthmonth"), "(.{4})(.{2})", remove=FALSE)
school_father_moba_kidpreg$mother_birthyear <- as.numeric(school_father_moba_kidpreg$mother_birthyear) 
school_father_moba_kidpreg$father_birthyear <- as.numeric(school_father_moba_kidpreg$father_birthyear) 

## 3.7 Save data -----------------------------------------------------------------
save(school_father_moba_allkids, file="data/school_father_moba_allkids_211124.rda")
save(school_father_moba_kidpreg, file="data/school_father_moba_kidpreg_211124.rda")

rm(linkage_child)
rm(school_performance)

## 3.8 Get only families with uncles (analysis sample) -----------------------------------------------

# Load paternal cousins list
load("data/paternal_grandparents_cousins_final_211117.rda")
head(paternal_grandparents_cousins_final) #1 317205
paternal_grandparents_cousins_list <- paternal_grandparents_cousins_final$w19_0634_lnr

head(school_father_moba_kidpreg)
school_father_moba_cousins <- school_father_moba_kidpreg[school_father_moba_kidpreg$w19_0634_lnr %in% 
                                                           paternal_grandparents_cousins_list,] #39351

head(school_father_moba_cousins)
names(school_father_moba_cousins)[names(school_father_moba_cousins) == "mother_lnr.y"] <- "mother_lnr"
names(school_father_moba_cousins)[names(school_father_moba_cousins) == "father_lnr.y"] <- "father_lnr"

#remove incomplete families 
school_father_moba_cousins <- school_father_moba_cousins %>%
  group_by(paternal_grandparents) %>%
  filter(length(unique(father_lnr))>1)
#7333
table(duplicated(school_father_moba_cousins$w19_0634_lnr))#no duplicates

save(school_father_moba_cousins, file= "data/school_father_moba_cousins_211124.rda")


# 3. Get cross sex sib families ##################################################
rm(list=ls())

# Get list of cousins "crossex"
load("data/crossex_grandparents_cousins_final_211202.rda")

#Get data mothers
#save(school_mother_moba_allkids, file="data/school_mother_moba_allkids_211124.rda")
load(file="data/school_mother_moba_kidpreg_211124.rda")

head(school_mother_moba_kidpreg)

#Get data fathers 
#load("data/school_father_moba_allkids_211124.rda")
load("data/school_father_moba_kidpreg_211124.rda")

head(school_father_moba_kidpreg)


## 3.1 Select comparable parental traits ###################
school_mother_moba_kidpreg <- school_mother_moba_kidpreg %>%
  rename(SCL5_Q1 = scale_score_items_m_SCL_Q1_full, 
         ADHD = scale_score_items_m_ADHD_Q3,
         SCL8 = scale_score_items_m_SCL_Q8_full, 
         AUDIT = scale_score_items_m_AUDIT_Q8
         )

school_father_moba_kidpreg <- school_father_moba_kidpreg %>%
  rename(SCL5_Q1 = scale_score_items_f_SCL_Q1_SCL5, 
         ADHD = scale_score_items_f_ADHD_Q1,
         SCL8 = scale_score_items_f_SCL_Q2015_full, 
         AUDIT = scale_score_items_f_AUDIT_Q2015
  )


## If completion test > year of completion, then father trait is NA 
# Done in the analyses scripts

# Clean dataframe
school_mother_moba_kidpreg_cl <- school_mother_moba_kidpreg %>% 
  select(w19_0634_lnr,
         preg_id,
         m_id,
         f_id,
         BARN_NR,
         birth_yr,
         mother_lnr.y, father_lnr.y,
         sex,
         mother_birthyear, father_birthyear,
         maternal_grandparents, paternal_grandparents, parents,
         father_edu_2018, father_ISCED11_2018, father_EduYears11_2018,
         mother_edu_2018, mother_ISCED11_2018, mother_EduYears11_2018,
         year, std_score_NPENG05, std_score_NPLES05, std_score_NPREG05,
         SCL5_Q1, ADHD, SCL8, AUDIT)

school_father_moba_kidpreg_cl <- school_father_moba_kidpreg %>% 
  select(w19_0634_lnr,
         preg_id,
         m_id,
         f_id,
         BARN_NR,
         birth_yr,
         mother_lnr.y, father_lnr.y,
         sex,
         mother_birthyear, father_birthyear,
         maternal_grandparents, paternal_grandparents, parents,
         father_edu_2018, father_ISCED11_2018, father_EduYears11_2018,
         mother_edu_2018, mother_ISCED11_2018, mother_EduYears11_2018,
         year, std_score_NPENG05, std_score_NPLES05, std_score_NPREG05,
         SCL5_Q1, ADHD, SCL8, AUDIT)


## 3.2 Merge with list of cousins ##############


cousins_from_mother <- crossex_grandparents_cousins_final[crossex_grandparents_cousins_final$sib_parent == "mother",]
cousins_from_father <- crossex_grandparents_cousins_final[crossex_grandparents_cousins_final$sib_parent == "father",]

cousins_from_mother_list <- cousins_from_mother$w19_0634_lnr
cousins_from_father_list <- cousins_from_father$w19_0634_lnr

school_mother_moba_cousins_crossex <- school_mother_moba_kidpreg_cl[
  school_mother_moba_kidpreg_cl$w19_0634_lnr %in% 
    cousins_from_mother_list,] #36842

school_father_moba_cousins_crossex <- school_father_moba_kidpreg_cl[
  school_father_moba_kidpreg_cl$w19_0634_lnr %in% 
    cousins_from_father_list,] #21379

head(school_father_moba_cousins_crossex)
school_moba_cousins_crossex <- rbind(school_mother_moba_cousins_crossex, 
                                     school_father_moba_cousins_crossex) #58221


school_moba_cousins_crossex <- school_moba_cousins_crossex %>%
  rename(mother_lnr = mother_lnr.y,
         father_lnr = father_lnr.y
  )

#get info on side of the family 
head(crossex_grandparents_cousins_final)
info_family <- crossex_grandparents_cousins_final %>% 
  select(w19_0634_lnr, shared_grandparents_id, sib_parent_id, sib_parent) 

school_moba_cousins_crossex <- merge(school_moba_cousins_crossex, info_family, by = "w19_0634_lnr")

# remove incomplete families 
school_moba_cousins_crossex <- school_moba_cousins_crossex %>%
  group_by(shared_grandparents_id) %>%
  filter(length(unique(sib_parent_id))>1)
# 13 262  #Loose 46000 children 
table(duplicated(school_moba_cousins_crossex)) # no duplicates

save(school_moba_cousins_crossex, file= "data/school_moba_cousins_crossex_211202.rda")



# 4. Merge paternal, maternal, and crossex cousins ############################

load("data/total_grandparents_cousins_final_211207.rda")

#Get data mothers
load(file="data/school_mother_moba_kidpreg_211124.rda")

#Get data fathers 
load("data/school_father_moba_kidpreg_211124.rda")

head(school_father_moba_kidpreg)


## 4.1 Select comparable parental traits ###################
school_mother_moba_kidpreg <- school_mother_moba_kidpreg %>%
  rename(SCL5_Q1 = scale_score_items_m_SCL_Q1_full, 
         ADHD = scale_score_items_m_ADHD_Q3,
         SCL8 = scale_score_items_m_SCL_Q8_full, 
         AUDIT = scale_score_items_m_AUDIT_Q8
  )

school_father_moba_kidpreg <- school_father_moba_kidpreg %>%
  rename(SCL5_Q1 = scale_score_items_f_SCL_Q1_SCL5, 
         ADHD = scale_score_items_f_ADHD_Q1,
         SCL8 = scale_score_items_f_SCL_Q2015_full, 
         AUDIT = scale_score_items_f_AUDIT_Q2015
  )


# Clean dataframe
school_mother_moba_kidpreg_cl <- school_mother_moba_kidpreg %>% 
  select(w19_0634_lnr,
         preg_id,
         m_id,
         f_id,
         BARN_NR,
         birth_yr,
         mother_lnr.y, father_lnr.y,
         sex,
         mother_birthyear, father_birthyear,
         maternal_grandparents, paternal_grandparents, parents,
         father_edu_2018, father_ISCED11_2018, father_EduYears11_2018,
         mother_edu_2018, mother_ISCED11_2018, mother_EduYears11_2018,
         year, std_score_NPENG05, std_score_NPLES05, std_score_NPREG05,
         SCL5_Q1, ADHD, SCL8, AUDIT)

school_father_moba_kidpreg_cl <- school_father_moba_kidpreg %>% 
  select(w19_0634_lnr,
         preg_id,
         m_id,
         f_id,
         BARN_NR,
         birth_yr,
         mother_lnr.y, father_lnr.y,
         sex,
         mother_birthyear, father_birthyear,
         maternal_grandparents, paternal_grandparents, parents,
         father_edu_2018, father_ISCED11_2018, father_EduYears11_2018,
         mother_edu_2018, mother_ISCED11_2018, mother_EduYears11_2018,
         year, std_score_NPENG05, std_score_NPLES05, std_score_NPREG05,
         SCL5_Q1, ADHD, SCL8, AUDIT)


## 4.2 Merge with list of cousins ##############

cousins_from_mother <- total_grandparents_cousins_final[total_grandparents_cousins_final$sib_parent == "mother",]
cousins_from_father <- total_grandparents_cousins_final[total_grandparents_cousins_final$sib_parent == "father",]

cousins_from_mother_list <- cousins_from_mother$w19_0634_lnr
cousins_from_father_list <- cousins_from_father$w19_0634_lnr

school_mother_moba_cousins_total <- school_mother_moba_kidpreg_cl[
  school_mother_moba_kidpreg_cl$w19_0634_lnr %in% 
    cousins_from_mother_list,] #83371
school_mother_moba_cousins_total$sib_parent <- "mother"

school_mother_moba_cousins_total[duplicated(school_mother_moba_cousins_total$w19_0634_lnr),]
school_mother_moba_cousins_total[school_mother_moba_cousins_total$w19_0634_lnr == "P007036657",]
# something is going on with the linkage because the same w19 id is given to two different kids in moba
# these two kids seem to have the same parents, but one is born 2002, one in 2006
# to investigate later # These kids are not in the final sample

school_father_moba_cousins_total <- school_father_moba_kidpreg_cl[
  school_father_moba_kidpreg_cl$w19_0634_lnr %in% 
    cousins_from_father_list,] #52018
school_father_moba_cousins_total$sib_parent <- "father"

school_father_moba_cousins_total[duplicated(school_father_moba_cousins_total$w19_0634_lnr),]
school_father_moba_cousins_total[school_father_moba_cousins_total$w19_0634_lnr == "P007036657",]
# same kid is duplicated as for the mother

head(school_father_moba_cousins_total)
school_moba_cousins_total <- rbind(school_mother_moba_cousins_total, 
                                     school_father_moba_cousins_total) #135389

sum(duplicated(school_moba_cousins_total$w19_0634_lnr)) #43228 individual are in two different sibships


school_moba_cousins_total <- school_moba_cousins_total %>%
  rename(mother_lnr = mother_lnr.y,
         father_lnr = father_lnr.y
  )

head(school_moba_cousins_total) #information on the kid and its own variables 
head(total_grandparents_cousins_final) #information on the family of the kid 

#get info on side of the family 
#Same individual can be in different families (by mother and by father side )
info_family <- total_grandparents_cousins_final %>% 
  select(w19_0634_lnr, shared_grandparents_id, sib_parent_id, sib_parent) 

sum(duplicated(info_family$w19_0634_lnr)) #1215735

school_moba_cousins_total <- merge(school_moba_cousins_total, info_family, by = c("w19_0634_lnr", "sib_parent"))
#135389

sum(duplicated(school_moba_cousins_total$w19_0634_lnr)) 
head(school_moba_cousins_total[duplicated(school_moba_cousins_total$w19_0634_lnr),])
school_moba_cousins_total[school_moba_cousins_total$w19_0634_lnr == "P000000163",]

# remove incomplete families 
school_moba_cousins_total <- school_moba_cousins_total %>%
  group_by(shared_grandparents_id) %>%
  filter(length(unique(sib_parent_id))>1)
# 29393

sum(duplicated(school_moba_cousins_total$w19_0634_lnr)) 
#still 2324 kids in two sibships 

save(school_moba_cousins_total, file= "data/school_moba_cousins_total_211208.rda")

# 5. Get everyone, not only with cousins ################

school_mother_moba_kidpreg_cl$sib_parent <- "mother"
school_father_moba_kidpreg_cl$sib_parent <- "father"

school_mother_moba_kidpreg_cl$sib_parent_id <- school_mother_moba_kidpreg_cl$mother_lnr.y
school_father_moba_kidpreg_cl$sib_parent_id <- school_father_moba_kidpreg_cl$father_lnr.y

school_mother_moba_kidpreg_cl$grandparents <- school_mother_moba_kidpreg_cl$maternal_grandparents
school_father_moba_kidpreg_cl$grandparents <- school_father_moba_kidpreg_cl$paternal_grandparents

school_moba_kidpreg_total <- rbind(school_mother_moba_kidpreg_cl, school_father_moba_kidpreg_cl)
save(school_moba_kidpreg_total, file= "data/school_moba_kidpreg_total_211208.rda")
