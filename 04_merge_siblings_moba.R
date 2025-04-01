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
load("data/school_performance_250106.rda")
head(final_data_educ) # 999812

## 1.2 Linkage equivalence between registry and Moba  --------
linkage <- fread("N:/durable/data/moba/linkage/merged_IDs/MoBa_SSB_IDs.csv") 
#new linkage file made by Clara, it should contain the linkage with the 05/2023 delivery
head(linkage) # 304 449

#remove sentrix because we do not need genetics here
linkage$SENTRIX_ID <- NULL

#preg-id is num instead of chr 
linkage$PREG_ID_2601 <- as.character(linkage$PREG_ID_2601)

#maybe some weird NAs
linkage_with_na <- linkage %>%
  filter(if_any(everything(), is.na))
#all rows have NAs (either child or parents id so it makes sense)
linkage[is.na(linkage$CHILD_NR),]
linkage[is.na(linkage$PARENT_ID),]
#both "types" of Nas are indeed considered Nas

#Get ID of parents in other columns
linkage_wide <- spread(linkage,ROLE,w19_0634_lnr)
head(linkage_wide)

#Check duplicates
tail(linkage_wide %>%
  filter(!is.na(Mother) & Mother %in% Mother[duplicated(Mother)]))
linkage_wide[!is.na(linkage_wide$Mother) & (linkage_wide$Mother == "P009875203"),]
tail(linkage_wide %>%
       filter(!is.na(Father) & Father %in% Father[duplicated(Father)]))
linkage_wide %>%
       filter(!is.na(Child) & Child %in% Child[duplicated(Child)]) #one duplicate

#remove Nas and duplicated child 
linkage_child <- linkage_wide %>%
  filter(!(Child %in% Child[duplicated(Child)])) 
head(linkage_child)
linkage_child$Father <- NULL
linkage_child$Mother <- NULL

linkage_mother <- drop_na(linkage_wide, Mother)
linkage_father<- drop_na(linkage_wide, Father)
head(linkage_mother)
head(linkage_father)
linkage_mother$Child <- NULL
linkage_mother$CHILD_NR <- NULL
linkage_mother$Father <- NULL
linkage_father$Child <- NULL
linkage_father$CHILD_NR <- NULL
linkage_father$Mother <- NULL
linkage_parents <- full_join(linkage_mother, linkage_father) #198258
tail(linkage_parents)
#need to be careful with memory, remove unused dataframes regularly 
rm(linkage)
rm(linkage_mother)
rm(linkage_father)
rm(linkage_wide)
rm(linkage_with_na)

## 1.3 Pedigree (get parental ID) ----------
#each row contains individual w19_0634_lnr and the mother and parents IDs lnr 
# Only load it when needed, to be more careful with memory
load("data/pedigree_241220.rda")
head(kinship)
#parentsID <- kinship[, 1:3]

## 1.4 educational attainment for parents ----------------------------------------
load("data/educational_attainement_250102.rda")
head(edu_dat_nodup)
sum(duplicated(edu_dat_nodup$w19_0634_lnr))

## 1.4 Mother's Moba data ------------------------------------------------------
#each row is a pregnancy ID and the mother data 
load("data/data_moba_mother_SCL_audit_adhd_ed_250103.rda")
head(moba_data_mother_all) #114 009 so less following the update as it was #114 625
moba_data_mother <- moba_data_mother_all
rm(moba_data_mother_all)


## 1.5 Father's Moba data -------------------------------------------------------
# Only load it when needed, to be more careful with memory
load("data/data_moba_father_SCL_audit_adhd_250103.rda")
head(moba_data_father) #114 012 so less following the update as it was#114628

# 2. Merge mothers' moba data with child education  ###########################
## 2.1 Get registry ID and pedigree info for parents in Moba -------------------------------------
names(linkage_parents)[names(linkage_parents) == "PREG_ID_2601"] <- "preg_id"
moba_data_mother_id <- merge(moba_data_mother, linkage_parents, 
                             by.x = c("m_id", "preg_id"), 
                             by.y = c("PARENT_ID", "preg_id")) 
#114005 #weirdly losing 4 

names(moba_data_mother_id)[names(moba_data_mother_id) == "Mother"] <- "mother_lnr"
names(moba_data_mother_id)[names(moba_data_mother_id) == "Father"] <- "father_lnr"

# get father_lnr
moba_data_mother_id <- left_join(moba_data_mother_id, linkage_parents, 
                             by = c("f_id" = "PARENT_ID", 
                                    "preg_id" = "preg_id")) 
head(moba_data_mother_id)
moba_data_mother_id$father_lnr <- NULL
moba_data_mother_id$Mother <- NULL
names(moba_data_mother_id)[names(moba_data_mother_id) == "Father"] <- "father_lnr"

## 2.2 Get parental educational attainment ---- 
moba_data_mother_id_ea <- left_join(moba_data_mother_id, edu_dat_nodup,
                                    by = c("mother_lnr" = "w19_0634_lnr") )

head(moba_data_mother_id_ea)
colnames(moba_data_mother_id_ea)[18:21] <- paste("mother", 
                                                 colnames(moba_data_mother_id_ea)[18:21], 
                                                 sep = "_")

moba_data_mother_id_ea <- left_join(moba_data_mother_id_ea, edu_dat_nodup,
                                    by = c("father_lnr" = "w19_0634_lnr") )
colnames(moba_data_mother_id_ea)[22:25] <- paste("father",
                                                 colnames(moba_data_mother_id_ea)[22:25],
                                                 sep = "_")
head(moba_data_mother_id_ea)
rm(moba_data_mother)
rm(moba_data_mother_id)

## 2.3 Get parents ID, and others pedigree info in the school performance data ---- 
school_performance <- left_join(final_data_educ, kinship)
head(school_performance)

rm(kinship)


## 2.4 Merge education with mother, all kids -------------------------------------
school_mother_moba_allkids <- merge(moba_data_mother_id_ea, 
                                    school_performance,
                                    by="mother_lnr")
head(school_mother_moba_allkids)
# This gives me all the kids of the mother in Moba, not only the kid relevant to the pregnancy 
# 252184, before update was 213847

## 2.5 Merge education with mother, kids relevant to the pregnancy ---------------
head(linkage_child)
head(moba_data_mother_id_ea)
names(linkage_child)[names(linkage_child) == "PREG_ID_2601"] <- "preg_id"
names(moba_data_mother_id_ea)[names(moba_data_mother_id_ea) == "BARN_NR"] <- "CHILD_NR"
moba_data_mother_id_child <- inner_join(moba_data_mother_id_ea, linkage_child) 
# 105919 before was more 106 455
names(moba_data_mother_id_child)[names(moba_data_mother_id_child) == "Child"] <- "w19_0634_lnr"
school_mother_moba_kidpreg <- merge(moba_data_mother_id_child, school_performance, by="w19_0634_lnr")
#101700
head(school_mother_moba_kidpreg)

## 2.6 Get birth year of parents -------------------------------------------------
# Not all birthyear of the parents are given by the parent_birthdate variable! 
school_mother_moba_allkids <- extract(school_mother_moba_allkids, mother_birthdate, 
                                      into = c("mother_birthyear", "mother_birthmonth"),
                                      "(.{4})(.{2})", remove=FALSE)
school_mother_moba_allkids <- extract(school_mother_moba_allkids, father_birthdate, 
                                      into = c("father_birthyear", "father_birthmonth"),
                                      "(.{4})(.{2})", remove=FALSE)
school_mother_moba_allkids$mother_birthyear <- as.numeric(school_mother_moba_allkids$mother_birthyear) 
school_mother_moba_allkids$father_birthyear <- as.numeric(school_mother_moba_allkids$father_birthyear) 

school_mother_moba_kidpreg <- extract(school_mother_moba_kidpreg, mother_birthdate,
                                      into = c("mother_birthyear", "mother_birthmonth"),
                                      "(.{4})(.{2})", remove=FALSE)
school_mother_moba_kidpreg <- extract(school_mother_moba_kidpreg, father_birthdate, 
                                      into = c("father_birthyear", "father_birthmonth"),
                                      "(.{4})(.{2})", remove=FALSE)
school_mother_moba_kidpreg$mother_birthyear <- as.numeric(school_mother_moba_kidpreg$mother_birthyear) 
school_mother_moba_kidpreg$father_birthyear <- as.numeric(school_mother_moba_kidpreg$father_birthyear) 

## 2.7 Save data -----------------------------------------------------------------
save(school_mother_moba_allkids, file="data/school_mother_moba_allkids_250103.rda")
save(school_mother_moba_kidpreg, file="data/school_mother_moba_kidpreg_250103.rda")

## 2.8 Get only families with aunts (analysis sample) -----------------------------------------------

# Load maternal cousins list
load("data/maternal_grandparents_cousins_final_241220.rda")
head(maternal_grandparents_cousins_final) #1 420 232 before was 2 123 112
maternal_grandparents_cousins_list <- maternal_grandparents_cousins_final$w19_0634_lnr

head(school_mother_moba_kidpreg)
school_mother_moba_cousins <- school_mother_moba_kidpreg[school_mother_moba_kidpreg$w19_0634_lnr %in%
                                                           maternal_grandparents_cousins_list,] #41 626 before was 82 480

head(school_mother_moba_cousins)
names(school_mother_moba_cousins)[names(school_mother_moba_cousins) == "mother_lnr.y"] <- "mother_lnr"
names(school_mother_moba_cousins)[names(school_mother_moba_cousins) == "father_lnr.y"] <- "father_lnr"

#remove incomplete families 
school_mother_moba_cousins <- school_mother_moba_cousins %>%
  group_by(maternal_grandparents) %>%
  filter(length(unique(mother_lnr))>1)
#12054 before was 11 447
table(duplicated(school_mother_moba_cousins$w19_0634_lnr)) #all false

save(school_mother_moba_cousins, file= "data/school_mother_moba_cousins_250103.rda")


# 3. Merge with father data ######################################################

## 3.1 Get registry ID and pedigree info for parents in Moba -------------------------------------
names(linkage_parents)[names(linkage_parents) == "PREG_ID_2601"] <- "preg_id"
moba_data_father_id <- merge(moba_data_father, linkage_parents, 
                             by.x = c("f_id", "preg_id"), 
                             by.y = c("PARENT_ID", "preg_id")) 
#87739


names(moba_data_father_id)[names(moba_data_father_id) == "Mother"] <- "mother_lnr"
names(moba_data_father_id)[names(moba_data_father_id) == "Father"] <- "father_lnr"

# get mother_lnr
moba_data_father_id <- left_join(moba_data_father_id, linkage_parents, 
                                 by = c("m_id" = "PARENT_ID", 
                                        "preg_id" = "preg_id")) 

head(moba_data_father_id)
moba_data_father_id$mother_lnr <- NULL
moba_data_father_id$Father <- NULL
names(moba_data_father_id)[names(moba_data_father_id) == "Mother"] <- "mother_lnr"


## 3.2 Get parental educational attainment ---- 
moba_data_father_id_ea <- left_join(moba_data_father_id, edu_dat_nodup,
                                    by = c("father_lnr" = "w19_0634_lnr") )
tail(moba_data_father_id_ea)
colnames(moba_data_father_id_ea)[18:21] <- paste("father",
                                                 colnames(moba_data_father_id_ea)[18:21], 
                                                 sep = "_")
moba_data_father_id_ea <- left_join(moba_data_father_id_ea, 
                                    edu_dat_nodup, 
                                    by = c("mother_lnr" = "w19_0634_lnr") )
colnames(moba_data_father_id_ea)[22:25] <- paste("mother", 
                                                 colnames(moba_data_father_id_ea)[22:25], 
                                                 sep = "_")

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
sum(is.na(moba_data_father_id_ea$father_lnr)) #0
sum(is.na(school_performance$father_lnr))#20363
#remove NAs, this seems to lead to issues
#moba_data_father_id_ea <- moba_data_father_id_ea[!is.na(moba_data_father_id_ea$father_lnr),] #88680
school_performance <- school_performance[!is.na(school_performance$father_lnr),]

school_father_moba_allkids <- merge(moba_data_father_id_ea, 
                                    school_performance, 
                                    by="father_lnr")
head(school_father_moba_allkids)
# This gives me all the kids of the mother in Moba, not only the kid relevant to the pregnancy 
# 194373 before was 159 811


## 3.5 Merge education with father, kids relevant to the pregnancy ---------------
head(linkage_child)
head(moba_data_father_id_ea)
names(linkage_child)[names(linkage_child) == "PREG_ID_2601"] <- "preg_id"
names(moba_data_father_id_ea)[names(moba_data_father_id_ea) == "BARN_NR"] <- "CHILD_NR"
moba_data_father_id_child <- inner_join(moba_data_father_id_ea, linkage_child) 
# 85712 before was 86603
names(moba_data_father_id_child)[names(moba_data_father_id_child) == 
                                   "Child"] <- "w19_0634_lnr"
school_father_moba_kidpreg <- merge(moba_data_father_id_child, 
                                    school_performance, by="w19_0634_lnr")
# 82440 before was 80424
head(school_father_moba_kidpreg)

## 3.6 Get birth year of parents -------------------------------------------------
school_father_moba_allkids <- extract(school_father_moba_allkids,
                                      mother_birthdate,
                                      into = c("mother_birthyear",
                                               "mother_birthmonth"), "(.{4})(.{2})",
                                      remove=FALSE)
school_father_moba_allkids <- extract(school_father_moba_allkids,
                                      father_birthdate, 
                                      into = c("father_birthyear",
                                               "father_birthmonth"), "(.{4})(.{2})",
                                      remove=FALSE)
school_father_moba_allkids$mother_birthyear <- as.numeric(school_father_moba_allkids$mother_birthyear) 
school_father_moba_allkids$father_birthyear <- as.numeric(school_father_moba_allkids$father_birthyear) 

school_father_moba_kidpreg <- extract(school_father_moba_kidpreg,
                                      mother_birthdate, 
                                      into = c("mother_birthyear",
                                               "mother_birthmonth"), "(.{4})(.{2})",
                                      remove=FALSE)
school_father_moba_kidpreg <- extract(school_father_moba_kidpreg,
                                      father_birthdate,
                                      into = c("father_birthyear", "father_birthmonth"), 
                                      "(.{4})(.{2})", remove=FALSE)
school_father_moba_kidpreg$mother_birthyear <- as.numeric(school_father_moba_kidpreg$mother_birthyear) 
school_father_moba_kidpreg$father_birthyear <- as.numeric(school_father_moba_kidpreg$father_birthyear) 

## 3.7 Save data -----------------------------------------------------------------
save(school_father_moba_allkids, file="data/school_father_moba_allkids_250103.rda")
save(school_father_moba_kidpreg, file="data/school_father_moba_kidpreg_250103.rda")

rm(linkage_child)
rm(school_performance)

## 3.8 Get only families with uncles (analysis sample) -----------------------------------------------

# Load paternal cousins list
load("data/paternal_grandparents_cousins_final_241220.rda")
head(paternal_grandparents_cousins_final) #1 422 424 before was 1 317 205
paternal_grandparents_cousins_list <- paternal_grandparents_cousins_final$w19_0634_lnr

head(school_father_moba_kidpreg)
school_father_moba_cousins <- school_father_moba_kidpreg[school_father_moba_kidpreg$w19_0634_lnr %in% 
                                                           paternal_grandparents_cousins_list,] #35438 before was 39351

head(school_father_moba_cousins)
names(school_father_moba_cousins)[names(school_father_moba_cousins) == "mother_lnr.y"] <- "mother_lnr"
names(school_father_moba_cousins)[names(school_father_moba_cousins) == "father_lnr.y"] <- "father_lnr"

#remove incomplete families 
school_father_moba_cousins <- school_father_moba_cousins %>%
  group_by(paternal_grandparents) %>%
  filter(length(unique(father_lnr))>1)
# 7618 before was 7333
table(duplicated(school_father_moba_cousins$w19_0634_lnr))#no duplicates

save(school_father_moba_cousins, file= "data/school_father_moba_cousins_250103.rda")


# 3. Get cross sex sib families ##################################################
rm(list=ls())

# Get list of cousins "crossex"
load("data/crossex_grandparents_cousins_final_250102.rda")

#Get data mothers
load(file="data/school_mother_moba_kidpreg_250103.rda")

head(school_mother_moba_kidpreg)

#Get data fathers 
load("data/school_father_moba_kidpreg_250103.rda")

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
         CHILD_NR,
         birth_yr,
         mother_lnr.y, father_lnr.y,
         sex,
         mother_birthyear, father_birthyear,
         maternal_grandparents, paternal_grandparents, parents,
         father_edu_2023, father_ISCED11_2023, father_EduYears11_2023,
         mother_edu_2023, mother_ISCED11_2023, mother_EduYears11_2023,
         year, std_score_NPENG05, std_score_NPLES05, std_score_NPREG05,
         SCL5_Q1, ADHD, SCL8, AUDIT)

school_father_moba_kidpreg_cl <- school_father_moba_kidpreg %>% 
  select(w19_0634_lnr,
         preg_id,
         m_id,
         f_id,
         CHILD_NR,
         birth_yr,
         mother_lnr.y, father_lnr.y,
         sex,
         mother_birthyear, father_birthyear,
         maternal_grandparents, paternal_grandparents, parents,
         father_edu_2023, father_ISCED11_2023, father_EduYears11_2023,
         mother_edu_2023, mother_ISCED11_2023, mother_EduYears11_2023,
         year, std_score_NPENG05, std_score_NPLES05, std_score_NPREG05,
         SCL5_Q1, ADHD, SCL8, AUDIT)


## 3.2 Merge with list of cousins ##############


cousins_from_mother <- crossex_grandparents_cousins_final[crossex_grandparents_cousins_final$sib_parent == "mother",]
cousins_from_father <- crossex_grandparents_cousins_final[crossex_grandparents_cousins_final$sib_parent == "father",]

cousins_from_mother_list <- cousins_from_mother$w19_0634_lnr
cousins_from_father_list <- cousins_from_father$w19_0634_lnr

school_mother_moba_cousins_crossex <- school_mother_moba_kidpreg_cl[
  school_mother_moba_kidpreg_cl$w19_0634_lnr %in% 
    cousins_from_mother_list,] #39101 before was 36842

school_father_moba_cousins_crossex <- school_father_moba_kidpreg_cl[
  school_father_moba_kidpreg_cl$w19_0634_lnr %in% 
    cousins_from_father_list,] #21332 before was 21379

head(school_father_moba_cousins_crossex)
school_moba_cousins_crossex <- rbind(school_mother_moba_cousins_crossex, 
                                     school_father_moba_cousins_crossex) 
#60433 before was 58221


school_moba_cousins_crossex <- school_moba_cousins_crossex %>%
  rename(mother_lnr = mother_lnr.y,
         father_lnr = father_lnr.y
  )

#get info on side of the family 
head(crossex_grandparents_cousins_final)
info_family <- crossex_grandparents_cousins_final %>% 
  select(w19_0634_lnr, shared_grandparents_id, sib_parent_id, sib_parent) 

school_moba_cousins_crossex <- merge(school_moba_cousins_crossex, 
                                     info_family, by = "w19_0634_lnr")

# remove incomplete families 
school_moba_cousins_crossex <- school_moba_cousins_crossex %>%
  group_by(shared_grandparents_id) %>%
  filter(length(unique(sib_parent_id))>1)
# 13651 before was 13 262  
table(duplicated(school_moba_cousins_crossex)) # no duplicates

save(school_moba_cousins_crossex, file= "data/school_moba_cousins_crossex_250103.rda")



# 4. Merge paternal, maternal, and crossex cousins ############################

load("data/total_grandparents_cousins_final_250102.rda")

#Get data mothers
load(file="data/school_mother_moba_kidpreg_250103.rda")

#Get data fathers 
load("data/school_father_moba_kidpreg_250103.rda")

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
         CHILD_NR,
         birth_yr,
         mother_lnr.y, father_lnr.y,
         sex,
         mother_birthyear, father_birthyear,
         maternal_grandparents, paternal_grandparents, parents,
         father_edu_2023, father_ISCED11_2023, father_EduYears11_2023,
         mother_edu_2023, mother_ISCED11_2023, mother_EduYears11_2023,
         year, std_score_NPENG05, std_score_NPLES05, std_score_NPREG05,
         SCL5_Q1, ADHD, SCL8, AUDIT)

school_father_moba_kidpreg_cl <- school_father_moba_kidpreg %>% 
  select(w19_0634_lnr,
         preg_id,
         m_id,
         f_id,
         CHILD_NR,
         birth_yr,
         mother_lnr.y, father_lnr.y,
         sex,
         mother_birthyear, father_birthyear,
         maternal_grandparents, paternal_grandparents, parents,
         father_edu_2023, father_ISCED11_2023, father_EduYears11_2023,
         mother_edu_2023, mother_ISCED11_2023, mother_EduYears11_2023,
         year, std_score_NPENG05, std_score_NPLES05, std_score_NPREG05,
         SCL5_Q1, ADHD, SCL8, AUDIT)


## 4.2 Merge with list of cousins ##############

cousins_from_mother <- total_grandparents_cousins_final[total_grandparents_cousins_final$sib_parent == "mother",]
cousins_from_father <- total_grandparents_cousins_final[total_grandparents_cousins_final$sib_parent == "father",]

cousins_from_mother_list <- cousins_from_mother$w19_0634_lnr
cousins_from_father_list <- cousins_from_father$w19_0634_lnr

school_mother_moba_cousins_total <- school_mother_moba_kidpreg_cl[
  school_mother_moba_kidpreg_cl$w19_0634_lnr %in% 
    cousins_from_mother_list,] #101700 before was 83371
school_mother_moba_cousins_total$sib_parent <- "mother"

school_mother_moba_cousins_total[duplicated(school_mother_moba_cousins_total$w19_0634_lnr),]

school_father_moba_cousins_total <- school_father_moba_kidpreg_cl[
  school_father_moba_kidpreg_cl$w19_0634_lnr %in% 
    cousins_from_father_list,] #53576 before was 52018
school_father_moba_cousins_total$sib_parent <- "father"

school_father_moba_cousins_total[duplicated(school_father_moba_cousins_total$w19_0634_lnr),]

head(school_father_moba_cousins_total)
school_moba_cousins_total <- rbind(school_mother_moba_cousins_total, 
                                     school_father_moba_cousins_total) #121517 before was 135389

sum(duplicated(school_moba_cousins_total$w19_0634_lnr)) #35143 (before was 43228) individual are in two different sibships


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

sum(duplicated(info_family$w19_0634_lnr)) 

school_moba_cousins_total <- merge(school_moba_cousins_total,
                                   info_family, 
                                   by = c("w19_0634_lnr", "sib_parent"))
#121517 before was 135389

sum(duplicated(school_moba_cousins_total$w19_0634_lnr)) 
head(school_moba_cousins_total[duplicated(school_moba_cousins_total$w19_0634_lnr),])
school_moba_cousins_total[school_moba_cousins_total$w19_0634_lnr == "P000000809",]
#got cousins on both sides 

# remove incomplete families 
school_moba_cousins_total <- school_moba_cousins_total %>%
  group_by(shared_grandparents_id) %>%
  filter(length(unique(sib_parent_id))>1)
# 29846 before was 29393

sum(duplicated(school_moba_cousins_total$w19_0634_lnr)) 
#still 2306 kids in two sibships 

save(school_moba_cousins_total, file= "data/school_moba_cousins_total_250103.rda")

# 5. Get everyone, not only with cousins ################

school_mother_moba_kidpreg_cl$sib_parent <- "mother"
school_father_moba_kidpreg_cl$sib_parent <- "father"

school_mother_moba_kidpreg_cl$sib_parent_id <- school_mother_moba_kidpreg_cl$mother_lnr.y
school_father_moba_kidpreg_cl$sib_parent_id <- school_father_moba_kidpreg_cl$father_lnr.y

school_mother_moba_kidpreg_cl$grandparents <- school_mother_moba_kidpreg_cl$maternal_grandparents
school_father_moba_kidpreg_cl$grandparents <- school_father_moba_kidpreg_cl$paternal_grandparents

school_moba_kidpreg_total <- rbind(school_mother_moba_kidpreg_cl, school_father_moba_kidpreg_cl)
save(school_moba_kidpreg_total, file= "data/school_moba_kidpreg_total_250103.rda")
