# Perline Demange 
# Identify children and siblings in parental generation, using the registry data 
# prefixlink function was adapted from ancestors-three-gen.R, created by Eivind Ystrom 
# Start date: 20/10/2021


# Libraries and set up ##############################
library(tidyverse)

# This is everyone in the register
basic <- data.table::fread("../../../data/registers/original/csv/w19_0634_faste_oppl_ut.csv", 
                           strip.white=TRUE)
head(basic) # there are 8 423 884 individuals 


# 1. Get IDs and (pseudo-extended) family IDs ########################

# This function prefixes the Id numbers of mother and father in a dataframe
prefixlink <- function(prefix, df) {
  n<- names(df)
  names(df) <- c("w19_0634_lnr", str_c(prefix, n[2:5]))
  return(df)
}

## 1.1 Get everybody and their moms and dads ##############
g0 <- basic %>% 
  select(w19_0634_lnr, mother_lnr = mor_lnr, father_lnr = far_lnr, sex=kjoenn, 
         birthdate=foedsels_aar_mnd)
nrow(g0)
head(g0)


## 1.2 Get everybody, their parents and the parents of their parents (grandparents): #######
# moms and dads moms and dads
g1 <- g0 %>% 
  left_join(prefixlink("mother_", g0), by=c(mother_lnr = "w19_0634_lnr")) %>% 
  left_join(prefixlink("father_", g0), by=c(father_lnr = "w19_0634_lnr"))
nrow(g1)


## 1.3 Make missing data uniformly noted #######
naval <- as.list(rep("", length.out=length(names(g1))))
names(naval) <- names(g1)

kinship <- g1 %>% 
  replace_na(naval)

## 1.4 Create pseudo-extended Family ID: #####
# identify a maternal or paternal grandparent couple
kinship$maternal_grandparents <- paste0(kinship$mother_mother_lnr,kinship$mother_father_lnr)
kinship$paternal_grandparents <- paste0(kinship$father_mother_lnr,kinship$father_father_lnr)
head(kinship)

## 1.5 Create restricted family ID: identify parents  ######
kinship$parents <- paste0(kinship$mother_lnr,kinship$father_lnr)

## 1.6  Change format of NAs #####

kinship[kinship == ""] <- NA #8423884

#save(kinship, file = "pedigree_211020.rda")
load("data/pedigree_211020.rda")

# 2. Identify all full siblings in the population ##############################
## 2.1 Select individual with both parental information ----
both_parents <- kinship[is.na(kinship$mother_lnr) == F & 
                          is.na(kinship$father_lnr) == F,] # 4 659 517

## 2.2 Exclude individuals with same sex parents -----------
both_parents <- both_parents[both_parents$mother_sex == 2 &
                               both_parents$father_sex == 1,] #4 656 964  
# There are 2553 individuals with same sex parents 

## 2.3 Keep only sibship --------
shared_parents <- both_parents %>% group_by(parents) %>% filter(n()>1) #3 942 659 

head(shared_parents)

## 2.4.Identify twins  ------------------

data_twin <- shared_parents %>%
  group_by(parents) %>% 
  mutate(multiple = ifelse(duplicated(birthdate) | duplicated(birthdate, fromLast=TRUE), T, F))
summary(data_twin$multiple)
                                      
#get number of multiples within family to investigate potential problems
summary <- data_twin %>%
  group_by(parents)%>%
  tally(multiple == T)
table(summary$n)
summary[summary$n == 7,]
#only missing data is land of birth, so it doesnt tell us more if these are biological septuplets or adopted. 
# I will go on considering to the best of my knowledge that all of them are twins 
head(summary)


## 2.5. NOT CURRENTLY DONE - Exclude siblings born within 6 months (but not born on the same date) ----------
library(lubridate)
#transform birthdate into full date to do time calculation
data_sib <- extract(data_twin, birthdate, into = c("birthyear", "birthmonth"), "(.{4})(.{2})", remove=FALSE)
data_sib$birthday <- paste0(data_sib$birthyear, "-", data_sib$birthmonth)
data_sib$birthday <- ym(data_sib$birthday)
head(data_sib$birthday)

save(data_sib, file = "data/pedigree_211103_temp.rda")
load("data/pedigree_211103_temp.rda")
# 
# #quick function returning T if a pair is born less than 6 months apart in the same family
# is_lower_6_months <- function(x,y){
#   person1 <- data_sib[data_sib$parents == family, ][x,]
#   person2 <- data_sib[data_sib$parents == family, ][y,]
#   ifelse(abs(person1$birthday - person2$birthday) < months(7), T, F)
# }
# #use months(7) and not 6 or person born exactly at 6 months return F, while we want T
# 
# 
# #Run a loop on all families looking at every siblings combinations
# # in DUTCH data this loop takes more than a day to run!
# # In norwegian data: more than 3 days... #I had to kill it than there was a memory issues
# families <- unique(data_sib$parents) #parents is the family iD
# closer_than_6_months <- NULL
# for (family in families){
#   number_family_members <- nrow(data_sib[data_sib$parents==family, ])
#   birthday_occurence <- matrix(0, nrow=number_family_members,
#                                ncol=number_family_members)
#   siblings <- data_sib[data_sib$parents == family, ]$w19_0634_lnr
#   possible_combi <- combn(length(siblings),2)
#   for (combi in 1:ncol(possible_combi)){
#     i <- possible_combi[1, combi]
#     j <- possible_combi[2, combi]
#     birthday_occurence[i,j] <- is_lower_6_months(i,j)
#     birthday_occurence[j,i] <- birthday_occurence[i,j]
#   }
#   for (individual in 1:ncol(birthday_occurence)){
#     closer_than_6_months <- c(closer_than_6_months,
#                               ifelse(sum(birthday_occurence[individual,]) > 0,
#                                      T,F))
#   }
# }
# 
# data_sib$closer_than_6_months <- closer_than_6_months
# 
# # exclude siblings closer than 6 months but keep twins
# data_sib$closer_than_6_months_not_multiple <- ifelse(data_sib$closer_than_6_months == T &
#                                                        data_sib$multiple == F, T, F)
# data_final <- data_sib[data_sib$closer_than_6_months_not_multiple == F, ]
# 
# # Only sibship
# data_final <- data_final %>% group_by(parents) %>% filter(n()>1) #3 942 659

## 2.7. Save list of full siblings -------------
data_final <- data_sib 
save(data_final, file = "allsibs_cleaned_211117.rda")
load("allsibs_cleaned_211117.rda")


# 3. Identify sisters in the parental generation and get list of maternal cousins ###############################

## 3.1 Identify everyone who has data on (at least one of) their maternal grandparents ------
atleastone_maternal_grandparents <-  kinship[is.na(kinship$maternal_grandparents) == F,] 

## 3.2 Identify individual whose maternal grandparents who had at least 2 daughters in the data  ------
shared_maternal_grandparents <- atleastone_maternal_grandparents %>% 
  group_by(maternal_grandparents) %>%
  filter(length(unique(mother_lnr))>1) #2 599 874
head(shared_maternal_grandparents)

## 3.3 Exclude individuals whose mothers are not part of the full siblings group (both parents, no same sex parents) -------
siblingsID <- data_final$w19_0634_lnr

real_shared_maternal_grandparents <- shared_maternal_grandparents[shared_maternal_grandparents$mother_lnr %in% siblingsID, ] #2 123 112

# Identify individuals whose maternal grandparents who still have at least 2 daughters in the data  
maternal_grandparents_cousins_final <- real_shared_maternal_grandparents %>% group_by(maternal_grandparents) %>% filter(n()>1) # 2 123 112

## 3.4 Save list of maternal cousins ------------------
save(maternal_grandparents_cousins_final, file = "data/maternal_grandparents_cousins_final_211103.rda")

## 3.5 Some descriptives -----------------
number_of_siblings <- table(maternal_grandparents_cousins_final$maternal_grandparents)
number_of_siblings <- as.data.frame(t(number_of_siblings))
table(number_of_siblings$Freq) # number of grandchildren with the same maternal grandparents # so number of grandchildren whose mothers are sisters


#4. Identify brothers in the parental generation and get list of paternal cousins ###############

## 4.1 Identify everyone who has data on (at least one of) their paternal grandparents ------
atleastone_paternal_grandparents <-  kinship[is.na(kinship$paternal_grandparents) == F,] 

## 4.2 Identify individual whose paternal grandparents who had at least 2 sons in the data  ------
shared_paternal_grandparents <- atleastone_paternal_grandparents %>% 
  group_by(paternal_grandparents) %>%
  filter(length(unique(father_lnr))>1) #1 384 171
head(shared_paternal_grandparents)

## 4.3 Exclude individuals whose fathers are not part of the full siblings group (both parents, no same sex parents) -------
siblingsID <- data_final$w19_0634_lnr

real_shared_paternal_grandparents <- shared_paternal_grandparents[shared_paternal_grandparents$father_lnr %in% siblingsID, ] #1 317 205

# Identify individuals whose paternal grandparents who still have at least 2 sons in the data  
paternal_grandparents_cousins_final <- real_shared_paternal_grandparents %>% group_by(paternal_grandparents) %>% filter(n()>1) # 1 317 205

## 4.4 Save list of paternal cousins ------------------
save(paternal_grandparents_cousins_final, file = "data/paternal_grandparents_cousins_final_211117.rda")

## 4.5 Some descriptives -----------------
number_of_siblings <- table(paternal_grandparents_cousins_final$paternal_grandparents)
number_of_siblings <- as.data.frame(t(number_of_siblings))
table(number_of_siblings$Freq) 

# 5. Identify cross sex siblings and their children ##############################

## 5.1 Identify everyone who has data on (at least one of) their grandparents ------
atleastone_paternal_grandparents <-  kinship[is.na(kinship$paternal_grandparents) == F,] 
atleastone_maternal_grandparents <-  kinship[is.na(kinship$maternal_grandparents) == F,] 
atleastone_grandparents <-  kinship[is.na(kinship$maternal_grandparents) == F | is.na(kinship$paternal_grandparents) == F ,] 
rm(kinship)

## 5.2 Identify all maternal grandparents who are also present in the paternal grandparents (and reverse) list 
maternal_grandparents_id <- atleastone_maternal_grandparents$maternal_grandparents
paternal_grandparents_id <- atleastone_paternal_grandparents$paternal_grandparents

shared_grandparents_mat <- atleastone_maternal_grandparents[atleastone_maternal_grandparents$maternal_grandparents %in% 
                                                              paternal_grandparents_id,]$maternal_grandparents
shared_grandparents_pat <- atleastone_paternal_grandparents[atleastone_paternal_grandparents$paternal_grandparents %in% 
                                                              maternal_grandparents_id,]$paternal_grandparents

shared_grandparents <- c(shared_grandparents_mat, shared_grandparents_pat)
shared_grandparents <- unique(shared_grandparents)


rm(kinship)
rm(data_final)
rm(shared_grandparents_mat)
rm(shared_grandparents_pat)
rm(atleastone_maternal_grandparents)
rm(atleastone_paternal_grandparents)

# Identify #takes a while 
maternal_grandparents <- atleastone_grandparents$maternal_grandparents
paternal_grandparents <- atleastone_grandparents$paternal_grandparents
mother_id <- atleastone_grandparents$mother_lnr
father_id <- atleastone_grandparents$father_lnr
shared_grandparents_id <- c()
sib_parent_id <- c()
sib_parent <- c()
for (i in 1:nrow(atleastone_grandparents)){
    if(maternal_grandparents[i] %in% shared_grandparents){
      shared_grandparents_id[i] <- maternal_grandparents[i]
      sib_parent_id[i] <- mother_id[i]
      sib_parent[i] <- "mother"
    } else if(paternal_grandparents[i] %in% shared_grandparents){
      shared_grandparents_id[i] <- paternal_grandparents[i] 
      sib_parent_id[i] <- father_id[i]
      sib_parent[i] <- "father"
    } else{ 
    shared_grandparents_id[i] <- NA 
    sib_parent_id[i] <- NA
    sib_parent[i] <- NA
    }
}
atleastone_grandparents$shared_grandparents_id <- shared_grandparents_id
atleastone_grandparents$sib_parent_id <- sib_parent_id
atleastone_grandparents$sib_parent <- sib_parent

save(atleastone_grandparents, file = "data/crossex_grandparents_cousins_temp.rda")
#3165566

## 5.2 Identify individual whose  grandparents who had at least 2 children in the data  ------
shared_grandparents <- atleastone_grandparents %>% 
  group_by(shared_grandparents_id) %>%
  filter(length(unique(sib_parent_id))>1) 
head(shared_grandparents) #1 859 166

## 5.3 Exclude individuals whose father or mother are not part of the full siblings group (both parents, no same sex parents) -------
load("allsibs_cleaned_211117.rda")
siblingsID <- data_final$w19_0634_lnr

real_shared_grandparents <- shared_grandparents[shared_grandparents$sib_parent_id %in% siblingsID, ] #1 766 186

# Identify individuals whose grandparents who still have at least 2 children in the data  
crossex_grandparents_cousins_final <- real_shared_grandparents %>% 
  group_by(shared_grandparents_id) %>% 
  filter(length(unique(sib_parent_id))>1)  #1 766 186

## 5.4 Save list of cousins ------------------
save(crossex_grandparents_cousins_final, file = "data/crossex_grandparents_cousins_final_211202.rda")



# 6. Combine a list of cousins with maternal cousins, paternal cousins, and "crossex" ####

rm(list=ls())

load("data/maternal_grandparents_cousins_final_211103.rda")
load("data/paternal_grandparents_cousins_final_211117.rda")
load("data/crossex_grandparents_cousins_final_211202.rda")

## 6.1 Identify all cousins ####
head(maternal_grandparents_cousins_final)
head(paternal_grandparents_cousins_final)
head(crossex_grandparents_cousins_final)
maternal_grandparents_cousins_final$shared_grandparents_id <- maternal_grandparents_cousins_final$maternal_grandparents
maternal_grandparents_cousins_final$sib_parent_id <- maternal_grandparents_cousins_final$mother_lnr
maternal_grandparents_cousins_final$sib_parent <- "mother"

paternal_grandparents_cousins_final$shared_grandparents_id <- paternal_grandparents_cousins_final$paternal_grandparents
paternal_grandparents_cousins_final$sib_parent_id <- paternal_grandparents_cousins_final$father_lnr
paternal_grandparents_cousins_final$sib_parent <- "father"

total_sib_sample <- rbind(maternal_grandparents_cousins_final, paternal_grandparents_cousins_final, crossex_grandparents_cousins_final)
#5206503


## 6.2 Remove duplicated individuals within cousinship #######

total_sib_sample_cl <- total_sib_sample %>%
  group_by(shared_grandparents_id) %>%
  distinct(w19_0634_lnr, .keep_all= TRUE)
#3821505 

total_grandparents_cousins_final <- total_sib_sample_cl

# Some individuals will be in two families, from mother and from father side,
# what to do with them in analyses? 
sum(duplicated(total_grandparents_cousins_final$w19_0634_lnr)) #1215735 out of 3821505


## 6.3 Save list of cousins ------ 
save(total_grandparents_cousins_final, file = "data/total_grandparents_cousins_final_211207.rda")

## 6.4 Some descriptives --------
number_of_siblings <- table(total_grandparents_cousins_final$shared_grandparents_id)
number_of_siblings <- as.data.frame(t(number_of_siblings))
table(number_of_siblings$Freq) # number of grandchildren with the same grandparents 
hist(number_of_siblings$Freq)
