# Perline Demange 
# Run siblings analyses 
# Start date: 03/11/2021
# cleaned 09-01-23

rm(list=ls())
library(tidyverse)
library(nlme)
library(lme4)
source("scripts/Functions.R")
library(psych)
set.seed(42)
library(grid)
library(gridExtra)

# 1. All sibships  ###################################################################### 
## 1.1 Load data with cousins, remove cousins <10 in 2015 when father data #######
load("data/school_moba_cousins_total_211208.rda") #29393

school_moba_cousins_total <- extract(school_moba_cousins_total, year, 
                                      into = c("yeartest", "monthtest"),
                                      "(.{4})(.{2})", remove=T)
table(school_moba_cousins_total$yeartest)
school_moba_cousins_total$agein2015 <- 2015 - school_moba_cousins_total$birth_yr
table(school_moba_cousins_total$agein2015)

# father's audit and anxiety and depression SCL8 where asked in 2015, which might be after the kid 10yo 
# so I exclude kids > 10yo in 2015 for these data when the parent of relevance if the father (not a problem for SCL8 in mothers) 
school_moba_cousins_total$SCL8[
  school_moba_cousins_total$sib_parent == "father" & school_moba_cousins_total$agein2015 >10 ] <- NA
school_moba_cousins_total$AUDIT[
  school_moba_cousins_total$sib_parent == "father" & school_moba_cousins_total$agein2015 >10 ] <- NA

school_moba_cousins_total_NPREG05 <- school_moba_cousins_total %>% 
  drop_na(std_score_NPREG05) #29146
school_moba_cousins_total_NPLES05 <- school_moba_cousins_total %>% 
  drop_na(std_score_NPLES05) #28772
school_moba_cousins_total_NPENG05 <- school_moba_cousins_total %>%
  drop_na(std_score_NPENG05) #28901

## 1.2 Run all analyses in the subsets for each traits and school performance #########
items_parents <- c("SCL5_Q1", "ADHD", "SCL8", "AUDIT")


for (trait in 1:length(items_parents)){
  assign(paste0("results_NPREG05_",items_parents[trait]),
         run_analyses_cross(school_moba_cousins_total_NPREG05, 
                            "std_score_NPREG05", 
                            items_parents[trait], 
                            T))
  assign(paste0("results_NPLES05_",items_parents[trait]),
         run_analyses_cross(school_moba_cousins_total_NPLES05, 
                            "std_score_NPLES05", 
                            items_parents[trait], 
                            T))
  assign(paste0("results_NPENG05_",items_parents[trait]),
         run_analyses_cross(school_moba_cousins_total_NPENG05, 
                            "std_score_NPENG05", 
                            items_parents[trait], 
                            T))
}


## 1.3 Get population, within and between effects for all conditions + sample sizes #############
school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_parents
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    estimates <- c("Population", "Within_effect", "Between_effect")
    modpop_estimates <- results[[12]][2,]
    modwithin_within_estimates <- results[[13]][2,]
    modwithin_between_estimates <- results[[13]][3,]
    
    res <- as.data.frame(rbind(modpop_estimates, 
                               modwithin_within_estimates, 
                               modwithin_between_estimates))
    res <- cbind(estimates, res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    res$ICC <- results[[9]] 
    res$number_of_families <- results[[4]]
    res$number_of_siblings <- mean(results[[5]]$Freq) # this is table
    res$loss_duplicated <- results[[6]]  
    res$loss_missingbirthyear <- results[[7]]
    full_results <- rbind(full_results, res)
  }
}
head(full_results)
full_results_siblings_total <- full_results
#save
save(full_results_siblings_total, file = "output/full_results_siblings_total_230109.rda")

## 1.4 Get all covariates estimates ##################################################
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    modpop_estimates <- as.data.frame(results[[12]])
    modwithin_estimates <- as.data.frame(results[[13]])
    modpop_estimates$model <- "population"
    modwithin_estimates$model <- "siblings"
    res <- rbind(modpop_estimates, modwithin_estimates)
    res$estimates <- rownames(res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

full_results_siblings_total_allcov <- full_results
save(full_results_siblings_total_allcov, file = "output/full_results_siblings_total_allcov_230109.rda")



## 1.5 Get descriptive in table #####
school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_parents

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    covariances <- as.data.frame(results[[10]])
    covariances$schoolsub <- school_subject[i]
    covariances$trait <- parental_trait[j]
    full_results <- rbind(full_results, covariances)
  }
}
covariances <- full_results
save(covariances, file = "output/full_results_siblings_total_covariances_230109.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    means <- as.data.frame(results[[11]])
    means$schoolsub <- school_subject[i]
    means$trait <- parental_trait[j]
    full_results <- rbind(full_results, means)
  }
}
means <- full_results
save(means, file = "output/full_results_siblings_total_means_230109.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    descriptives <- as.data.frame(results[[8]])
    descriptives$schoolsub <- school_subject[i]
    descriptives$trait <- parental_trait[j]
    full_results <- rbind(full_results, descriptives)
  }
}

descriptives <- full_results
save(descriptives, file = "output/full_results_siblings_total_descriptives_230109.rda")



## 1.6 Adjusting for  EA ####################

for (trait in 1:length(items_parents)){
  assign(paste0("results_NPREG05_",items_parents[trait]),
         run_analyses_cross_EA(school_moba_cousins_total_NPREG05, 
                               "std_score_NPREG05", 
                               items_parents[trait], 
                               T))
  assign(paste0("results_NPLES05_",items_parents[trait]),
         run_analyses_cross_EA(school_moba_cousins_total_NPLES05, 
                               "std_score_NPLES05", 
                               items_parents[trait], 
                               T))
  assign(paste0("results_NPENG05_",items_parents[trait]),
         run_analyses_cross_EA(school_moba_cousins_total_NPENG05, 
                               "std_score_NPENG05", 
                               items_parents[trait], 
                               T))
}

### 1.6.1 Get population, within and between effects for all conditions + sample sizes #############
school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_parents
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    estimates <- c("Population", "Within_effect", "Between_effect")
    modpop_estimates <- results[[12]][2,]
    modwithin_within_estimates <- results[[13]][2,]
    modwithin_between_estimates <- results[[13]][3,]
    
    res <- as.data.frame(rbind(modpop_estimates, 
                               modwithin_within_estimates, 
                               modwithin_between_estimates))
    res <- cbind(estimates, res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    res$ICC <- results[[9]] 
    res$number_of_families <- results[[4]]
    res$number_of_siblings <- mean(results[[5]]$Freq) # this is table
    res$loss_duplicated <- results[[6]]  
    res$loss_missingbirthyear <- results[[7]]
    full_results <- rbind(full_results, res)
  }
}
head(full_results)
full_results_siblings_total <- full_results
#save
save(full_results_siblings_total, file = "output/full_results_siblings_total_EA_230109.rda")

### 1.6.2 Get all covariates estimates ##################################################
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    modpop_estimates <- as.data.frame(results[[12]])
    modwithin_estimates <- as.data.frame(results[[13]])
    modpop_estimates$model <- "population"
    modwithin_estimates$model <- "siblings"
    res <- rbind(modpop_estimates, modwithin_estimates)
    res$estimates <- rownames(res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

full_results_siblings_total_allcov <- full_results
save(full_results_siblings_total_allcov, file = "output/full_results_siblings_total_allcov_EA_230109.rda")


### 1.6.3  Get descriptive in table### 
school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_parents

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    covariances <- as.data.frame(results[[10]])
    covariances$schoolsub <- school_subject[i]
    covariances$trait <- parental_trait[j]
    full_results <- rbind(full_results, covariances)
  }
}
covariances <- full_results
save(covariances, file = "output/full_results_siblings_total_covariances_EA_230109.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    means <- as.data.frame(results[[11]])
    means$schoolsub <- school_subject[i]
    means$trait <- parental_trait[j]
    full_results <- rbind(full_results, means)
  }
}
means <- full_results
save(means, file = "output/full_results_siblings_total_means_EA_230109.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    descriptives <- as.data.frame(results[[8]])
    descriptives$schoolsub <- school_subject[i]
    descriptives$trait <- parental_trait[j]
    full_results <- rbind(full_results, descriptives)
  }
}

descriptives <- full_results
save(descriptives, file = "output/full_results_siblings_total_descriptives_EA_230109.rda")

# 2. For mothers ################################################################
## 2.1  Load data with maternal cousins #######################
load("data/school_mother_moba_cousins_211124.rda") #11447

school_mother_moba_cousins_NPREG05 <- school_mother_moba_cousins %>% 
  drop_na(std_score_NPREG05) #11 346
school_mother_moba_cousins_NPLES05 <- school_mother_moba_cousins %>% 
  drop_na(std_score_NPLES05) #11 188
school_mother_moba_cousins_NPENG05 <- school_mother_moba_cousins %>%
  drop_na(std_score_NPENG05) #11 261

## 2.2 Run all analyses in the subsets for each traits and school performance #########
items_mother <- c("scale_score_items_m_SCL_Q1_full",
                  "scale_score_items_m_SCL_Q1_dep" ,
                  "scale_score_items_m_SCL_Q8_full",
                  "scale_score_items_m_SCL_Q8_SCL5",
                  "scale_score_items_m_SCL_Q8_anx", 
                  "scale_score_items_m_SCL_Q8_dep", 
                  "scale_score_items_m_ADHD_Q3", 
                  "scale_score_items_m_AUDIT_Q8", 
                  "scale_score_items_m_ED_NRM_Q1", 
                  "scale_score_items_m_ED_NRM_Q8")


for (trait in 1:length(items_mother)){
  assign(paste0("results_NPREG05_",items_mother[trait]),
         run_analyses_mother(school_mother_moba_cousins_NPREG05, 
                             "std_score_NPREG05", 
                             items_mother[trait], 
                             T))
  assign(paste0("results_NPLES05_",items_mother[trait]),
         run_analyses_mother(school_mother_moba_cousins_NPLES05, 
                             "std_score_NPLES05", 
                             items_mother[trait], 
                             T))
  assign(paste0("results_NPENG05_",items_mother[trait]),
         run_analyses_mother(school_mother_moba_cousins_NPENG05, 
                             "std_score_NPENG05", 
                             items_mother[trait], 
                             T))
}


## 2.3 Get population, within and between effects for all conditions + sample sizes #############
school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_mother
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    estimates <- c("Population", "Within_effect", "Between_effect")
    modpop_estimates <- results[[12]][2,]
    modwithin_within_estimates <- results[[13]][2,]
    modwithin_between_estimates <- results[[13]][3,]
    
    res <- as.data.frame(rbind(modpop_estimates, 
                               modwithin_within_estimates, 
                               modwithin_between_estimates))
    res <- cbind(estimates, res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    res$ICC <- results[[9]] 
    res$number_of_families <- results[[4]]
    res$number_of_siblings <- mean(results[[5]]$Freq) # this is table
    res$loss_duplicated <- results[[6]]  
    res$loss_missingbirthyear <- results[[7]]
    full_results <- rbind(full_results, res)
  }
}
head(full_results)
full_results_siblings_mother <- full_results
#save
save(full_results_siblings_mother, file = "output/full_results_siblings_mother_2301012.rda")

## 2.4 Get all covariates estimates ##################################################
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    modpop_estimates <- as.data.frame(results[[12]])
    modwithin_estimates <- as.data.frame(results[[13]])
    modpop_estimates$model <- "population"
    modwithin_estimates$model <- "siblings"
    res <- rbind(modpop_estimates, modwithin_estimates)
    res$estimates <- rownames(res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

full_results_siblings_mother_allcov <- full_results
save(full_results_siblings_mother_allcov, file = "output/full_results_siblings_mother_allcov_230112.rda")


## 2.5 Get descriptive in table #######

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    covariances <- as.data.frame(results[[10]])
    covariances$schoolsub <- school_subject[i]
    covariances$trait <- parental_trait[j]
    full_results <- rbind(full_results, covariances)
  }
}
covariances <- full_results
save(covariances, file = "output/full_results_siblings_mother_covariances_230112.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    means <- as.data.frame(results[[11]])
    means$schoolsub <- school_subject[i]
    means$trait <- parental_trait[j]
    full_results <- rbind(full_results, means)
  }
}
means <- full_results
save(means, file = "output/full_results_siblings_mother_means_230112.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    descriptives <- as.data.frame(results[[8]])
    descriptives$schoolsub <- school_subject[i]
    descriptives$trait <- parental_trait[j]
    full_results <- rbind(full_results, descriptives)
  }
}

descriptives <- full_results
save(descriptives, file = "output/full_results_siblings_mother_descriptives_230112.rda")

## 2.6 Adjusting for EA #######

for (trait in 1:length(items_mother)){
  assign(paste0("results_NPREG05_",items_mother[trait]),
         run_analyses_mother_EA(school_mother_moba_cousins_NPREG05, 
                             "std_score_NPREG05", 
                             items_mother[trait], 
                             T))
  assign(paste0("results_NPLES05_",items_mother[trait]),
         run_analyses_mother_EA(school_mother_moba_cousins_NPLES05, 
                             "std_score_NPLES05", 
                             items_mother[trait], 
                             T))
  assign(paste0("results_NPENG05_",items_mother[trait]),
         run_analyses_mother_EA(school_mother_moba_cousins_NPENG05, 
                             "std_score_NPENG05", 
                             items_mother[trait], 
                             T))
}


### 2.6.1 Get population, within and between effects for all conditions + sample sizes #############
school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_mother
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    estimates <- c("Population", "Within_effect", "Between_effect")
    modpop_estimates <- results[[12]][2,]
    modwithin_within_estimates <- results[[13]][2,]
    modwithin_between_estimates <- results[[13]][3,]
    
    res <- as.data.frame(rbind(modpop_estimates, 
                               modwithin_within_estimates, 
                               modwithin_between_estimates))
    res <- cbind(estimates, res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    res$ICC <- results[[9]] 
    res$number_of_families <- results[[4]]
    res$number_of_siblings <- mean(results[[5]]$Freq) # this is table
    res$loss_duplicated <- results[[6]]  
    res$loss_missingbirthyear <- results[[7]]
    full_results <- rbind(full_results, res)
  }
}
head(full_results)
full_results_siblings_mother <- full_results
#save
save(full_results_siblings_mother, file = "output/full_results_siblings_mother_EA_230112.rda")

### 2.6.2 Get all covariates estimates ##################################################
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    modpop_estimates <- as.data.frame(results[[12]])
    modwithin_estimates <- as.data.frame(results[[13]])
    modpop_estimates$model <- "population"
    modwithin_estimates$model <- "siblings"
    res <- rbind(modpop_estimates, modwithin_estimates)
    res$estimates <- rownames(res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

full_results_siblings_mother_allcov <- full_results
save(full_results_siblings_mother_allcov, file = "output/full_results_siblings_mother_allcov_EA_230112.rda")


### 2.6.3  Get descriptive in table ######


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    covariances <- as.data.frame(results[[10]])
    covariances$schoolsub <- school_subject[i]
    covariances$trait <- parental_trait[j]
    full_results <- rbind(full_results, covariances)
  }
}
covariances <- full_results
save(covariances, file = "output/full_results_siblings_mother_covariances_EA_230112.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    means <- as.data.frame(results[[11]])
    means$schoolsub <- school_subject[i]
    means$trait <- parental_trait[j]
    full_results <- rbind(full_results, means)
  }
}
means <- full_results
save(means, file = "output/full_results_siblings_mother_means_EA_230112.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    descriptives <- as.data.frame(results[[8]])
    descriptives$schoolsub <- school_subject[i]
    descriptives$trait <- parental_trait[j]
    full_results <- rbind(full_results, descriptives)
  }
}

descriptives <- full_results
save(descriptives, file = "output/full_results_siblings_mother_descriptives_EA_230112.rda")


# 3. For fathers ################################################################
## 3.1  Load data with paternal cousins, remove cousins <10 in 2015  #######################
# Load data with paternal cousins 
load("data/school_father_moba_cousins_211124.rda") #7333

school_father_moba_cousins <- extract(school_father_moba_cousins, year, 
                                      into = c("yeartest", "monthtest"),
                                      "(.{4})(.{2})", remove=T)
table(school_father_moba_cousins$yeartest)
school_father_moba_cousins$agein2015 <- 2015 - school_father_moba_cousins$birth_yr
table(school_father_moba_cousins$agein2015)

school_father_moba_cousins$scale_score_items_f_SCL_Q2015_full[school_father_moba_cousins$agein2015 >10 ] <- NA
sum(is.na(school_father_moba_cousins$scale_score_items_f_SCL_Q2015_SCL5))
school_father_moba_cousins$scale_score_items_f_SCL_Q2015_SCL5[school_father_moba_cousins$agein2015 >10 ] <- NA
school_father_moba_cousins$scale_score_items_f_SCL_Q2015_anx[school_father_moba_cousins$agein2015 >10 ] <- NA
school_father_moba_cousins$scale_score_items_f_SCL_Q2015_dep[school_father_moba_cousins$agein2015 >10 ] <- NA
school_father_moba_cousins$scale_score_items_f_AUDIT_Q2015[school_father_moba_cousins$agein2015 >10 ] <- NA

#Data with outcomes
school_father_moba_cousins_NPREG05 <-school_father_moba_cousins %>% 
  drop_na(std_score_NPREG05) #7280
school_father_moba_cousins_NPLES05 <-school_father_moba_cousins %>% 
  drop_na(std_score_NPLES05) #7198
school_father_moba_cousins_NPENG05 <-school_father_moba_cousins %>%
  drop_na(std_score_NPENG05) #7221

## 3.2 Run all analyses in the subsets for each traits and school performance #########
items_father <- c("scale_score_items_f_SCL_Q1_full", 
                  "scale_score_items_f_SCL_Q1_dep",
                  "scale_score_items_f_SCL_Q1_anx", 
                  "scale_score_items_f_SCL_Q1_SCL5", 
                  "scale_score_items_f_SCL_Q2015_full", 
                  "scale_score_items_f_SCL_Q2015_SCL5",
                  "scale_score_items_f_SCL_Q2015_anx", 
                  "scale_score_items_f_SCL_Q2015_dep", 
                  "scale_score_items_f_ADHD_Q1",
                  "scale_score_items_f_AUDIT_Q2015")


for (trait in 1:length(items_father)){
  assign(paste0("results_NPREG05_",items_father[trait]),
         run_analyses_father(school_father_moba_cousins_NPREG05, 
                             "std_score_NPREG05", 
                             items_father[trait], 
                             T))
  assign(paste0("results_NPLES05_",items_father[trait]),
         run_analyses_father(school_father_moba_cousins_NPLES05, 
                             "std_score_NPLES05", 
                             items_father[trait], 
                             T))
  assign(paste0("results_NPENG05_",items_father[trait]),
         run_analyses_father(school_father_moba_cousins_NPENG05, 
                             "std_score_NPENG05", 
                             items_father[trait], 
                             T))
}


## 3.3 Get population, within and between effects for all conditions + sample sizes #############
school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_father
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    estimates <- c("Population", "Within_effect", "Between_effect")
    modpop_estimates <- results[[12]][2,]
    modwithin_within_estimates <- results[[13]][2,]
    modwithin_between_estimates <- results[[13]][3,]
    
    res <- as.data.frame(rbind(modpop_estimates, 
                               modwithin_within_estimates, 
                               modwithin_between_estimates))
    res <- cbind(estimates, res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    res$ICC <- results[[9]] 
    res$number_of_families <- results[[4]]
    res$number_of_siblings <- mean(results[[5]]$Freq) # this is table
    res$loss_duplicated <- results[[6]]  
    res$loss_missingbirthyear <- results[[7]]
    full_results <- rbind(full_results, res)
  }
}
head(full_results)
full_results_siblings_father <- full_results
#save
save(full_results_siblings_father, file = "output/full_results_siblings_father_2301012.rda")

## 3.4 Get all covariates estimates ##################################################
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    modpop_estimates <- as.data.frame(results[[12]])
    modwithin_estimates <- as.data.frame(results[[13]])
    modpop_estimates$model <- "population"
    modwithin_estimates$model <- "siblings"
    res <- rbind(modpop_estimates, modwithin_estimates)
    res$estimates <- rownames(res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

full_results_siblings_father_allcov <- full_results
save(full_results_siblings_father_allcov, file = "output/full_results_siblings_father_allcov_230112.rda")


## 3.5 Get descriptive in table #######

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    covariances <- as.data.frame(results[[10]])
    covariances$schoolsub <- school_subject[i]
    covariances$trait <- parental_trait[j]
    full_results <- rbind(full_results, covariances)
  }
}
covariances <- full_results
save(covariances, file = "output/full_results_siblings_father_covariances_230112.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    means <- as.data.frame(results[[11]])
    means$schoolsub <- school_subject[i]
    means$trait <- parental_trait[j]
    full_results <- rbind(full_results, means)
  }
}
means <- full_results
save(means, file = "output/full_results_siblings_father_means_230112.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    descriptives <- as.data.frame(results[[8]])
    descriptives$schoolsub <- school_subject[i]
    descriptives$trait <- parental_trait[j]
    full_results <- rbind(full_results, descriptives)
  }
}

descriptives <- full_results
save(descriptives, file = "output/full_results_siblings_father_descriptives_230112.rda")

## 3.6 Adjusting for EA #######

for (trait in 1:length(items_father)){
  assign(paste0("results_NPREG05_",items_father[trait]),
         run_analyses_father_EA(school_father_moba_cousins_NPREG05, 
                                "std_score_NPREG05", 
                                items_father[trait], 
                                T))
  assign(paste0("results_NPLES05_",items_father[trait]),
         run_analyses_father_EA(school_father_moba_cousins_NPLES05, 
                                "std_score_NPLES05", 
                                items_father[trait], 
                                T))
  assign(paste0("results_NPENG05_",items_father[trait]),
         run_analyses_father_EA(school_father_moba_cousins_NPENG05, 
                                "std_score_NPENG05", 
                                items_father[trait], 
                                T))
}


### 3.6.1 Get population, within and between effects for all conditions + sample sizes #############
school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_father
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    estimates <- c("Population", "Within_effect", "Between_effect")
    modpop_estimates <- results[[12]][2,]
    modwithin_within_estimates <- results[[13]][2,]
    modwithin_between_estimates <- results[[13]][3,]
    
    res <- as.data.frame(rbind(modpop_estimates, 
                               modwithin_within_estimates, 
                               modwithin_between_estimates))
    res <- cbind(estimates, res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    res$ICC <- results[[9]] 
    res$number_of_families <- results[[4]]
    res$number_of_siblings <- mean(results[[5]]$Freq) # this is table
    res$loss_duplicated <- results[[6]]  
    res$loss_missingbirthyear <- results[[7]]
    full_results <- rbind(full_results, res)
  }
}
head(full_results)
full_results_siblings_father <- full_results
#save
save(full_results_siblings_father, file = "output/full_results_siblings_father_EA_230112.rda")

### 3.6.2 Get all covariates estimates ##################################################
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    modpop_estimates <- as.data.frame(results[[12]])
    modwithin_estimates <- as.data.frame(results[[13]])
    modpop_estimates$model <- "population"
    modwithin_estimates$model <- "siblings"
    res <- rbind(modpop_estimates, modwithin_estimates)
    res$estimates <- rownames(res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

full_results_siblings_father_allcov <- full_results
save(full_results_siblings_father_allcov, file = "output/full_results_siblings_father_allcov_EA_230112.rda")


### 3.6.3  Get descriptive in table ######


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    covariances <- as.data.frame(results[[10]])
    covariances$schoolsub <- school_subject[i]
    covariances$trait <- parental_trait[j]
    full_results <- rbind(full_results, covariances)
  }
}
covariances <- full_results
save(covariances, file = "output/full_results_siblings_father_covariances_EA_230112.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    means <- as.data.frame(results[[11]])
    means$schoolsub <- school_subject[i]
    means$trait <- parental_trait[j]
    full_results <- rbind(full_results, means)
  }
}
means <- full_results
save(means, file = "output/full_results_siblings_father_means_EA_230112.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_", school_subject[i],"_", parental_trait[j]))
    descriptives <- as.data.frame(results[[8]])
    descriptives$schoolsub <- school_subject[i]
    descriptives$trait <- parental_trait[j]
    full_results <- rbind(full_results, descriptives)
  }
}

descriptives <- full_results
save(descriptives, file = "output/full_results_siblings_father_descriptives_EA_230112.rda")




# 4. Simple regression in the bigger Moba sample (not only cousins) ######
## 4.1  All gender #####
load("data/school_moba_kidpreg_total_211208.rda") #pregnancy relevant child, all possible

items_parents <- c("SCL5_Q1", "ADHD", "SCL8", "AUDIT")
school_total_moba_kidpreg_NPREG05 <-school_moba_kidpreg_total %>% 
  drop_na(std_score_NPREG05) #177519
school_total_moba_kidpreg_NPLES05 <-school_moba_kidpreg_total %>% 
  drop_na(std_score_NPLES05) #175416
school_total_moba_kidpreg_NPENG05 <-school_moba_kidpreg_total %>%
  drop_na(std_score_NPENG05) #176099

for (trait in 1:length(items_parents)){
  assign(paste0("results_fullMoba_NPREG05_",items_parents[trait]),
         run_analyses_fullMoba(school_total_moba_kidpreg_NPREG05, 
                                     "std_score_NPREG05", 
                                     items_parents[trait], 
                                     onekid = T))
  assign(paste0("results_fullMoba_NPLES05_",items_parents[trait]),
         run_analyses_fullMoba(school_total_moba_kidpreg_NPLES05, 
                                     "std_score_NPLES05", 
                                     items_parents[trait], 
                                     onekid = T))
  assign(paste0("results_fullMoba_NPENG05_",items_parents[trait]),
         run_analyses_fullMoba(school_total_moba_kidpreg_NPENG05, 
                                     "std_score_NPENG05", 
                                     items_parents[trait], 
                                     onekid=T))
}

school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_parents
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_", school_subject[i],"_", parental_trait[j]))
    res.colnames <- c()
    res <- data.frame(as.list(results[[7]][2,]))
    res$estimates <- "Population_fullMoba"
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

summary(full_results)
head(full_results)

full_results_fullMoba_total <- full_results

save(full_results_fullMoba_total, file = "output/full_results_fullMoba_total_230112.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_", school_subject[i],"_", parental_trait[j]))
    res <- as.data.frame(results[[7]])
    res$estimates <- rownames(res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

full_results_siblings_fullMoba_allcov <- full_results
save(full_results_siblings_fullMoba_allcov, file = "output/full_results_siblings_fullMoba_allcov_230112.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_", school_subject[i],"_", parental_trait[j]))
    covariances <- as.data.frame(results[[5]])
    covariances$schoolsub <- school_subject[i]
    covariances$trait <- parental_trait[j]
    full_results <- rbind(full_results, covariances)
  }
}
covariances <- full_results
save(covariances, file = "output/full_results_siblings_FullMoba__covariances_230112.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_", school_subject[i],"_", parental_trait[j]))
    means <- as.data.frame(results[[6]])
    means$schoolsub <- school_subject[i]
    means$trait <- parental_trait[j]
    full_results <- rbind(full_results, means)
  }
}
means <- full_results
save(means, file = "output/full_results_siblings_fullMoba_means_EA_230112.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_", school_subject[i],"_", parental_trait[j]))
    descriptives <- as.data.frame(results[[4]])
    descriptives$schoolsub <- school_subject[i]
    descriptives$trait <- parental_trait[j]
    full_results <- rbind(full_results, descriptives)
  }
}

descriptives <- full_results
save(descriptives, file = "output/full_results_siblings_fullMoba_descriptives_230112.rda")



# 
## 4.2 In mothers - Eating disorders ######
load("data/school_mother_moba_kidpreg_211124.rda")#pregnancy relevant child

items_parents <- c("scale_score_items_m_ED_NRM_Q1", 
                   "scale_score_items_m_ED_NRM_Q8")
school_mother_moba_kidpreg_NPREG05 <-school_mother_moba_kidpreg %>% 
  drop_na(std_score_NPREG05) 
school_mother_moba_kidpreg_NPLES05 <-school_mother_moba_kidpreg %>% 
  drop_na(std_score_NPLES05) 
school_mother_moba_kidpreg_NPENG05 <-school_mother_moba_kidpreg %>%
  drop_na(std_score_NPENG05) 

for (trait in 1:length(items_parents)){
  assign(paste0("results_fullMoba_mother_NPREG05_",items_parents[trait]),
         run_analyses_fullMoba_mother(school_mother_moba_kidpreg_NPREG05, 
                               "std_score_NPREG05", 
                               items_parents[trait], 
                               onekid = T))
  assign(paste0("results_fullMoba_mother_NPLES05_",items_parents[trait]),
         run_analyses_fullMoba_mother(school_mother_moba_kidpreg_NPLES05, 
                               "std_score_NPLES05", 
                               items_parents[trait], 
                               onekid = T))
  assign(paste0("results_fullMoba_mother_NPENG05_",items_parents[trait]),
         run_analyses_fullMoba_mother(school_mother_moba_kidpreg_NPENG05, 
                               "std_score_NPENG05", 
                               items_parents[trait], 
                               onekid=T))
}

school_subject <- c("NPREG05", "NPLES05", "NPENG05")
parental_trait <- items_parents
full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_mother_", school_subject[i],"_", parental_trait[j]))
    res.colnames <- c()
    res <- data.frame(as.list(results[[7]][2,]))
    res$estimates <- "Population_fullMoba"
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

summary(full_results)
head(full_results)

full_results_fullMoba_total <- full_results

save(full_results_fullMoba_total, file = "output/full_results_fullMoba_mother_230112.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for ( j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_mother_", school_subject[i],"_", parental_trait[j]))
    res <- as.data.frame(results[[7]])
    res$estimates <- rownames(res)
    rownames(res)<-NULL
    res$school_subject <- school_subject[i]
    res$parental_trait <- parental_trait[j]
    res$sample_size <- results[[3]]
    full_results <- rbind(full_results, res)
  }
}

full_results_siblings_fullMoba_allcov <- full_results
save(full_results_siblings_fullMoba_allcov, file = "output/full_results_siblings_fullMoba_mother_allcov_230112.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_mother_", school_subject[i],"_", parental_trait[j]))
    covariances <- as.data.frame(results[[5]])
    covariances$schoolsub <- school_subject[i]
    covariances$trait <- parental_trait[j]
    full_results <- rbind(full_results, covariances)
  }
}
covariances <- full_results
save(covariances, file = "output/full_results_siblings_fullMoba_mother_covariances_230112.rda")

full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_mother_", school_subject[i],"_", parental_trait[j]))
    means <- as.data.frame(results[[6]])
    means$schoolsub <- school_subject[i]
    means$trait <- parental_trait[j]
    full_results <- rbind(full_results, means)
  }
}
means <- full_results
save(means, file = "output/full_results_siblings_fullMoba_mother_means_230112.rda")


full_results <- NULL
for (i in 1:length(school_subject)){
  for (j in 1:length(parental_trait)){
    results <- get(paste0("results_fullMoba_mother_", school_subject[i],"_", parental_trait[j]))
    descriptives <- as.data.frame(results[[4]])
    descriptives$schoolsub <- school_subject[i]
    descriptives$trait <- parental_trait[j]
    full_results <- rbind(full_results, descriptives)
  }
}

descriptives <- full_results
save(descriptives, file = "output/full_results_siblings_fullMoba_mother_descriptives_230112.rda")

