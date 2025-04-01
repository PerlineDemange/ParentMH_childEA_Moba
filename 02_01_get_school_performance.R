# Perline Demange 
# Get standardized national scores in 5th grade 
# in all the population available 
# Start date: 20/10/2021
# Update new data 25-01-06

#1.  Libraries and set up ######
library(tidyverse)
data <- data.table::fread("N:/durable/data/registers/SSB/01_data/data_v5.0/csv/EDUCATION_NASJONALE_PROVER.csv",
                          strip.white=TRUE)
head(data) # there are 7911960 rows

#keep relevant info and rename columns
data <- data %>% 
  select(w19_0634_lnr, year = AARGANG, status = DELTATTSTATUS,
         test = PROVE, levelscore=MESTRINGSNIVAA, score = POENG)

# 2. Selection #####
#Remove tests which status is not attended, D = attended the test
data <- data %>% 
  filter(status == "D") #7499941

# Select only Grade 5th tests 
# Select only norwegian tests (drop sami)

testname <- c("NPENG05",  "NPLES05", "NPREG05") #tests in norwegian in 5th grade
grade5th <- data %>% 
  filter(test %in% testname) #2864992
table(grade5th$test)
table(grade5th$levelscore) # three "." to remove 
hist(grade5th$score)
table(is.na(grade5th$score)) #13
grade5th[is.na(grade5th$score),]
table((grade5th$score == NULL))
#remove missing 
grade5th <- grade5th %>% 
  filter(levelscore > 0)
grade5th <- grade5th %>% 
  drop_na(score)

#3. Standardize scores per year and per test #####
# Get the average and sd of score per year and per test
hist(grade5th$year)
# month is given but there is only one test per year anyway 

average <- grade5th %>%
  group_by(year, test) %>%
  summarise(
    mean = mean(score),
    sd = sd(score))

results <- left_join(grade5th, average, by=c("year", "test"))
head(results)

# Standardize the scores per year and test 
results$std_score <- (results$score - results$mean)/results$sd

# clean year column 
results$year <- substr(results$year, 1, nchar(results$year) - 2)

# Check results by plotting 
ggplot(results, aes(x = score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)

ggplot(results, aes(x = std_score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)
#distribution vary quite a bit by years

hist(results$score)
table(results$levelscore, useNA="always") # no zero so everybody took the test
table(is.na(results$year))

#4. Identify and remove duplicates: students with 2 grades for the same year ###########
#values_ln=length give the number of duplicates when trying to reformat to wide format, duplicates based on same ID, year and status
duplicate <- pivot_wider(results, names_from = test, 
                         values_from = c(levelscore, score, mean, sd, std_score), 
                         values_fn=length) 

summary(duplicate$levelscore_NPLES05) # some duplicate
summary(duplicate$levelscore_NPENG05)# some duplicate
summary(duplicate$levelscore_NPREG05) # some duplicate
duplicatedstudents <- duplicate[!is.na(duplicate$levelscore_NPENG05) & duplicate$levelscore_NPENG05 > 1 |
                                  !is.na(duplicate$levelscore_NPLES05) & duplicate$levelscore_NPLES05 > 1 | 
                                  !is.na(duplicate$levelscore_NPREG05) & duplicate$levelscore_NPREG05 > 1 ,
                                ]$w19_0634_lnr # list of ids who are duplicated
# there are 7 students with duplicates grades 

table(is.na(duplicate$mean_NPLES05))
#note to self which( == NA) gives totally wrong results, need to use is.na
duplicate[is.na(duplicate$levelscore_NPLES05),]
results_with_na <- results %>%
  filter(if_any(everything(), is.na)) #empty so there is no NAs to begin with.. 
duplicate_with_na <- duplicate %>%
  filter(if_any(everything(), is.na)) # 113725 rows wow
tail(duplicate_with_na)
results[results$w19_0634_lnr == "P000000085",] # some individuals only have one of the tests available so NA is introduced in the others
duplicate[is.na(duplicate$levelscore_NPENG05),]
duplicate[is.na(duplicate$levelscore_NPREG05),]


results[(results$w19_0634_lnr %in% duplicatedstudents), ]
data <- data.table::fread("N:/durable/data/registers/SSB/01_data/data_v5.0/csv/EDUCATION_NASJONALE_PROVER.csv",
                          strip.white=TRUE)
data[(data$PROVE %in% c("NPENG05", "NPLES05", "NPREG05") & 
        data$w19_0634_lnr %in% duplicatedstudents), ]
# 3 took the same tests in two different schools 
# for those It is difficult to tell which data is more recent (no clear improvement in grade) 
# I drop these 6 students from the analyses 

results_woduplicated <- results[!which(results$w19_0634_lnr %in% duplicatedstudents), ]
head(results_woduplicated)

#5. Reformat to wide format ######
# one individual per row
# warning, if some duplicated are left, this will create lists
results_woduplicated$levelscore <- as.numeric(results_woduplicated$levelscore)
wide <- pivot_wider(results_woduplicated,
                    names_from = test,
                    values_from = c(levelscore, score, mean, sd, std_score))
head(wide) 
wide <- as.data.frame(wide)
# 999 973
save(wide, file = "data/school_performance_250106.rda")


#6. Identify and adjust for students who took the test at least twice (different years)#######
retakestudents <- wide[duplicated(wide$w19_0634_lnr),]$w19_0634_lnr # duplicate function only give the second row of the duplicated element
# there are 161 students who did retakes
retake <- wide[ wide$w19_0634_lnr %in% retakestudents, ] # so need to extract both rows to check what is happening 
retake
# Some students have missing data in the most recent year of the test 
# retaking a school year is not something that is usually done in Norway, so might be more related to change of schools, or students with special needs
# It looks like most students dont substantially improve in the second year they take the test, but some have some missing data for the first year
# I will keep only the first year results, as this is likely the test they took at the standard test age, and in the school they are the longest attending 

retake <- arrange(retake, w19_0634_lnr, year) # order by students, and by year of test, chronological, so first year first 
to_remove <- retake[duplicated(retake$w19_0634_lnr),] # the duplicated function select the second appearance of the value, so it select the second time the students passed the tests

final_data_educ <- anti_join(wide, to_remove, by = c("w19_0634_lnr", "year"))
# 999812

#7. Save data ########
save(final_data_educ, file = "data/school_performance_250106.rda")

# 8. Descriptives #########
#load("data/school_performance_211103.rda")
education <- final_data_educ

png("output/descriptive_math_raw_global_pop.png", height=2000)
ggplot(education, aes(x = score_NPREG05)) +
  geom_histogram(fill = "grey", colour = "black", binwidth = 2) +
  facet_wrap(~year, ncol=1)+ 
  theme_minimal() +
  ggtitle(" Math raw scores in global population ")
dev.off()

ggplot(results, aes(x = score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(~year )

png("output/descriptive_math_std_global_pop.png", width=1800)
ggplot(education, aes(x = std_score_NPREG05)) +
  geom_histogram(fill = "grey", colour = "black") +
  facet_grid( ~ year) + 
  theme_minimal() +
  ggtitle(" Math scores std per year in global population ")
dev.off()

png("output/descriptive_english_raw_global_pop.png", width=1800)
ggplot(education, aes(x = score_NPENG05)) +
  geom_histogram(fill = "grey", colour = "black") +
  facet_grid( ~ year)+ 
  theme_minimal() +
  ggtitle(" English raw scores in global population ")
dev.off()

png("output/descriptive_english_std_global_pop.png", width=1800)
ggplot(education, aes(x = std_score_NPENG05)) +
  geom_histogram(fill = "grey", colour = "black") +
  facet_grid( ~ year)+ 
  theme_minimal() +
  ggtitle(" English scores std per year in global population ")
dev.off()

png("output/descriptive_reading_raw_global_pop.png", width=1800)
ggplot(education, aes(x = score_NPLES05)) +
  geom_histogram(fill = "grey", colour = "black") +
  facet_grid( ~ year)+ 
  theme_minimal() +
  ggtitle(" Reading raw scores in global population ")
dev.off()

png("output/descriptive_reading_std_global_pop.png", width=1800)
ggplot(education, aes(x = std_score_NPLES05)) +
  geom_histogram(fill = "grey", colour = "black") +
  facet_grid( ~ year)+ 
  theme_minimal() +
  ggtitle(" Reading scores std per year in global population ")
dev.off()



