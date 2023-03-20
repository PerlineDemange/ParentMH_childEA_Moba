# Perline Demange 
# Get standardized national scores in 5th grade 
# in all the population available 
# Start date: 20/10/2021

#1.  Libraries and set up ######
library(tidyverse)
data <- data.table::fread("../../../data/registers/original/csv/w19_0634_nasjonale_prover_ut.csv", strip.white=TRUE)
head(data) # there are 5 346 708 rows

#keep relevant info and rename columns
data <- data %>% 
  select(w19_0634_lnr, year = AARGANG, status = DELTATTSTATUS, test = PROVE, levelscore=MESTRINGSNIVAA, score = POENG)

# 2. Selection #####
#Remove tests which status is not attended, D = attended the test
data <- data %>% 
  filter(status == "D") #5 131 280

# Select only Grade 5th tests 
# Select only norwegian tests (drop sami)
table(data$PROVE) #dropping sami means I only drop 1 NPLSA05, NPSSA05 and 59 NPNSA

testname <- c("NPENG05",  "NPLES05", "NPREG05") #tests in norwegian in 5th grade
grade5th <- data %>% 
  filter(test %in% testname) #1 989 833

table(grade5th$test)

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


# Check results by plotting 
ggplot(results, aes(x = score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)

ggplot(results, aes(x = std_score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)

hist(results$score)
table(results$levelscore) # no zero so everybody took the test


#4. Identify and remove duplicates: students with 2 grades for the same year ###########
#values_ln=length give the number of duplicates when trying to reformat to wide format, duplicates based on same ID, year and status
duplicate <- pivot_wider(results, names_from = test, values_from = c(levelscore, score, mean, sd, std_score), values_fn=length) 

duplicatedstudents <- duplicate[which(duplicate$levelscore_NPENG05 == 2), ]$w19_0634_lnr # list of ids who are duplicated
# there are 2 students with duplicates grades 

results[which(results$w19_0634_lnr %in% duplicatedstudents), ]
data <- data.table::fread("../../../data/registers/original/csv/w19_0634_nasjonale_prover_ut.csv", strip.white=TRUE)
data[which(data$PROVE %in% c("NPENG05", "NPLES05", "NPREG05") & data$w19_0634_lnr %in% duplicatedstudents), ]
# They took the same tests in two different schools 
# It is difficult to tell which data is more recent (no clear improvement in grade) 
# I drop both students from the analyses 

results_woduplicated <- results[!which(results$w19_0634_lnr %in% duplicatedstudents), ]
head(results_woduplicated)

#5. Reformat to wide format ######
# one individual per row
wide <- pivot_wider(results_woduplicated, names_from = test, values_from = c(levelscore, score, mean, sd, std_score))
head(wide) 
# 700 367 
#save(wide, file = "data/school_performance_211025.rda")
load("data/school_performance_211025.rda")

#6. Identify and adjust for students who took the test at least twice #######
retakestudents <- wide[duplicated(wide$w19_0634_lnr),]$w19_0634_lnr # duplicate function only give the second row of the duplicated element
# there are 120 students who did retakes
retake <- wide[ wide$w19_0634_lnr %in% retakestudents, ] # so need to extract both rows to check what is happening 
head(retake)
# Some students have missing data in the most recent year of the test 
# retaking a school year is not something that is usually done in Norway, so might be more related to change of schools, or students with special needs
# It looks like most students dont substantially improve in the second year they take the test 
# I will keep only the first year results, as this is likely the test they took at the standard test age, and in the school they are the longest attending 

retake <- arrange(retake, w19_0634_lnr, year) # order by students, and by year of test, chronological, so first year first 
to_remove <- retake[duplicated(retake$w19_0634_lnr),] # the duplicated function select the second appearance of the value, so it select the second time the students passed the tests

final_data_educ <- anti_join(wide, to_remove, by = c("w19_0634_lnr", "year"))
# 700 247 (700 367-120 so correct)

#7. Save data ########
save(final_data_educ, file = "data/school_performance_211103.rda")

load("data/school_performance_211103.rda")

# 8. Descriptives #########
load("data/school_performance_211103.rda")
education <- final_data_educ

png("output/descriptive_math_raw_global_pop.png", width=1800)
ggplot(education, aes(x = score_NPREG05)) +
  geom_histogram(fill = "grey", colour = "black") +
  facet_grid( ~ year) + 
  theme_minimal() +
  ggtitle(" Math raw scores in global population ")
dev.off()

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



