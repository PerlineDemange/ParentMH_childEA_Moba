# Functions
# Perline Demange

# Get the intraclass correlation for the variable defined in the model
ICCest <- function(model) {
  # Get the intraclass correlation for the variable defined in the model
  icc <- sqrt(diag(getVarCov(model)))^2 / (sqrt(diag(getVarCov(model)))^2 + model$sigma^2 )
  as.vector(icc)
}

#

# data <- school_moba_cousins_total_NPENG05
# data$parent_outcome <- data$SCL8
# data$school_perf <- data$std_score_NPENG05
# 
# onekid <- T

run_analyses_cross <- function(data, school_performance, trait, onekid) {
  
  data$parent_outcome <- data[[trait]]
  data$school_perf <- data[[school_performance]]
  
  # Remove missing data 
  data <- data %>% drop_na(parent_outcome) 
  
  # Remove families without 2 sisters 
  data <- data %>%
    group_by(shared_grandparents_id) %>%
    filter(length(unique(sib_parent_id))>1)
  
  # If more than one kid per mother, randomly remove one 
  if (onekid == T){
    data <- data %>%
      group_by(sib_parent_id) %>%
      slice_head(n = 1) %>%
      ungroup() 
  }
  
  # Remove duplicated children, part of two families
  sample_size_beforedup <- nrow(data)
  duplicates <- sum(duplicated(data$w19_0634_lnr)) 
  data <- data[!duplicated(data$w19_0634_lnr),]
  data <- data %>%
    group_by(shared_grandparents_id) %>%
    filter(length(unique(sib_parent_id))>1)
  
  sample_size_afterdup <- nrow(data)
  
  # Remove individual with missing birthyear for father or mother and get estimate of how much that is 
  # Ideally I can find all missing data in additional Moba surveys
  
  data <- data[!is.na(data$father_birthyear),]
  data <- data[!is.na(data$mother_birthyear),]
  data <- data %>%
    group_by(shared_grandparents_id) %>%
    filter(length(unique(sib_parent_id))>1)
  
  # Get sample sizes 
  sample_size <- nrow(data)
  
  number_of_siblings <- table(data$shared_grandparents_id)
  number_of_siblings <- as.data.frame(t(number_of_siblings))
  number_of_families <- as.data.frame(table(number_of_siblings$Freq))
  number_of_families <- sum(number_of_families$Freq)
  loss_duplicated <- sample_size_beforedup - sample_size_afterdup
  loss_missingbirthyear <- sample_size_afterdup - sample_size
  
  # Data transformation
  data$sex <- as.numeric(data$sex)
  data <- data %>% mutate(sib_parent = recode(sib_parent,
                                              "mother"=2,
                                              "father"=1))
  
  # Descriptives
  # mean, sd, skew and covariances 
  
  descriptives <- as.data.frame(describe(cbind(data$parent_outcome, data$school_perf)))
  descriptives$vars <- c(trait, school_performance)
  
  covariances <- cov(cbind(data$school_perf, data$parent_outcome, 
                           data$birth_yr, data$father_birthyear, data$mother_birthyear, 
                           data$sex, data$sib_parent))
  rowname <- c("school_perf", "parent_outcome", 
               "birth_yr", 
               "father_birthyear", "mother_birthyear", 
               "sex", "sib_parent")
  covariances <- cbind(as.data.frame(covariances), rowname)
  means <- colMeans(cbind(data$school_perf, data$parent_outcome, 
                     data$birth_yr, data$father_birthyear, data$mother_birthyear, 
                     data$sex, data$sib_parent))
  means <-cbind(as.data.frame(means), rowname)
  
  # ICC
  m0 <- lme(parent_outcome ~ 1, 
            random=~1|shared_grandparents_id, 
            method="ML", 
            na.action=na.omit,
            data=data)
  ICC <- ICCest(m0)
  

  # STD the trait
  data$parent_outcome <- as.numeric(scale(data$parent_outcome))
  # careful when using mutate_at, this gives a wrong result: data %>% mutate_at( "scale_score_items_m_ADHD_Q3", scale)
  

  # plot the data 
  htop <- ggplot(data=data, aes(x=school_perf)) + 
    geom_histogram(aes(y=..density..), fill = "white", 
                   color = "black", binwidth = 1) + 
    #stat_density(colour = "blue", geom="line", size = 1.5, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous(breaks = 40) + 
    scale_y_continuous("Count") + 
    theme_bw() + theme(axis.title.x = element_blank())
  
  blank <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), panel.background=element_blank(), 
          panel.grid=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hright <- ggplot(data=data, aes(x=parent_outcome)) + 
    geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
    #stat_density(colour = "red", geom="line", size = 1, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
    scale_y_continuous("Count") + 
    coord_flip() + theme_bw() + 
    theme(axis.title.y = element_blank())

  quantile_plot <-
    ggplot(data= data,aes( y=parent_outcome ,x= school_perf)) +
    geom_quantile(method = "rqss", lambda = 0.4) +
    theme_light()+
    xlab(school_performance)+
    ylab(trait)
  
  
  figure <- arrangeGrob(htop, blank, quantile_plot, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  ggsave(file=paste0(school_performance,"_",trait,".png"), figure)

  # Create within and between variables 
  mean_pheno<-group_by(data,shared_grandparents_id) %>% 
    summarize(m=mean(parent_outcome))
  colnames(mean_pheno) <- c("shared_grandparents_id", "between_pheno")
  
  finalsib<-merge(data,mean_pheno,by="shared_grandparents_id")
  finalsib$within_pheno <- finalsib$parent_outcome - finalsib$between_pheno 
  finalsib
  
  # Run observational analysis 
  modpop<-lme(school_perf ~ parent_outcome + 
                birth_yr + father_birthyear + mother_birthyear +
                sex + sib_parent,
              random=~1|shared_grandparents_id,
              na.action=na.omit,
              data=finalsib)
  modpop_coef <- summary(modpop)$tTable
  
  #Run within-sibling analysis 
  modwithin<-lme(school_perf ~ within_pheno + between_pheno +
                   birth_yr + father_birthyear + mother_birthyear +
                   sex + sib_parent,
                 random=~1|shared_grandparents_id,
                 na.action=na.omit,
                 data=finalsib)
  modwithin_coef <- summary(modwithin)$tTable
  
  # Save all results 
  results <- list(trait, school_performance, # variables
                  sample_size, number_of_families, number_of_siblings, loss_duplicated, loss_missingbirthyear,  # sample sizes 
                  descriptives, ICC, covariances, means, #descriptives
                  modpop_coef, modwithin_coef) # regressions results 
  results
} 

run_analyses_cross_EA <- function(data, school_performance, trait, onekid) {
  
  data$parent_outcome <- data[[trait]]
  data$school_perf <- data[[school_performance]]
  
  # Remove missing data 
  data <- data %>% drop_na(parent_outcome) 
  
  # Remove families without 2 sisters 
  data <- data %>%
    group_by(shared_grandparents_id) %>%
    filter(length(unique(sib_parent_id))>1)
  
  # If more than one kid per mother, randomly remove one 
  if (onekid == T){
    data <- data %>%
      group_by(sib_parent_id) %>%
      slice_head(n = 1) %>%
      ungroup() 
  }
  
  # Remove duplicated children, part of two families
  sample_size_beforedup <- nrow(data)
  duplicates <- sum(duplicated(data$w19_0634_lnr)) 
  data <- data[!duplicated(data$w19_0634_lnr),]
  data <- data %>%
    group_by(shared_grandparents_id) %>%
    filter(length(unique(sib_parent_id))>1)
  
  sample_size_afterdup <- nrow(data)
  
  # Remove individual with missing birthyear for father or mother and get estimate of how much that is 
  # Ideally I cna find all missing data in additional MOba surveys
  #Here I also check the missing of the education # it shoudl not be missing 
  
  data <- data[!is.na(data$father_birthyear),]
  data <- data[!is.na(data$mother_birthyear),]
  data <- data[!is.na(data$mother_EduYears11_2018),]
  data <- data[!is.na(data$father_EduYears11_2018),]
  data <- data %>%
    group_by(shared_grandparents_id) %>%
    filter(length(unique(sib_parent_id))>1)
  
  # Get sample sizes 
  sample_size <- nrow(data)
  
  number_of_siblings <- table(data$shared_grandparents_id)
  number_of_siblings <- as.data.frame(t(number_of_siblings))
  number_of_families <- as.data.frame(table(number_of_siblings$Freq))
  number_of_families <- sum(number_of_families$Freq)
  loss_duplicated <- sample_size_beforedup - sample_size_afterdup
  loss_missingbirthyear <- sample_size_afterdup - sample_size
  
  # Data transformation
  data$sex <- as.numeric(data$sex)
  data <- data %>% mutate(sib_parent = recode(sib_parent,
                                              "mother"=2,
                                              "father"=1))
  
  # Descriptives
  # mean, sd, skew and covariances 
  
  descriptives <- as.data.frame(describe(cbind(data$parent_outcome, data$school_perf)))
  descriptives$vars <- c(trait, school_performance)
  
  covariances <- cov(cbind(data$school_perf, data$parent_outcome, 
                           data$birth_yr, data$father_birthyear, data$mother_birthyear, 
                           data$sex, data$sib_parent, 
                           data$mother_EduYears11_2018, data$father_EduYears11_2018))
  rowname <- c("school_perf", "parent_outcome", 
               "birth_yr", 
               "father_birthyear", "mother_birthyear", 
               "sex", "sib_parent", 
               "mother_EA", "father_EA")
  covariances <- cbind(as.data.frame(covariances), rowname)
  means <- colMeans(cbind(data$school_perf, data$parent_outcome, 
                          data$birth_yr, data$father_birthyear, data$mother_birthyear, 
                          data$sex, data$sib_parent, 
                          data$mother_EduYears11_2018, data$father_EduYears11_2018))
  means <- cbind(as.data.frame(means), rowname)
  
  # ICC
  m0 <- lme(parent_outcome ~ 1, 
            random=~1|shared_grandparents_id, 
            method="ML", 
            na.action=na.omit,
            data=data)
  ICC <- ICCest(m0)
  
  
  # STD the trait
  data$parent_outcome <- as.numeric(scale(data$parent_outcome))
  # careful when using mutate_at, this gives a wrong result: data %>% mutate_at( "scale_score_items_m_ADHD_Q3", scale)
  
  
  # plot the data 
  htop <- ggplot(data=data, aes(x=school_perf)) + 
    geom_histogram(aes(y=..density..), fill = "white", 
                   color = "black", binwidth = 1) + 
    #stat_density(colour = "blue", geom="line", size = 1.5, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous(breaks = 40) + 
    scale_y_continuous("Count") + 
    theme_bw() + theme(axis.title.x = element_blank())
  
  blank <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), panel.background=element_blank(), 
          panel.grid=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hright <- ggplot(data=data, aes(x=parent_outcome)) + 
    geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
    #stat_density(colour = "red", geom="line", size = 1, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
    scale_y_continuous("Count") + 
    coord_flip() + theme_bw() + 
    theme(axis.title.y = element_blank())
  
  quantile_plot <-
    ggplot(data= data,aes( y=parent_outcome ,x= school_perf)) +
    geom_quantile(method = "rqss", lambda = 0.4) +
    theme_light()+
    xlab(school_performance)+
    ylab(trait)
  
  figure <- arrangeGrob(htop, blank, quantile_plot, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  ggsave(file=paste0(school_performance,"_",trait,"_EA.png"), figure)
  
  # Create within and between variables 
  mean_pheno<-group_by(data,shared_grandparents_id) %>% 
    summarize(m=mean(parent_outcome))
  colnames(mean_pheno) <- c("shared_grandparents_id", "between_pheno")
  
  finalsib<-merge(data,mean_pheno,by="shared_grandparents_id")
  finalsib$within_pheno <- finalsib$parent_outcome - finalsib$between_pheno 
  finalsib
  
  # Run observational analysis 
  modpop<-lme(school_perf ~ parent_outcome + 
                birth_yr + father_birthyear + mother_birthyear +
                sex + sib_parent +
                mother_EduYears11_2018 + father_EduYears11_2018,
              random=~1|shared_grandparents_id,
              na.action=na.omit,
              data=finalsib)
  modpop_coef <- summary(modpop)$tTable
  
  #Run within-sibling analysis 
  modwithin<-lme(school_perf ~ within_pheno + between_pheno +
                   birth_yr + father_birthyear + mother_birthyear +
                   sex + sib_parent +
                   mother_EduYears11_2018 + father_EduYears11_2018,
                 random=~1|shared_grandparents_id,
                 na.action=na.omit,
                 data=finalsib)
  modwithin_coef <- summary(modwithin)$tTable
  
  # Save all results 
  results <- list(trait, school_performance, # variables
                  sample_size, number_of_families, number_of_siblings, loss_duplicated, loss_missingbirthyear,  # sample sizes 
                  descriptives, ICC, covariances, means, #descriptives
                  modpop_coef, modwithin_coef) # regressions results 
  results
} 


run_analyses_mother <- function(data, school_performance, trait, onekid) {
  
  data$parent_outcome <- data[[trait]]
  data$school_perf <- data[[school_performance]]
  
  # Remove missing data 
  data <- data %>% drop_na(parent_outcome) 
  
  # Remove families without 2 sisters 
  data <- data %>%
    group_by(maternal_grandparents) %>%
    filter(length(unique(mother_lnr))>1)
  
  # If more than one kid per mother, randomly remove one 
  if (onekid == T){
    data <- data %>%
      group_by(mother_lnr) %>%
      slice_head(n = 1) %>%
      ungroup() 
  }
  
  # Remove duplicated children, part of two families
  sample_size_beforedup <- nrow(data)
  duplicates <- sum(duplicated(data$w19_0634_lnr)) 
  data <- data[!duplicated(data$w19_0634_lnr),]
  data <- data %>%
    group_by(maternal_grandparents) %>%
    filter(length(unique(mother_lnr))>1)
  
  sample_size_afterdup <- nrow(data)
  
  # Remove individual with missing birthyear for father or mother and get estimate of how much that is 
  # Ideally I cna find all missing data in additional MOba surveys
  
  data <- data[!is.na(data$mother_birthyear),]
  data <- data %>%
    group_by(maternal_grandparents) %>%
    filter(length(unique(mother_lnr))>1)
  
  # Get sample sizes 
  sample_size <- nrow(data)
  
  number_of_siblings <- table(data$maternal_grandparents)
  number_of_siblings <- as.data.frame(t(number_of_siblings))
  number_of_families <- as.data.frame(table(number_of_siblings$Freq))
  number_of_families <- sum(number_of_families$Freq)
  loss_duplicated <- sample_size_beforedup - sample_size_afterdup
  loss_missingbirthyear <- sample_size_afterdup - sample_size
  
  # Data transformation
  data$sex <- as.numeric(data$sex)

  # Descriptives
  # mean, sd, skew and covariances 
  
  descriptives <- as.data.frame(describe(cbind(data$parent_outcome, data$school_perf)))
  descriptives$vars <- c(trait, school_performance)
  
  covariances <- cov(cbind(data$school_perf, data$parent_outcome, 
                           data$birth_yr,  data$mother_birthyear, 
                           data$sex))
  rowname <- c("school_perf", "parent_outcome", 
               "birth_yr", "mother_birthyear", 
               "sex")
  covariances <- cbind(as.data.frame(covariances), rowname)
  means <- colMeans(cbind(data$school_perf, data$parent_outcome, 
                          data$birth_yr,  data$mother_birthyear, 
                          data$sex))
  means <- cbind(as.data.frame(means), rowname)
  
  # ICC
  m0 <- lme(parent_outcome ~ 1, 
            random=~1|maternal_grandparents, 
            method="ML", 
            na.action=na.omit,
            data=data)
  ICC <- ICCest(m0)
  
  
  # STD the trait
  data$parent_outcome <- as.numeric(scale(data$parent_outcome))
  # careful when using mutate_at, this gives a wrong result: data %>% mutate_at( "scale_score_items_m_ADHD_Q3", scale)
  
  
  # plot the data 
  htop <- ggplot(data=data, aes(x=school_perf)) + 
    geom_histogram(aes(y=..density..), fill = "white", 
                   color = "black", binwidth = 1) + 
    #stat_density(colour = "blue", geom="line", size = 1.5, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous(breaks = 40) + 
    scale_y_continuous("Count") + 
    theme_bw() + theme(axis.title.x = element_blank())
  
  blank <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), panel.background=element_blank(), 
          panel.grid=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hright <- ggplot(data=data, aes(x=parent_outcome)) + 
    geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
    #stat_density(colour = "red", geom="line", size = 1, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
    scale_y_continuous("Count") + 
    coord_flip() + theme_bw() + 
    theme(axis.title.y = element_blank())
  
  quantile_plot <-
    ggplot(data= data,aes( y=parent_outcome ,x= school_perf)) +
    geom_quantile(method = "rqss", lambda = 0.4) +
    theme_light()+
    xlab(school_performance)+
    ylab(trait)
  
  figure <- arrangeGrob(htop, blank, quantile_plot, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  ggsave(file=paste0(school_performance,"_",trait,"_mother.png"), figure)
  
  # Create within and between variables 
  mean_pheno<-group_by(data,maternal_grandparents) %>% 
    summarize(m=mean(parent_outcome))
  colnames(mean_pheno) <- c("maternal_grandparents", "between_pheno")
  
  finalsib<-merge(data,mean_pheno,by="maternal_grandparents")
  finalsib$within_pheno <- finalsib$parent_outcome - finalsib$between_pheno 
  finalsib
  
  # Run observational analysis 
  modpop<-lme(school_perf ~ parent_outcome + 
                birth_yr + mother_birthyear +
                sex ,
              random=~1|maternal_grandparents,
              na.action=na.omit,
              data=finalsib)
  modpop_coef <- summary(modpop)$tTable
  
  #Run within-sibling analysis 
  modwithin<-lme(school_perf ~ within_pheno + between_pheno +
                   birth_yr +  mother_birthyear +
                   sex ,
                 random=~1|maternal_grandparents,
                 na.action=na.omit,
                 data=finalsib)
  modwithin_coef <- summary(modwithin)$tTable
  
  # Save all results 
  results <- list(trait, school_performance, # variables
                  sample_size, number_of_families, number_of_siblings, loss_duplicated, loss_missingbirthyear,  # sample sizes 
                  descriptives, ICC, covariances, means, #descriptives
                  modpop_coef, modwithin_coef) # regressions results 
  results
} 

run_analyses_mother_EA <- function(data, school_performance, trait, onekid) {
  
  data$parent_outcome <- data[[trait]]
  data$school_perf <- data[[school_performance]]
  
  # Remove missing data 
  data <- data %>% drop_na(parent_outcome) 
  
  # Remove families without 2 sisters 
  data <- data %>%
    group_by(maternal_grandparents) %>%
    filter(length(unique(mother_lnr))>1)
  
  # If more than one kid per mother, randomly remove one 
  if (onekid == T){
    data <- data %>%
      group_by(mother_lnr) %>%
      slice_head(n = 1) %>%
      ungroup() 
  }
  
  # Remove duplicated children, part of two families
  sample_size_beforedup <- nrow(data)
  duplicates <- sum(duplicated(data$w19_0634_lnr)) 
  data <- data[!duplicated(data$w19_0634_lnr),]
  data <- data %>%
    group_by(maternal_grandparents) %>%
    filter(length(unique(mother_lnr))>1)
  
  sample_size_afterdup <- nrow(data)
  
  # Remove individual with missing birthyear for father or mother and get estimate of how much that is 
  # Ideally I cna find all missing data in additional MOba surveys
  
  data <- data[!is.na(data$mother_birthyear),]
  data <- data[!is.na(data$mother_EduYears11_2018),]
  data <- data %>%
    group_by(maternal_grandparents) %>%
    filter(length(unique(mother_lnr))>1)
  
  # Get sample sizes 
  sample_size <- nrow(data)
  
  number_of_siblings <- table(data$maternal_grandparents)
  number_of_siblings <- as.data.frame(t(number_of_siblings))
  number_of_families <- as.data.frame(table(number_of_siblings$Freq))
  number_of_families <- sum(number_of_families$Freq)
  loss_duplicated <- sample_size_beforedup - sample_size_afterdup
  loss_missingbirthyear <- sample_size_afterdup - sample_size
  
  # Data transformation
  data$sex <- as.numeric(data$sex)
  
  # Descriptives
  # mean, sd, skew and covariances 
  
  descriptives <- as.data.frame(describe(cbind(data$parent_outcome, data$school_perf)))
  descriptives$vars <- c(trait, school_performance)
  
  covariances <- cov(cbind(data$school_perf, data$parent_outcome, 
                           data$birth_yr,  data$mother_birthyear, 
                           data$sex, 
                           data$mother_EduYears11_2018))
  rowname <- c("school_perf", "parent_outcome", 
               "birth_yr", 
               "mother_birthyear", 
               "sex", 
               "mother_EA")
  means <- colMeans(cbind(data$school_perf, data$parent_outcome, 
                          data$birth_yr,  data$mother_birthyear, 
                          data$sex, 
                          data$mother_EduYears11_2018))  
  means <- cbind(as.data.frame(means), rowname)
  
  
  # ICC
  m0 <- lme(parent_outcome ~ 1, 
            random=~1|maternal_grandparents, 
            method="ML", 
            na.action=na.omit,
            data=data)
  ICC <- ICCest(m0)
  
  
  # STD the trait
  data$parent_outcome <- as.numeric(scale(data$parent_outcome))
  # careful when using mutate_at, this gives a wrong result: data %>% mutate_at( "scale_score_items_m_ADHD_Q3", scale)
  
  
  # plot the data 
  htop <- ggplot(data=data, aes(x=school_perf)) + 
    geom_histogram(aes(y=..density..), fill = "white", 
                   color = "black", binwidth = 1) + 
    #stat_density(colour = "blue", geom="line", size = 1.5, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous(breaks = 40) + 
    scale_y_continuous("Count") + 
    theme_bw() + theme(axis.title.x = element_blank())
  
  blank <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), panel.background=element_blank(), 
          panel.grid=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hright <- ggplot(data=data, aes(x=parent_outcome)) + 
    geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
    #stat_density(colour = "red", geom="line", size = 1, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
    scale_y_continuous("Count") + 
    coord_flip() + theme_bw() + 
    theme(axis.title.y = element_blank())
  
  quantile_plot <-
    ggplot(data= data,aes( y=parent_outcome ,x= school_perf)) +
    geom_quantile(method = "rqss", lambda = 0.4) +
    theme_light()+
    xlab(school_performance)+
    ylab(trait)
  
  figure <- arrangeGrob(htop, blank, quantile_plot, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  ggsave(file=paste0(school_performance,"_",trait,"_mother_EA.png"), figure)
  
  # Create within and between variables 
  mean_pheno<-group_by(data,maternal_grandparents) %>% 
    summarize(m=mean(parent_outcome))
  colnames(mean_pheno) <- c("maternal_grandparents", "between_pheno")
  
  finalsib<-merge(data,mean_pheno,by="maternal_grandparents")
  finalsib$within_pheno <- finalsib$parent_outcome - finalsib$between_pheno 
  finalsib
  
  # Run observational analysis 
  modpop<-lme(school_perf ~ parent_outcome + 
                birth_yr + mother_birthyear +
                sex +
                mother_EduYears11_2018,
              random=~1|maternal_grandparents,
              na.action=na.omit,
              data=finalsib)
  modpop_coef <- summary(modpop)$tTable
  
  #Run within-sibling analysis 
  modwithin<-lme(school_perf ~ within_pheno + between_pheno +
                   birth_yr +  mother_birthyear +
                   sex +
                   mother_EduYears11_2018,
                 random=~1|maternal_grandparents,
                 na.action=na.omit,
                 data=finalsib)
  modwithin_coef <- summary(modwithin)$tTable
  
  # Save all results 
  results <- list(trait, school_performance, # variables
                  sample_size, number_of_families, number_of_siblings, loss_duplicated, loss_missingbirthyear,  # sample sizes 
                  descriptives, ICC, covariances, means, #descriptives
                  modpop_coef, modwithin_coef) # regressions results 
  results
} 

# data <- school_father_moba_cousins_NPLES05
# data$parent_outcome <- data$scale_score_items_f_SCL_Q2015_full
# data$school_perf <- data$std_score_NPLES05
# 
# onekid <- T


run_analyses_father <- function(data, school_performance, trait, onekid) {
  
  data$parent_outcome <- data[[trait]]
  data$school_perf <- data[[school_performance]]
  
  # Remove missing data 
  data <- data %>% drop_na(parent_outcome) 
  
  # Remove families without 2 sisters 
  data <- data %>%
    group_by(paternal_grandparents) %>%
    filter(length(unique(father_lnr))>1)
  
  # If more than one kid per mother, randomly remove one 
  if (onekid == T){
    data <- data %>%
      group_by(father_lnr) %>%
      slice_head(n = 1) %>%
      ungroup() 
  }
  
  # Remove duplicated children, part of two families
  sample_size_beforedup <- nrow(data)
  duplicates <- sum(duplicated(data$w19_0634_lnr)) 
  data <- data[!duplicated(data$w19_0634_lnr),]
  data <- data %>%
    group_by(paternal_grandparents) %>%
    filter(length(unique(father_lnr))>1)
  
  sample_size_afterdup <- nrow(data)
  
  # Remove individual with missing birthyear for father or mother and get estimate of how much that is 
  # Ideally I cna find all missing data in additional MOba surveys
  
  data <- data[!is.na(data$father_birthyear),]
  data <- data %>%
    group_by(paternal_grandparents) %>%
    filter(length(unique(father_lnr))>1)
  
  # Get sample sizes 
  sample_size <- nrow(data)
  
  number_of_siblings <- table(data$paternal_grandparents)
  number_of_siblings <- as.data.frame(t(number_of_siblings))
  number_of_families <- as.data.frame(table(number_of_siblings$Freq))
  number_of_families <- sum(number_of_families$Freq)
  loss_duplicated <- sample_size_beforedup - sample_size_afterdup
  loss_missingbirthyear <- sample_size_afterdup - sample_size
  
  # Data transformation
  data$sex <- as.numeric(data$sex)
  
  # Descriptives
  # mean, sd, skew and covariances 
  
  descriptives <- as.data.frame(describe(cbind(data$parent_outcome, data$school_perf)))
  descriptives$vars <- c(trait, school_performance)
  
  covariances <- cov(cbind(data$school_perf, data$parent_outcome, 
                           data$birth_yr,  data$father_birthyear, 
                           data$sex))
  rowname <- c("school_perf", "parent_outcome", 
               "birth_yr", "father_birthyear", 
               "sex")
  covariances <- cbind(as.data.frame(covariances), rowname)
  means <- colMeans(cbind(data$school_perf, data$parent_outcome, 
                          data$birth_yr,  data$father_birthyear, 
                          data$sex))
  means <- cbind(as.data.frame(means), rowname)
  
  # ICC
  m0 <- lme(parent_outcome ~ 1, 
            random=~1|paternal_grandparents, 
            method="ML", 
            na.action=na.omit,
            data=data)
  ICC <- ICCest(m0)
  
  
  # STD the trait
  data$parent_outcome <- as.numeric(scale(data$parent_outcome))
  # careful when using mutate_at, this gives a wrong result: data %>% mutate_at( "scale_score_items_m_ADHD_Q3", scale)
  
  
  # plot the data 
  htop <- ggplot(data=data, aes(x=school_perf)) + 
    geom_histogram(aes(y=..density..), fill = "white", 
                   color = "black", binwidth = 1) + 
    #stat_density(colour = "blue", geom="line", size = 1.5, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous(breaks = 40) + 
    scale_y_continuous("Count") + 
    theme_bw() + theme(axis.title.x = element_blank())
  
  blank <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), panel.background=element_blank(), 
          panel.grid=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hright <- ggplot(data=data, aes(x=parent_outcome)) + 
    geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
    #stat_density(colour = "red", geom="line", size = 1, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
    scale_y_continuous("Count") + 
    coord_flip() + theme_bw() + 
    theme(axis.title.y = element_blank())
  
  quantile_plot <-
    ggplot(data= data,aes( y=parent_outcome ,x= school_perf)) +
    geom_quantile(method = "rqss", lambda = 0.4) +
    theme_light()+
    xlab(school_performance)+
    ylab(trait)
  
  figure <- arrangeGrob(htop, blank, quantile_plot, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  ggsave(file=paste0(school_performance,"_",trait,"_father.png"), figure)
  
  # Create within and between variables 
  mean_pheno<-group_by(data,paternal_grandparents) %>% 
    summarize(m=mean(parent_outcome))
  colnames(mean_pheno) <- c("paternal_grandparents", "between_pheno")
  
  finalsib<-merge(data,mean_pheno,by="paternal_grandparents")
  finalsib$within_pheno <- finalsib$parent_outcome - finalsib$between_pheno 
  finalsib
  
  # Run observational analysis 
  modpop<-lme(school_perf ~ parent_outcome + 
                birth_yr + father_birthyear +
                sex ,
              random=~1|paternal_grandparents,
              na.action=na.omit,
              data=finalsib)
  modpop_coef <- summary(modpop)$tTable
  
  #Run within-sibling analysis 
  modwithin<-lme(school_perf ~ within_pheno + between_pheno +
                   birth_yr +  father_birthyear +
                   sex ,
                 random=~1|paternal_grandparents,
                 na.action=na.omit,
                 data=finalsib)
  modwithin_coef <- summary(modwithin)$tTable
  
  # Save all results 
  results <- list(trait, school_performance, # variables
                  sample_size, number_of_families, number_of_siblings, loss_duplicated, loss_missingbirthyear,  # sample sizes 
                  descriptives, ICC, covariances, means, #descriptives
                  modpop_coef, modwithin_coef) # regressions results 
  results
} 

run_analyses_father_EA <- function(data, school_performance, trait, onekid) {
  
  data$parent_outcome <- data[[trait]]
  data$school_perf <- data[[school_performance]]
  
  # Remove missing data 
  data <- data %>% drop_na(parent_outcome) 
  
  # Remove families without 2 sisters 
  data <- data %>%
    group_by(paternal_grandparents) %>%
    filter(length(unique(father_lnr))>1)
  
  # If more than one kid per father, randomly remove one 
  if (onekid == T){
    data <- data %>%
      group_by(father_lnr) %>%
      slice_head(n = 1) %>%
      ungroup() 
  }
  
  # Remove duplicated children, part of two families
  sample_size_beforedup <- nrow(data)
  duplicates <- sum(duplicated(data$w19_0634_lnr)) 
  data <- data[!duplicated(data$w19_0634_lnr),]
  data <- data %>%
    group_by(paternal_grandparents) %>%
    filter(length(unique(father_lnr))>1)
  
  sample_size_afterdup <- nrow(data)
  
  # Remove individual with missing birthyear for father or mother and get estimate of how much that is 
  # Ideally I cna find all missing data in additional MOba surveys
  
  data <- data[!is.na(data$father_birthyear),]
  data <- data[!is.na(data$father_EduYears11_2018),]
  data <- data %>%
    group_by(paternal_grandparents) %>%
    filter(length(unique(father_lnr))>1)
  
  # Get sample sizes 
  sample_size <- nrow(data)
  
  number_of_siblings <- table(data$paternal_grandparents)
  number_of_siblings <- as.data.frame(t(number_of_siblings))
  number_of_families <- as.data.frame(table(number_of_siblings$Freq))
  number_of_families <- sum(number_of_families$Freq)
  loss_duplicated <- sample_size_beforedup - sample_size_afterdup
  loss_missingbirthyear <- sample_size_afterdup - sample_size
  
  # Data transformation
  data$sex <- as.numeric(data$sex)
  
  # Descriptives
  # mean, sd, skew and covariances 
  
  descriptives <- as.data.frame(describe(cbind(data$parent_outcome, data$school_perf)))
  descriptives$vars <- c(trait, school_performance)
  
  covariances <- cov(cbind(data$school_perf, data$parent_outcome, 
                           data$birth_yr,  data$father_birthyear, 
                           data$sex, 
                           data$father_EduYears11_2018))
  rowname <- c("school_perf", "parent_outcome", 
               "birth_yr", 
               "father_birthyear", 
               "sex", 
               "father_EA")
  means <- cbind(as.data.frame(means), rowname)
  means <- colMeans(cbind(data$school_perf, data$parent_outcome, 
                          data$birth_yr,  data$father_birthyear, 
                          data$sex, 
                          data$father_EduYears11_2018))  
  means <- cbind(as.data.frame(means), rowname)
  
  
  # ICC
  m0 <- lme(parent_outcome ~ 1, 
            random=~1|paternal_grandparents, 
            method="ML", 
            na.action=na.omit,
            data=data)
  ICC <- ICCest(m0)
  
  
  # STD the trait
  data$parent_outcome <- as.numeric(scale(data$parent_outcome))
  # careful when using mutate_at, this gives a wrong result: data %>% mutate_at( "scale_score_items_m_ADHD_Q3", scale)
  
  
  # plot the data 
  htop <- ggplot(data=data, aes(x=school_perf)) + 
    geom_histogram(aes(y=..density..), fill = "white", 
                   color = "black", binwidth = 1) + 
    #stat_density(colour = "blue", geom="line", size = 1.5, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous(breaks = 40) + 
    scale_y_continuous("Count") + 
    theme_bw() + theme(axis.title.x = element_blank())
  
  blank <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), panel.background=element_blank(), 
          panel.grid=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hright <- ggplot(data=data, aes(x=parent_outcome)) + 
    geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
    #stat_density(colour = "red", geom="line", size = 1, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
    scale_y_continuous("Count") + 
    coord_flip() + theme_bw() + 
    theme(axis.title.y = element_blank())
  
  quantile_plot <-
    ggplot(data= data,aes( y=parent_outcome ,x= school_perf)) +
    geom_quantile(method = "rqss", lambda = 0.4) +
    theme_light()+
    xlab(school_performance)+
    ylab(trait)
  
  figure <- arrangeGrob(htop, blank, quantile_plot, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  ggsave(file=paste0(school_performance,"_",trait,"_father_EA.png"), figure)
  
  # Create within and between variables 
  mean_pheno<-group_by(data,paternal_grandparents) %>% 
    summarize(m=mean(parent_outcome))
  colnames(mean_pheno) <- c("paternal_grandparents", "between_pheno")
  
  finalsib<-merge(data,mean_pheno,by="paternal_grandparents")
  finalsib$within_pheno <- finalsib$parent_outcome - finalsib$between_pheno 
  finalsib
  
  # Run observational analysis 
  modpop<-lme(school_perf ~ parent_outcome + 
                birth_yr + father_birthyear +
                sex +
                father_EduYears11_2018,
              random=~1|paternal_grandparents,
              na.action=na.omit,
              data=finalsib)
  modpop_coef <- summary(modpop)$tTable
  
  #Run within-sibling analysis 
  modwithin<-lme(school_perf ~ within_pheno + between_pheno +
                   birth_yr +  father_birthyear +
                   sex +
                   father_EduYears11_2018,
                 random=~1|paternal_grandparents,
                 na.action=na.omit,
                 data=finalsib)
  modwithin_coef <- summary(modwithin)$tTable
  
  # Save all results 
  results <- list(trait, school_performance, # variables
                  sample_size, number_of_families, number_of_siblings, loss_duplicated, loss_missingbirthyear,  # sample sizes 
                  descriptives, ICC, covariances, means, #descriptives
                  modpop_coef, modwithin_coef) # regressions results 
  results
} 



run_analyses_fullMoba <- function(data, school_performance, trait, onekid) {
  
  data$parent_outcome <- data[[trait]]
  data$school_perf <- data[[school_performance]]
  
  # Remove missing data 
  data <- data %>% drop_na(parent_outcome) 
  

  # If more than one kid per mother, randomly remove one 
  if (onekid == T){
    data <- data %>%
      group_by(sib_parent_id) %>%
      slice_head(n = 1) %>%
      ungroup() 
  }

  # Remove individual with missing birthyear for father or mother and get estimate of how much that is 
  # Ideally I can find all missing data in additional Moba surveys
  
  data <- data[!is.na(data$father_birthyear),]
  data <- data[!is.na(data$mother_birthyear),]

  # Get sample sizes 
  sample_size <- nrow(data)
  
  # Data transformation
  data$sex <- as.numeric(data$sex)
  data <- data %>% mutate(sib_parent = recode(sib_parent,
                                              "mother"=2,
                                              "father"=1))
  
  # Descriptives
  # mean, sd, skew and covariances 
  
  descriptives <- as.data.frame(describe(cbind(data$parent_outcome, data$school_perf)))
  descriptives$vars <- c(trait, school_performance)
  
  covariances <- cov(cbind(data$school_perf, data$parent_outcome, 
                           data$birth_yr, data$father_birthyear, data$mother_birthyear, 
                           data$sex, data$sib_parent))
  rowname <- c("school_perf", "parent_outcome", 
               "birth_yr", 
               "father_birthyear", "mother_birthyear", 
               "sex", "sib_parent")
  covariances <- cbind(as.data.frame(covariances), rowname)
  means <- colMeans(cbind(data$school_perf, data$parent_outcome, 
                          data$birth_yr, data$father_birthyear, data$mother_birthyear, 
                          data$sex, data$sib_parent))
  means <-cbind(as.data.frame(means), rowname)
  

  # STD the trait
  data$parent_outcome <- as.numeric(scale(data$parent_outcome))
  # careful when using mutate_at, this gives a wrong result: data %>% mutate_at( "scale_score_items_m_ADHD_Q3", scale)
  
  # plot the data 
  htop <- ggplot(data=data, aes(x=school_perf)) + 
    geom_histogram(aes(y=..density..), fill = "white", 
                   color = "black", binwidth = 1) + 
    #stat_density(colour = "blue", geom="line", size = 1.5, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous(breaks = 40) + 
    scale_y_continuous("Count") + 
    theme_bw() + theme(axis.title.x = element_blank())
  
  blank <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), panel.background=element_blank(), 
          panel.grid=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hright <- ggplot(data=data, aes(x=parent_outcome)) + 
    geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
    #stat_density(colour = "red", geom="line", size = 1, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
    scale_y_continuous("Count") + 
    coord_flip() + theme_bw() + 
    theme(axis.title.y = element_blank())
  
  quantile_plot <-
    ggplot(data= data,aes( y=parent_outcome ,x= school_perf)) +
    geom_quantile(method = "rqss", lambda = 0.4) +
    theme_light()+
    xlab(school_performance)+
    ylab(trait)
  
  figure <- arrangeGrob(htop, blank, quantile_plot, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  ggsave(file=paste0(school_performance,"_",trait,"fullMoba.png"), figure)
  

  # Run observational analysis 
  modpop<-lme(school_perf ~ parent_outcome + 
                birth_yr + father_birthyear + mother_birthyear +
                sex + sib_parent,
              random=~1|grandparents,
              na.action=na.omit,
              data=data)
  modpop_coef <- summary(modpop)$tTable
  

  
  # Save all results 
  results <- list(trait, school_performance, # variables
                  sample_size,  # sample sizes 
                  descriptives, covariances, means, #descriptives
                  modpop_coef) # regressions results 
  results
} 

run_analyses_fullMoba_mother <- function(data, school_performance, trait, onekid) {
  
  data$parent_outcome <- data[[trait]]
  data$school_perf <- data[[school_performance]]
  
  # Remove missing data 
  data <- data %>% drop_na(parent_outcome) 
  
  
  # If more than one kid per mother, randomly remove one 
  if (onekid == T){
    data <- data %>%
      group_by(mother_lnr.x) %>%
      slice_head(n = 1) %>%
      ungroup() 
  }
  
  # Remove individual with missing birthyear for father or mother and get estimate of how much that is 
  # Ideally I can find all missing data in additional Moba surveys
  
  data <- data[!is.na(data$father_birthyear),]
  data <- data[!is.na(data$mother_birthyear),]
  
  # Get sample sizes 
  sample_size <- nrow(data)
  
  # Data transformation
  data$sex <- as.numeric(data$sex)
  
  # Descriptives
  # mean, sd, skew and covariances 
  
  descriptives <- as.data.frame(describe(cbind(data$parent_outcome, data$school_perf)))
  descriptives$vars <- c(trait, school_performance)
  
  covariances <- cov(cbind(data$school_perf, data$parent_outcome, 
                           data$birth_yr, data$mother_birthyear, 
                           data$sex))
  rowname <- c("school_perf", "parent_outcome", 
               "birth_yr", 
               "mother_birthyear", 
               "sex")
  covariances <- cbind(as.data.frame(covariances), rowname)
  means <- colMeans(cbind(data$school_perf, data$parent_outcome, 
                          data$birth_yr, data$mother_birthyear, 
                          data$sex))
  means <-cbind(as.data.frame(means), rowname)
  
  
  # STD the trait
  data$parent_outcome <- as.numeric(scale(data$parent_outcome))
  # careful when using mutate_at, this gives a wrong result: data %>% mutate_at( "scale_score_items_m_ADHD_Q3", scale)
  
  # plot the data 
  htop <- ggplot(data=data, aes(x=school_perf)) + 
    geom_histogram(aes(y=..density..), fill = "white", 
                   color = "black", binwidth = 1) + 
    #stat_density(colour = "blue", geom="line", size = 1.5, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous(breaks = 40) + 
    scale_y_continuous("Count") + 
    theme_bw() + theme(axis.title.x = element_blank())
  
  blank <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), panel.background=element_blank(), 
          panel.grid=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hright <- ggplot(data=data, aes(x=parent_outcome)) + 
    geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
    #stat_density(colour = "red", geom="line", size = 1, position="identity", show_guide=FALSE) +
    stat_function(fun = dnorm, n = 101, 
                  args = list(mean = 0, sd = 1), color="blue") + ylab("") +
    #scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
    scale_y_continuous("Count") + 
    coord_flip() + theme_bw() + 
    theme(axis.title.y = element_blank())
  
  quantile_plot <-
    ggplot(data= data,aes( y=parent_outcome ,x= school_perf)) +
    geom_quantile(method = "rqss", lambda = 0.4) +
    theme_light()+
    xlab(school_performance)+
    ylab(trait)
  
  figure <- arrangeGrob(htop, blank, quantile_plot, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  ggsave(file=paste0(school_performance,"_",trait,"fullMoba_mother.png"), figure)
  
  
  # Run observational analysis 
  modpop<-lme(school_perf ~ parent_outcome + 
                birth_yr + mother_birthyear +
                sex,
              random=~1|maternal_grandparents,
              na.action=na.omit,
              data=data)
  modpop_coef <- summary(modpop)$tTable
  
  
  
  # Save all results 
  results <- list(trait, school_performance, # variables
                  sample_size,  # sample sizes 
                  descriptives, covariances, means, #descriptives
                  modpop_coef) # regressions results 
  results
} 

