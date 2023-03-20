# Figures and final tables
# Perline Demange

library(ggplot2)
rm(list=ls())
# when minor grid coincide with major grid that has been disabled it is not appearing 
# workaround, to do everytime 
trace(ggplot2:::guide_grid, edit=TRUE)
# I manually remove 
# x.minor <- setdiff(x.minor, x.major)
# y.minor <- setdiff(y.minor, y.major)
#can re-edit with untrace(ggplot2:::guide_grid)


#1. Figure 2. +  FDR pvalues ######

# load all siblings results 
load("output/full_results_siblings_total_230109.rda") 

#weirdly t.value and p.value are not written the same 
full_results_siblings_total <- full_results_siblings_total %>% 
  rename(
    p.value = `p-value`,
    t.value = `t-value`
  )


# load mothers results for eating disorder symptoms 
load("output/full_results_siblings_mother_2301012.rda")

full_results_siblings_mother <- full_results_siblings_mother %>% 
  rename(
    p.value = `p-value`,
    t.value = `t-value`
  )

full_results_siblings_mother <- full_results_siblings_mother[
  which(full_results_siblings_mother$parental_trait ==
          "scale_score_items_m_ED_NRM_Q1" | 
          full_results_siblings_mother$parental_trait == 
          "scale_score_items_m_ED_NRM_Q8"), ]


full_results <- rbind(full_results_siblings_mother, full_results_siblings_total)


# Load results adjusted for EA
load("output/full_results_siblings_total_EA_230109.rda") # new 
full_results_siblings_total_EA <- full_results_siblings_total
load("output/full_results_siblings_mother_EA_230112.rda")
full_results_siblings_EA <- full_results_siblings_mother
full_results_siblings_EA <- full_results_siblings_EA[
  which(full_results_siblings_EA$parental_trait ==
          "scale_score_items_m_ED_NRM_Q1" | 
          full_results_siblings_EA$parental_trait == 
          "scale_score_items_m_ED_NRM_Q8"), ]

full_EA <- rbind(full_results_siblings_EA , full_results_siblings_total_EA )
full_EA$estimates <- paste(full_EA$estimates, "_EA", sep="")
full_EA <- full_EA %>% 
  rename(
    p.value = `p-value`,
    t.value = `t-value`
  )


results_plot <- rbind(full_EA, full_results)

# Create fdr-corrected pvalues 
results_plot <- results_plot %>%
  group_by(estimates) %>% 
  mutate("p.value.fdr" = p.adjust(p.value, method="fdr"))
write.csv2(results_plot, "main_results_allsex_fdr_230112.csv")


# Remove between effects as we do not plot it 
results_plot <- results_plot[results_plot$estimates %in% 
                               c("Within_effect", "Population",
                                 "Within_effect_EA"),]


# run one of the three versions 
## 1.1 Version with EA adjusted #######
# create factors to fix order and rename 
results_plot$estimates <- factor(results_plot$estimates, 
                                 levels = c("Within_effect_EA", 
                                            "Within_effect", 
                                            "Population"))

results_plot$school_subject <- factor(results_plot$school_subject,
                                      levels = c("NPREG05","NPLES05", "NPENG05"), 
                                      labels = c('Maths','Reading','English'))


results_plot$parental_trait <- factor(results_plot$parental_trait,
                                      levels = c("scale_score_items_m_ED_NRM_Q8","scale_score_items_m_ED_NRM_Q1",
                                                  "ADHD","AUDIT",
                                                  "SCL8", "SCL5_Q1"), 
                                      labels = c("Eating disorder","Eating disorder in pregnancy",
                                                 "ADHD","Alcohol problematic use",
                                                 "Anx/Dep", "Anx/Dep in pregnancy"
                                                 ))



# Create figure 
png("output/figure1_0112.png", width= 1800, height=1200)
ggplot(results_plot, aes(x=parental_trait, y=Value, fill=estimates)) + 
  geom_bar(position=position_dodge(0.9), stat="identity") +
  geom_errorbar(aes(ymin= Value - 1.96 * Std.Error, ymax=Value + 1.96 * Std.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) + 
  #pvalue star doesnt work, it is located in the middle # I add them manually on powerpoint 
  # geom_point(data = results_plot[results_plot$p.value.fdr < .05, ], # star to indicate the significance of the difference
  #            aes(y=-0.1, x=parental_trait, fill=estimates), color= "red",
  #            shape = 8, size=3, position=position_dodge(.9)) +
  geom_hline(yintercept=0, colour="black")+
  coord_flip()+
  guides(fill = guide_legend(reverse = T))+
  theme_light()+
  facet_grid(. ~ school_subject)+
  #scale_fill_manual(values=c("#008fd5", "#de6b35", "yellow"))+
  theme(text = element_text(size = 30), 
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()


## 1.2 Pop + within-siblings #######
results_plot <- results_plot[!(results_plot$estimates == "Within_effect_EA"),]
# create factors to fix order and rename 
results_plot$estimates <- factor(results_plot$estimates, 
                                 levels = c("Within_effect", 
                                            "Population"))

results_plot$school_subject <- factor(results_plot$school_subject,
                                      levels = c("NPREG05","NPLES05", "NPENG05"), 
                                      labels = c('Maths','Reading','English'))


results_plot$parental_trait <- factor(results_plot$parental_trait,
                                      levels = c("scale_score_items_m_ED_NRM_Q8","scale_score_items_m_ED_NRM_Q1",
                                                 "ADHD","AUDIT",
                                                 "SCL8", "SCL5_Q1"), 
                                      labels = c("Eating disorder","Eating disorder in pregnancy",
                                                 "ADHD","Alcohol problematic use",
                                                 "Anx/Dep", "Anx/Dep in pregnancy"
                                      ))



# Create figure 
png("output/figure1_0112.png", width= 1800, height=1200)
ggplot(results_plot, aes(x=parental_trait, y=Value, col=estimates, fill=estimates)) + 
  geom_hline(yintercept=0, colour="grey", size=1)+
  geom_point(position=position_dodge2(width=.7, preserve = "single"),
             stat="identity",
             size=10,
             shape=21,
             stroke=0.5) +
  geom_linerange(aes(ymin= Value - 1.96 * Std.Error, 
                     ymax=Value + 1.96 * Std.Error, 
                     col=estimates),
                 size=2.5,
                position=position_dodge2(width=.7, preserve = "single")) + 
  #pvalue star doesnt work, it is located in the middle # I add them manually on powerpoint 
  coord_flip()+
  scale_fill_manual(values=c("#00889b", "#83eb9d") , 
                    breaks = c("Population",
                               "Within_effect")) +
  scale_color_manual(values=c("#00889b", "#83eb9d"), 
                     breaks = c("Population", 
                                "Within_effect"))+
  #guides(fill = guide_legend(reverse = T))+
  theme_light(base_size=20)+
  facet_grid(. ~ school_subject)+
  theme(text = element_text(size = 30), 
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()


## 1.3 Version with full Moba pop as well ####
load("output/full_results_fullMoba_total_230112.rda")
fullmoba <- full_results_fullMoba_total
load("output/full_results_fullMoba_mother_230112.rda")
fullmobamother <- full_results_fullMoba_total

fullmoba_plot <- rbind(fullmoba, fullmobamother)

# Create fdr-corrected pvalues 
fullmoba_plot <- fullmoba_plot %>%
  mutate("p.value.fdr" = p.adjust(p.value, method="fdr"))
write.csv2(fullmoba_plot, "fullmoba_results_fdr_230112.csv")

# combine 
results_plot <- results_plot[!(results_plot$estimates == "Within_effect_EA"),]
results_plot <- results_plot[,c("estimates", "Value", "Std.Error","DF",
                                 "t.value", "p.value", "school_subject", 
                                 "parental_trait", "sample_size")]
fullmoba_plot <- fullmoba_plot[,c("estimates", "Value", "Std.Error","DF",
                                "t.value", "p.value", "school_subject", 
                                "parental_trait", "sample_size")]
results_plot <- rbind(results_plot, fullmoba_plot)

# create factors to fix order and rename 
results_plot$estimates <- factor(results_plot$estimates, 
                                 levels = c("Within_effect", 
                                            "Population", 
                                            "Population_fullMoba"), 
                                 labels = c("Within-sibling effect in cousins sample", 
                                            "Observational association in cousins sample", 
                                            "Observational association in total sample"))

results_plot$school_subject <- factor(results_plot$school_subject,
                                      levels = c("NPREG05","NPLES05", "NPENG05"), 
                                      labels = c('Maths','Reading','English'))


results_plot$parental_trait <- factor(results_plot$parental_trait,
                                      levels = c("scale_score_items_m_ED_NRM_Q8","scale_score_items_m_ED_NRM_Q1",
                                                 "ADHD","AUDIT",
                                                 "SCL8", "SCL5_Q1"), 
                                      labels = c("Eating disorder","Eating disorder in pregnancy",
                                                 "ADHD","Alcohol problematic use",
                                                 "Anx/Dep", "Anx/Dep in pregnancy"
                                      ))



# Create figure 
png("output/figure1_version3_0112.png", width= 2200, height=1200)
ggplot(results_plot, aes(x=parental_trait, y=Value, col=estimates, fill=estimates)) + 
  geom_hline(yintercept=0, colour="grey", size=1)+
  geom_point(position=position_dodge2(width=.7, preserve = "single"),
             stat="identity",
             size=10,
             shape=21,
             stroke=0.5) +
  geom_linerange(aes(ymin= Value - 1.96 * Std.Error, 
                     ymax=Value + 1.96 * Std.Error, 
                     col=estimates),
                 size=2.5,
                 position=position_dodge2(width=.7, preserve = "single")) + 
  #pvalue star doesnt work, it is located in the middle # I add them manually on powerpoint 
  coord_flip()+
  scale_fill_manual(values=c("black", "#00889b", "#83eb9d") , 
                    breaks = c("Observational association in total sample", 
                               "Observational association in cousins sample", 
                               "Within-sibling effect in cousins sample")) +
  scale_color_manual(values=c("black", "#00889b", "#83eb9d"), 
                     breaks = c("Observational association in total sample", 
                                "Observational association in cousins sample", 
                                "Within-sibling effect in cousins sample"))+
  #guides(fill = guide_legend(reverse = T))+
  theme_light(base_size=20)+
  facet_grid(. ~ school_subject)+
  theme(text = element_text(size = 30), 
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()

#2. Table 1. & co. Print tables and do average for main results #####
## 2.1 Descriptive statistics ####
load("output/full_results_siblings_total_descriptives_230109.rda")
descriptives_all <- descriptives
load("output/full_results_siblings_total_descriptives_EA_230109.rda")
descriptives_all_EA <- descriptives
load("output/full_results_siblings_mother_descriptives_230112.rda")
descriptives_mother <- descriptives
load("output/full_results_siblings_mother_descriptives_EA_230112.rda")
descriptives_mother_EA <- descriptives

descriptives_all$group <- "all"
descriptives_all_EA$group <- "all_EA"
descriptives_mother$group <- "mother"
descriptives_mother_EA$group <- "mother_EA"

descriptives <- rbind(descriptives_all, descriptives_all_EA, descriptives_mother, descriptives_mother_EA)

# keep only main results (remove extra mothers')
descriptives <- descriptives[descriptives$trait %in% c("SCL5_Q1", 
                                                       "SCL8", 
                                                       "ADHD", 
                                                       "AUDIT", 
                                                       "scale_score_items_m_ED_NRM_Q1", 
                                                       "scale_score_items_m_ED_NRM_Q8"),]
write.csv2(descriptives, "descriptive_table.csv")

# Separate parental mental health and academic skills for more clarity 
head(descriptives)
ac_skills <- descriptives[descriptives$vars %in% c("std_score_NPREG05", 
                                                 "std_score_NPLES05", 
                                                 "std_score_NPENG05"),]
parentsmh <- descriptives[descriptives$vars %in% c("SCL5_Q1", 
                                                 "SCL8", 
                                                 "ADHD", 
                                                 "AUDIT", 
                                                 "scale_score_items_m_ED_NRM_Q1", 
                                                 "scale_score_items_m_ED_NRM_Q8"),]


write.csv2(ac_skills, "descriptive_ac_skills_main.csv")
write.csv2(parentsmh, "descriptive_parentsmh_main.csv")

## 2.2 Make table 1B : descriptive of ac. skills on average ####
# As we decided to not present the model adjusted for EA in the figure, 
# I also only keep the all and mother samples here 

ac_skills <- ac_skills[ac_skills$group %in% c("all", 
                                                "mother"),]

table2b <- ac_skills %>%
  group_by(schoolsub) %>%
  summarise_at(c("mean", "sd", "skew"), mean, na.rm = TRUE)

png("output/table2b.png", height=50*nrow(table2b), width=200*ncol(table2b))
p<-gridExtra::tableGrob(table2b,  theme=gridExtra::ttheme_minimal())
gridExtra::grid.arrange(p)
dev.off()


## 2.3 Make part of table 1A : descriptive of parental mental health on average ####
# As we decided to not present the model adjusted for EA in the figure, 
# I also only keep the all and mother samples here 

parentsmh <- parentsmh[parentsmh$group %in% c("all", 
                                              "mother"),]

table2a <- parentsmh %>%
  group_by(trait) %>%
  summarise_at(c("n", "mean", "sd", "skew"), mean, na.rm = TRUE)

png("output/table2a_part1.png", height=50*nrow(table2a), width=200*ncol(table2a))
p<-gridExtra::tableGrob(table2a,  theme=gridExtra::ttheme_minimal())
gridExtra::grid.arrange(p)
dev.off()


## 2.4 Means ####
load("output/full_results_siblings_total_means_230109.rda")
means_all <- means
load("output/full_results_siblings_total_means_EA_230109.rda")
means_all_EA <- means
load("output/full_results_siblings_mother_means_230112.rda")
means_mother <- means
load("output/full_results_siblings_mother_means_EA_230112.rda")
means_mother_EA <- means

means_all$group <- "all"
means_all_EA$group <- "all_EA"
means_mother$group <- "mother"
means_mother_EA$group <- "mother_EA"

means <- rbind(means_all, means_all_EA, means_mother, means_mother_EA)
means <- means[means$trait %in% c("SCL5_Q1",  "SCL8", 
                                                       "ADHD", 
                                                       "AUDIT", 
                                                       "scale_score_items_m_ED_NRM_Q1", 
                                                       "scale_score_items_m_ED_NRM_Q8"),]


meanswide <- means %>% pivot_wider (names_from = rowname, values_from = means)

head(meanswide)
write.csv2(meanswide, "means_main.csv")

## 2.5 Make part of table 1A : mean gender ####

means<- means[means$group %in% c("all", "mother"),]
meanssex <- means[means$rowname == "sex",]
table2asex <- meanssex %>%
  group_by(trait) %>%
  summarise_at("means", mean, na.rm = TRUE)
meansparent <- means[means$rowname == "sib_parent",]
table2aparent <- meansparent %>%
  group_by(trait) %>%
  summarise_at("means", mean, na.rm = TRUE)

png("output/table2a_part2.png", height=50*10, width=200*ncol(table2asex))
p<-gridExtra::tableGrob(rbind(table2asex, table2aparent),  theme=gridExtra::ttheme_minimal())
gridExtra::grid.arrange(p)
dev.off()

## 2.6 Make part of Table 1A: number of families + ICC ######

# load all siblings results 
load("output/full_results_siblings_total_230109.rda") 
# load mothers results for eating disorder symptoms 
load("output/full_results_siblings_mother_2301012.rda")

full_results_siblings_mother <- full_results_siblings_mother[
  which(full_results_siblings_mother$parental_trait ==
          "scale_score_items_m_ED_NRM_Q1" | 
          full_results_siblings_mother$parental_trait == 
          "scale_score_items_m_ED_NRM_Q8"), ]
head(full_results_siblings_mother)

numfam <- rbind(full_results_siblings_total[,c("parental_trait", "sample_size", "ICC", "number_of_families")], 
                full_results_siblings_mother[,c("parental_trait", "sample_size", "ICC", "number_of_families")])

table2asample <- numfam %>%
  group_by(parental_trait) %>%
  summarise_at(c("sample_size", "ICC", "number_of_families"),
               mean, na.rm = TRUE)

png("output/table2a_part3.png", height=50*nrow(table2asample), width=200*ncol(table2asample))
p<-gridExtra::tableGrob(table2asample,  theme=gridExtra::ttheme_minimal())
gridExtra::grid.arrange(p)
dev.off()

# 3. Prepare tables for Supplementary Tables #####
## 3.1 Fullmoba ######
### 3.1.1 Descriptives ########
load("output/full_results_siblings_fullMoba_descriptives_230112.rda")
descriptives_all <- descriptives
load("output/full_results_siblings_fullMoba_mother_descriptives_230112.rda")
descriptives_mother <- descriptives
descriptives_all$group <- "all"
descriptives_mother$group <- "mother"

descriptives <- rbind(descriptives_all,  descriptives_mother)

ac_skills <- descriptives[descriptives$vars %in% c("std_score_NPREG05", 
                                                   "std_score_NPLES05", 
                                                   "std_score_NPENG05"),]
parentsmh <- descriptives[descriptives$vars %in% c("SCL5_Q1", 
                                                   "SCL8", 
                                                   "ADHD", 
                                                   "AUDIT", 
                                                   "scale_score_items_m_ED_NRM_Q1", 
                                                   "scale_score_items_m_ED_NRM_Q8"),]


write.csv2(ac_skills, "descriptive_ac_skills_fullmoba.csv")
write.csv2(parentsmh, "descriptive_parentsmh_fullmoba.csv")


table2b <- ac_skills %>%
  group_by(schoolsub) %>%
  summarise_at(c("mean", "sd", "skew"), mean, na.rm = TRUE)

png("output/table2b_fullmoba.png", height=50*nrow(table2b), width=200*ncol(table2b))
p<-gridExtra::tableGrob(table2b,  theme=gridExtra::ttheme_minimal())
gridExtra::grid.arrange(p)
dev.off()

table2a <- parentsmh %>%
  group_by(trait) %>%
  summarise_at(c("n", "mean", "sd", "skew"), mean, na.rm = TRUE)

png("output/table2a_part1_fullmoba.png", height=50*nrow(table2a), width=200*ncol(table2a))
p<-gridExtra::tableGrob(table2a,  theme=gridExtra::ttheme_minimal())
gridExtra::grid.arrange(p)
dev.off()

#Means
load("output/full_results_siblings_fullMoba_means_EA_230112.rda")
means_all <- means
load("output/full_results_siblings_fullMoba_mother_means_230112.rda")
means_mother <- means

means_all$group <- "all"
means_mother$group <- "mother"

means <- rbind(means_all,  means_mother)
meanswide <- means %>% pivot_wider (names_from = rowname, values_from = means)
head(meanswide)
write.csv2(meanswide, "means_fullMoba.csv")

meanssex <- means[means$rowname == "sex",]
table2asex <- meanssex %>%
  group_by(trait) %>%
  summarise_at("means", mean, na.rm = TRUE)
meansparent <- means[means$rowname == "sib_parent",]
table2aparent <- meansparent %>%
  group_by(trait) %>%
  summarise_at("means", mean, na.rm = TRUE)

png("output/table2a_part2_fullmoba.png", height=50*10, width=200*ncol(table2asex))
p<-gridExtra::tableGrob(rbind(table2asex, table2aparent),  theme=gridExtra::ttheme_minimal())
gridExtra::grid.arrange(p)
dev.off()

#there is no icc and number of families for these results 

## 3.2 Mothers only #####
### 3.2.1 Results & figure #####
load("output/full_results_siblings_mother_2301012.rda")
full_results_siblings_mother1 <- full_results_siblings_mother
load("output/full_results_siblings_mother_EA_230112.rda")
full_results_siblings_mother$estimates <- paste(full_results_siblings_mother$estimates, "_EA", sep="")
results_plot <- rbind(full_results_siblings_mother1, full_results_siblings_mother)
#write.csv2(results_plot, "core_results_mothers.csv")

results_plot$estimates <- factor(results_plot$estimates, 
                                 levels = c("Within_effect_EA", 
                                            "Within_effect", 
                                            "Population"))

results_plot$school_subject <- factor(results_plot$school_subject,
                                      levels = c("NPREG05","NPLES05", "NPENG05"), 
                                      labels = c('Maths','Reading','English'))

png("output/figure_mother_only_allscales.png", width= 2200, height=1200)
ggplot(results_plot, aes(x=parental_trait, y=Value, col=estimates, fill=estimates)) + 
  geom_hline(yintercept=0, colour="grey", size=1)+
  geom_point(position=position_dodge2(width=.7, preserve = "single"),
             stat="identity",
             size=10,
             shape=21,
             stroke=0.5) +
  geom_linerange(aes(ymin= Value - 1.96 * Std.Error, 
                     ymax=Value + 1.96 * Std.Error, 
                     col=estimates),
                 size=2.5,
                 position=position_dodge2(width=.7, preserve = "single")) + 
  #pvalue star doesnt work, it is located in the middle # I add them manually on powerpoint 
  coord_flip()+
  scale_fill_manual(values=c("#00889b", "#83eb9d", "orange") , 
                    breaks = c("Population", 
                               "Within_effect", 
                               "Within_effect_EA")) +
  scale_color_manual(values=c("#00889b", "#83eb9d", "orange"), 
                     breaks = c("Population", 
                                "Within_effect", 
                                "Within_effect_EA"))+
  #guides(fill = guide_legend(reverse = T))+
  theme_light(base_size=20)+
  facet_grid(. ~ school_subject)+
  theme(text = element_text(size = 30), 
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()

results_plot <- results_plot[results_plot$parental_trait %in%
                               c("scale_score_items_m_SCL_Q1_full", 
                                 "scale_score_items_m_SCL_Q8_full", 
                                 "scale_score_items_m_ADHD_Q3", 
                                 "scale_score_items_m_AUDIT_Q8",
                                 "scale_score_items_m_ED_NRM_Q1",
                                 "scale_score_items_m_ED_NRM_Q8"),]
results_plot <- results_plot[results_plot$estimates %in%
                               c("Within_effect", 
                                 "Population"),]
png("output/figure_mother_only_corescales.png", width= 2200, height=1200)
ggplot(results_plot, aes(x=parental_trait, y=Value, col=estimates, fill=estimates)) + 
  geom_hline(yintercept=0, colour="grey", size=1)+
  geom_point(position=position_dodge2(width=.7, preserve = "single"),
             stat="identity",
             size=10,
             shape=21,
             stroke=0.5) +
  geom_linerange(aes(ymin= Value - 1.96 * Std.Error, 
                     ymax=Value + 1.96 * Std.Error, 
                     col=estimates),
                 size=2.5,
                 position=position_dodge2(width=.7, preserve = "single")) + 
  #pvalue star doesnt work, it is located in the middle # I add them manually on powerpoint 
  coord_flip()+
  scale_fill_manual(values=c("#00889b", "#83eb9d") , 
                    breaks = c("Population", 
                               "Within_effect")) +
  scale_color_manual(values=c("#00889b", "#83eb9d"), 
                     breaks = c("Population", 
                                "Within_effect"))+
  #guides(fill = guide_legend(reverse = T))+
  theme_light(base_size=20)+
  facet_grid(. ~ school_subject)+
  theme(text = element_text(size = 30), 
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()


### 3.2.2 Descriptives ######
load("output/full_results_siblings_mother_descriptives_230112.rda")
descriptives_mother <- descriptives
load("output/full_results_siblings_mother_descriptives_EA_230112.rda")
descriptives_mother_EA <- descriptives

descriptives_mother$group <- "mother"
descriptives_mother_EA$group <- "mother_EA"

descriptives <- rbind( descriptives_mother, descriptives_mother_EA)

# Separate parental mental health and academic skills for more clarity 
ac_skills <- descriptives[descriptives$vars %in% c("std_score_NPREG05", 
                                                   "std_score_NPLES05", 
                                                   "std_score_NPENG05"),]
parentsmh <- descriptives[!(descriptives$vars %in% c("std_score_NPREG05", 
                                                          "std_score_NPLES05", 
                                                          "std_score_NPENG05")),]


write.csv2(ac_skills, "descriptive_ac_skills_mothers.csv")
write.csv2(parentsmh, "descriptive_parentsmh_mothers.csv")


load("output/full_results_siblings_mother_means_230112.rda")
means_mother <- means
load("output/full_results_siblings_mother_means_EA_230112.rda")
means_mother_EA <- means


means_mother$group <- "mother"
means_mother_EA$group <- "mother_EA"

means <- rbind( means_mother, means_mother_EA)

meanswide <- means %>% pivot_wider (names_from = rowname, values_from = means)

head(meanswide)
write.csv2(meanswide, "means_mothers.csv")

## 3.3 Fathers only #######
### 3.3.1 Results & figure #####
load("output/full_results_siblings_father_2301012.rda")
full_results_siblings_father1 <- full_results_siblings_father
load("output/full_results_siblings_father_EA_230112.rda")
full_results_siblings_father$estimates <- paste(full_results_siblings_father$estimates, "_EA", sep="")
results_plot <- rbind(full_results_siblings_father1, full_results_siblings_father)
#write.csv2(results_plot, "core_results_fathers.csv")

results_plot$estimates <- factor(results_plot$estimates, 
                                 levels = c("Within_effect_EA", 
                                            "Within_effect", 
                                            "Population"))

results_plot$school_subject <- factor(results_plot$school_subject,
                                      levels = c("NPREG05","NPLES05", "NPENG05"), 
                                      labels = c('Maths','Reading','English'))

png("output/figure_father_only_allscales.png", width= 2200, height=1200)
ggplot(results_plot, aes(x=parental_trait, y=Value, col=estimates, fill=estimates)) + 
  geom_hline(yintercept=0, colour="grey", size=1)+
  geom_point(position=position_dodge2(width=.7, preserve = "single"),
             stat="identity",
             size=10,
             shape=21,
             stroke=0.5) +
  geom_linerange(aes(ymin= Value - 1.96 * Std.Error, 
                     ymax=Value + 1.96 * Std.Error, 
                     col=estimates),
                 size=2.5,
                 position=position_dodge2(width=.7, preserve = "single")) + 
  #pvalue star doesnt work, it is located in the middle # I add them manually on powerpoint 
  coord_flip()+
  scale_fill_manual(values=c("#00889b", "#83eb9d", "orange") , 
                    breaks = c("Population", 
                               "Within_effect", 
                               "Within_effect_EA")) +
  scale_color_manual(values=c("#00889b", "#83eb9d", "orange"), 
                     breaks = c("Population", 
                                "Within_effect", 
                                "Within_effect_EA"))+
  #guides(fill = guide_legend(reverse = T))+
  theme_light(base_size=20)+
  facet_grid(. ~ school_subject)+
  theme(text = element_text(size = 30), 
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()

results_plot <- results_plot[results_plot$parental_trait %in%
                               c("scale_score_items_f_SCL_Q1_SCL5", 
                                 "scale_score_items_f_SCL_Q2015_full", 
                                 "scale_score_items_f_ADHD_Q1", 
                                 "scale_score_items_f_AUDIT_Q2015"),]
results_plot <- results_plot[results_plot$estimates %in%
                               c("Within_effect","Population"
                                 ),]
png("output/figure_father_only_corescales.png", width= 2200, height=1200)
ggplot(results_plot, aes(x=parental_trait, y=Value, col=estimates, fill=estimates)) + 
  geom_hline(yintercept=0, colour="grey", size=1)+
  geom_point(position=position_dodge2(width=.7, preserve = "single"),
             stat="identity",
             size=10,
             shape=21,
             stroke=0.5) +
  geom_linerange(aes(ymin= Value - 1.96 * Std.Error, 
                     ymax=Value + 1.96 * Std.Error, 
                     col=estimates),
                 size=2.5,
                 position=position_dodge2(width=.7, preserve = "single")) + 
  #pvalue star doesnt work, it is located in the middle # I add them manually on powerpoint 
  coord_flip()+
  scale_fill_manual(values=c("#00889b", "#83eb9d") , 
                    breaks = c("Population", 
                               "Within_effect")) +
  scale_color_manual(values=c("#00889b", "#83eb9d"), 
                     breaks = c("Population", 
                                "Within_effect"))+
  #guides(fill = guide_legend(reverse = T))+
  theme_light(base_size=20)+
  facet_grid(. ~ school_subject)+
  theme(text = element_text(size = 30), 
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
dev.off()

### 3.3.2 Descriptives ######
load("output/full_results_siblings_father_descriptives_230112.rda")
descriptives_father <- descriptives
load("output/full_results_siblings_father_descriptives_EA_230112.rda")
descriptives_father_EA <- descriptives

descriptives_father$group <- "father"
descriptives_father_EA$group <- "father_EA"

descriptives <- rbind( descriptives_father, descriptives_father_EA)

# Separate parental mental health and academic skills for more clarity 
ac_skills <- descriptives[descriptives$vars %in% c("std_score_NPREG05", 
                                                   "std_score_NPLES05", 
                                                   "std_score_NPENG05"),]
parentsmh <- descriptives[!(descriptives$vars %in% c("std_score_NPREG05", 
                                                     "std_score_NPLES05", 
                                                     "std_score_NPENG05")),]


write.csv2(ac_skills, "descriptive_ac_skills_fathers.csv")
write.csv2(parentsmh, "descriptive_parentsmh_fathers.csv")


load("output/full_results_siblings_father_means_230112.rda")
means_father <- means
load("output/full_results_siblings_father_means_EA_230112.rda")
means_father_EA <- means


means_father$group <- "father"
means_father_EA$group <- "father_EA"

means <- rbind( means_father, means_father_EA)

meanswide <- means %>% pivot_wider (names_from = rowname, values_from = means)

head(meanswide)
write.csv2(meanswide, "means_fathers.csv")

## 3.4 Save covariances #####
load("output/full_results_siblings_total_covariances_230109.rda")
write.csv2(covariances, "covariances_main.csv")
load("output/full_results_siblings_total_covariances_EA_230109.rda")
write.csv2(covariances, "covariances_mainEA.csv")
load("output/full_results_siblings_mother_covariances_230112.rda")
write.csv2(covariances, "covariances_mother.csv")
load("output/full_results_siblings_mother_covariances_EA_230112.rda")
write.csv2(covariances, "covariances_motherEA.csv")
load("output/full_results_siblings_father_covariances_230112.rda")
write.csv2(covariances, "covariances_father.csv")
load("output/full_results_siblings_father_covariances_EA_230112.rda")
write.csv2(covariances, "covariances_fatherEA.csv")
load("output/full_results_siblings_FullMoba__covariances_230112.rda")
write.csv2(covariances, "covariances_fullMoba.csv")
load("output/full_results_siblings_fullMoba_mother_covariances_230112.rda")
write.csv2(covariances, "covariances_fullMoba_mother.csv")


# 4. Figures and tables summarizing the covariates effects #### 
load("output/full_results_siblings_total_allcov_230109.rda")
#write.csv2(full_results_siblings_total_allcov, "covariates_effects_main.csv")
plot_pop <- full_results_siblings_total_allcov
plot_pop <- plot_pop[(plot_pop$estimates == "sib_parent" | plot_pop$estimates == "sib_parent1"), ]

png("output/figure_comparison_siblings_sexparents_total.png", width= 1400, height=600)
ggplot(plot_pop, aes(x=parental_trait, y=Value, fill=model)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin= Value - 1.96 * Std.Error, ymax=Value + 1.96 * Std.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme_light()+
  ggtitle("Sex beta")+
  facet_grid(. ~ school_subject)+
  geom_hline(yintercept=0, colour="black")+
  theme(panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle=45, hjust = 1)) #+
dev.off()


plot_pop <- plot_pop[(plot_pop$estimates == "birth_yr"| plot_pop$estimates == "birth_yr1"), ]

png("output/figure_comparison_siblings_birthyear_total.png", width= 1400, height=600)
ggplot(plot_pop, aes(x=parental_trait, y=Value,fill=model)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin= Value - 1.96 * Std.Error, ymax=Value + 1.96 * Std.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme_light()+
  ggtitle("Birthyear beta")+
  facet_grid(. ~ school_subject)+
  geom_hline(yintercept=0, colour="black")+
  theme(axis.title.y=element_blank(),
        #legend.title=element_text("PRS"),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle=45, hjust = 1)) #+
dev.off()

#year of birth of mother
plot_pop <- plot_pop[(plot_pop$estimates == "mother_birthyear"| plot_pop$estimates == "mother_birthyear1"), ]

png("output/figure_comparison_siblings_motherbirthyear_total.png", width= 1400, height=600)
ggplot(plot_pop, aes(x=parental_trait, y=Value, fill=model)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin= Value - 1.96 * Std.Error, ymax=Value + 1.96 * Std.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme_light()+
  ggtitle("Mother Birthyear beta")+
  facet_grid(. ~ school_subject)+
  geom_hline(yintercept=0, colour="black")+
  theme(panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle=45, hjust = 1)) #+
dev.off()

#year of birth of father
plot_pop <- plot_pop[(plot_pop$estimates == "father_birthyear"| plot_pop$estimates == "father_birthyear1"), ]

png("output/figure_comparison_siblings_fatherbirthyear_total.png", width= 1400, height=600)
ggplot(plot_pop, aes(x=parental_trait, y=Value, fill=model)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin= Value - 1.96 * Std.Error, ymax=Value + 1.96 * Std.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme_light()+
  ggtitle("Father Birthyear beta")+
  facet_grid(. ~ school_subject)+
  geom_hline(yintercept=0, colour="black")+
  theme(panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle=45, hjust = 1)) #+
dev.off()

#sex
plot_pop <- full_results_siblings_total_allcov
plot_pop <- plot_pop[(plot_pop$estimates == "sex" | plot_pop$estimates == "sex1"), ]

png("output/figure_comparison_siblings_sex_total.png", width= 1400, height=600)
ggplot(plot_pop, aes(x=parental_trait, y=Value, fill=model)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin= Value - 1.96 * Std.Error, ymax=Value + 1.96 * Std.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme_light()+
  ggtitle("Sex beta")+
  facet_grid(. ~ school_subject)+
  geom_hline(yintercept=0, colour="black")+
  theme(panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle=45, hjust = 1)) #+
dev.off()

#save others full results to csv 
load("output/full_results_siblings_total_allcov_EA_230109.rda")
plot_pop_ea <- full_results_siblings_total_allcov
plot_pop_ea$model <- paste(plot_pop_ea$model, "_EA", sep="")
write.csv2(plot_pop_ea, "covariates_effects_mainEA.csv")

#plot education 
plot_pop <- plot_pop_ea[(plot_pop_ea$estimates == "mother_EduYears11_2018" | plot_pop_ea$estimates == "mother_EduYears11_20181"), ]

png("output/figure_comparison_siblings_motherEA_total.png", width= 1400, height=600)
ggplot(plot_pop, aes(x=parental_trait, y=Value, fill=model)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin= Value - 1.96 * Std.Error, ymax=Value + 1.96 * Std.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme_light()+
  ggtitle("mother edu beta")+
  facet_grid(. ~ school_subject)+
  geom_hline(yintercept=0, colour="black")+
  theme(panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle=45, hjust = 1)) #+
dev.off()

plot_pop <- plot_pop_ea[(plot_pop_ea$estimates == "father_EduYears11_2018" | plot_pop_ea$estimates == "father_EduYears11_20181"), ]

png("output/figure_comparison_siblings_fatherEA_total.png", width= 1400, height=600)
ggplot(plot_pop, aes(x=parental_trait, y=Value, fill=model)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin= Value - 1.96 * Std.Error, ymax=Value + 1.96 * Std.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme_light()+
  ggtitle("father edu beta")+
  facet_grid(. ~ school_subject)+
  geom_hline(yintercept=0, colour="black")+
  theme(panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size = 12, angle=45, hjust = 1)) #+
dev.off()

# in mothers 
load("output/full_results_siblings_mother_allcov_230112.rda")
write.csv2(full_results_siblings_mother_allcov, "covariates_effects_mothers.csv")

load("output/full_results_siblings_mother_allcov_EA_230112.rda")
full_results_siblings_mother_allcov$model <- paste(full_results_siblings_mother_allcov$model, "_EA", sep="")
write.csv2(full_results_siblings_mother_allcov, "covariates_effects_mothersEA.csv")


#in fathers 
load("output/full_results_siblings_father_allcov_230112.rda")
write.csv2(full_results_siblings_father_allcov, "covariates_effects_fathers.csv")

load("output/full_results_siblings_father_allcov_EA_230112.rda")
full_results_siblings_father_allcov$model <- paste(full_results_siblings_father_allcov$model, "_EA", sep="")
write.csv2(full_results_siblings_father_allcov, "covariates_effects_fathersEA.csv")

#in fullmoba 
load("output/full_results_siblings_fullMoba_allcov_230112.rda")
full_results_siblings_fullMoba_allcovall <- full_results_siblings_fullMoba_allcov
load("output/full_results_siblings_fullMoba_mother_allcov_230112.rda")
full_results_siblings_fullMoba_allcovmo <- full_results_siblings_fullMoba_allcov
full_results_siblings_fullMoba_allcov <- rbind(full_results_siblings_fullMoba_allcovall, full_results_siblings_fullMoba_allcovmo)
write.csv2(full_results_siblings_fullMoba_allcov, "covariates_effects_fullmoba.csv")



