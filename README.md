# No effect of parental mental health on children’s academic achievement: a within-family study 

Scripts used for the analyses of the upcoming manuscript "No effect of parental mental health on children’s academic achievement: a within-family study". 


- 01_identify_siblings_children.R: identify siblings and children of siblings in the Norwegian registry 
- 02_01_get_school_performance.R: prepare data on standardized test scores
- 02_02_educational_attainment.R: prepare data on educational attainment of the parents
- 03_get_Moba_mental_health.R: prepare data on parental mental health symptoms. 
- 04_merge_siblings_moba.R: merge datasets with pedigree information, parental mental health and education and children’s standardized test scores
- 05_run_analyses.R: run all analyses
- 06_figures_fdr_tables.R: create figures, prepare and clean tables for manuscript and SI. 
- Functions.R: functions created to run analyses 
- A1_profile_paths_and_packages.R: setting up from phenotools, https://github.com/psychgen/phenotools
- A2_initalise_project.R: setting up from phenotools, https://github.com/psychgen/phenotools
- IRT.do: create IRT scores of the eating disorder symptoms 
- getting_results.do: print parameters and characteristic functions for the IRT scores
