# For phenotools package 

# author: "C.Rayner"
# contact: "christopher.rayner@kcl.ac.uk"

# -------------------------------------------------------------- #
# IF YOU USE THIS SCRIPT CHANGE ALL THE PATHS TO YOU DIRECTORIES #
# -------------------------------------------------------------- #

# cannot access external cran - redirect to TSD cran when installing packages
sink('M:/p805-perlinedd/.Rprofile')
cat('
 local({
   r = getOption("repos")
   r["CRAN"] <- "https://cran.tsd.usit.no"
   r["BIOCONDUCTOR"] <- "https://bioconductor.tsd.usit.no"
   r["BIOCONDUCTOR-annotation"] <- "https://bioconductor.tsd.usit.no/data/annotation"
   r["BIOCONDUCTOR-experiment"] <- "https://bioconductor.tsd.usit.no/data/experiment"
   r["BioC_mirror"] <- "https://bioconductor.tsd.usit.no"
   options(repos=r)
   options(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS = FALSE)
   options(download.file.method = "libcurl")
 })
 ')
sink()
##############################################
# shortcuts for paths used throughout project #
###############################################
shrd_dir="N:/data/durable/shared/"
work_dir="N:/data/durable/projects/Perline/Parental psychopathology and school performance/"
data_dir="N:/data/durable/data/moba/Original files/sav/"

######################
# set R library path #
######################
.libPaths("C:/Users/p805-perlinedd/Documents/R/win-library/4.0")
######################
# install phenotools #
######################

# the phenotools package folder has been downloaded and imported to the shared folder in p805
# if you do not have access to this folder - download and import via github
# Only install if not previously installed:

installed_phenotools <- "phenotools" %in% rownames(installed.packages())

if (installed_phenotools == FALSE) {
  install.packages(paste0(shrd_dir,"phenotools_0.2.4.zip"), repos=NULL,  type = "binary")
}

#################################
# install and load dependencies #
#################################

# List of package names

packages <- c("phenotools", 'dplyr', 'tidyr', 'haven', 'tibble', 'readr', 'pacman')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load all listed packages
invisible(lapply(packages, library, character.only = TRUE))

