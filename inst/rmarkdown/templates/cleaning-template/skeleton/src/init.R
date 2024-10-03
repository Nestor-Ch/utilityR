# loading all packages, function and list of variables.

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")

devtools::install_github('https://github.com/REACH-WoU/utilityR', build_vignettes = T)
devtools::install_github('https://github.com/REACH-WoU-Regional/translateR', build_vignettes = T)
devtools::install_github('https://github.com/cynkra/dm')
devtools::install_github('https://github.com/dickoa/robotoolbox')

pacman::p_load(docstring, tidyverse, readxl, writexl, openxlsx, stringr, markovchain,
               sf, geosphere, qdapRegex, cluster, randomcoloR, svDialogs, scales, janitor, utilityR,zip,geosphere,
               translateR,robotoolbox)


make.short.name <- function(name, no_date = F){
  return(gsub("__","_", paste0(directory_dictionary$research_cycle_name,"_",directory_dictionary$round,"_",
                               name, ifelse(no_date, "", paste0("_", directory_dictionary$dctime_short,'_',
                                                                format(Sys.time(), "%m_%d_%H_%M"))))))}

make.filename.xlsx <- function(dir = ".", name, no_date = F) return(gsub("//","/", paste0(dir, "/", make.short.name(name, no_date), ".xlsx")))



options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
###############################################################################

