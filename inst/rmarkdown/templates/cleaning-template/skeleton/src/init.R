# loading all packages, function and list of variables.

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")

devtools::install_github('Nestor-Ch/utilityR', build_vignettes = T)

pacman::p_load(docstring, tidyverse, readxl, writexl, openxlsx, stringr, 
               sf, geosphere, qdapRegex, cluster, randomcoloR, svDialogs, scales, janitor, utilityR,zip)


make.short.name <- function(name, no_date = F){
  return(gsub("__","_", paste0(directory_dictionary$research_cycle_name,"_",directory_dictionary$round,"_",
                               name, ifelse(no_date, "", paste0("_", directory_dictionary$dctime_short)))))}

make.filename.xlsx <- function(dir = ".", name, no_date = F) return(gsub("//","/", paste0(dir, "/", make.short.name(name, no_date), ".xlsx")))



options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
###############################################################################

# load TOOL Refuggess
cat("\n- LOADING tool ...\n")

cat("\nLoading Kobo tool from file", directory_dictionary$filename.tool, "...\n")
label_colname <- utilityR::load.label.colname(directory_dictionary$filename.tool)
tool.survey  <- utilityR::load.tool.survey(directory_dictionary$filename.tool, label_colname = directory_dictionary$label_colname)
tool.choices <- utilityR::load.tool.choices(directory_dictionary$filename.tool, label_colname = directory_dictionary$label_colname)

cat("..OK\n")
