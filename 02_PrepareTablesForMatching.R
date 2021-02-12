# ************************************************************ #
# Prepare treatment and control tables for matching
# this script is run as a job on the HPC
# this multiplies our 49 datasets by 6(comparisons), and in turn, by 2 (other counterfactual)
# ************************************************************ #

# libraries
library(readr)
library(dplyr)
library(fastDummies)
library(tidyr)

# parameters
controlVars = c("private","public")
treatmentVars = c("private","public", "protected", "sustainable_use", "indigenous", "communal", "quilombola")


# --------------------------------------------------------------------------#

# 1. READ IN DATA ----
wdmain <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1"
wd_data_formatching <- paste0(wdmain, "/inputs/00data/for_matching/forMatchAnalysis/")
wd_out <- paste0(wdmain, "/inputs/00data/for_matching/forMatchAnalysisCEM")

# 2. Load match-analysis-ready datasets ----
# (these are parcel-level datasets for the extent of all Brazil that include joined data from Ruben's extractions of variables to be matched on)

setwd(wd_data_formatching)
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID'))
dataset <- readRDS(input[i])
n <- gsub("_allAnalysisData.rds", "", input[i])

# 3. Set up tables for matching (creating dummies and separate dataframes for each match we're making) ----
# (e.g. indigenous tenure) and control (e.g. private tenure)
datalist <- dummy_cols(dataset, select_columns = "tenure")

# we need to create a table listing for each spatial-temporal scale combination of all individual matches that have to be built, e.g. indigenous against private, etc. 

# create function to compare tenures: creates column where treatment is coded as 1, and control is coded as 0. everything else is coded as NA
# the function should also return a dataframe that keeps only the treatment and control observations (dropping all NA's)
# datalist original looked like: datalist[[i]][[j]] (i=extents)(j=data)

compareTenures <- function(datalist, control, treatment){
  
  comparison_table <- datalist[,-grep("tenure_", colnames(datalist))]
  comparison_table[,paste0(control, "_vs_", treatment)] <-  ifelse(datalist[,paste0("tenure_", treatment)] == 1,1, 
                                                                   ifelse(datalist[,paste0("tenure_", control)] == 1,0,NA ) ) # give me a column that re-codes treatment and control variables 
  comparison_table <- drop_na(comparison_table) # give me a table that keeps only those observations which I'm specifically compariing (not NA's)
  return(comparison_table)
}

# create function to apply compareTenures to all tenure forms 
# returns a table with only one column that specifies the control compared to the treatment. e.g. public_vs_private
createTable_control_vs_treatment <- function(match_list, control) {
  table_c_vs_t <- list()
  # for(i in 1:length(match_list)) # for each extent (whether that's spatial or temporal)
  # {
  for(j in 1:length(treatmentVars)) # for each tenure type (except the one you're comparing to)
  {
    if(treatmentVars[j] != control) {
      
      if(match(treatmentVars[j], gsub("tenure_", "", colnames(match_list)), nomatch = 0) != 0 ){
        
        table_c_vs_t[[length(table_c_vs_t)+1]] <- compareTenures(match_list, control, treatmentVars[j])
        names(table_c_vs_t)[length(table_c_vs_t)] <- paste0(n, "_", control, "_", treatmentVars[j])
      }
    }
  }
  # }
  return(table_c_vs_t) # this should return all dataframes needed for matching, within this control established
}

# create function to apply "createTable_control_vs_treatment" for all controls by looping through our pre-established controlVars 
loopThruControls <- function(match_extents_list,controlVars) {
  tableForMatching <- list()
  for(i in 1:length(controlVars))
  {
    tableForMatching[[i]] <- createTable_control_vs_treatment(match_extents_list, controlVars[i]) 
  }
  names(tableForMatching) <- controlVars
  return(tableForMatching)
}

mydataset <- loopThruControls(datalist, controlVars)

# write data to be matched on
setwd(wd_out)

for(i in 1:length(mydataset))
{
  for(j in 1:length(mydataset[[i]]))
  {
    write_csv(mydataset[[i]][[j]], paste0(names(mydataset[[i]][j]), ".csv"))
  }
}




