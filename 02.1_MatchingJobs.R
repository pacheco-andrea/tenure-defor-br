# ************************************************************ #
# Script for conducting Coarsened Exact Matching (CEM) 
# ************************************************************ #

# this script runs through the process of matching and exporting matched datasets for all of Brazil
# because certain datasets were very large and computationally intensive, matches were carried out on the HPC

setwd("~/data/MAS-group-share/04_personal/Andrea/P1_analysis/code/")
source("00_Setting_Parameters.R")


# CEM:
# For our study we have two "controls", as we compare all tenure forms to 1) Public and 2) Private lands. As established in the parameters script:
controlVars
# We also have 6 "treatments":
treatmentVars

# set working directories
wd_main <- "~/data/MAS-group-share/04_personal/Andrea/P1_analysis/"
wd_data_formatching <- ("~/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/for_matching/")


# Load match-analysis-ready datasets ----
setwd(wd_data_formatching)
match_spaceExtents <- readRDS("forMatchAnalysis/allAnalysisData_spatialextents_1985_2018.rds") # disaggregation over spatial extents
match_tempExtents <- readRDS("forMatchAnalysis/allAnalysisData_brazil_temporalextents.rds") # disaggregation over temporal extents
match_allExtents <- readRDS("forMatchAnalysis/allAnalysisData_spatialextents_temporalextents.rds") # disaggregation over both

# Set up tables for matching ----
# create dummies and separate dataframes for each match we're making (e.g. indigenous tenure) and control (e.g. private tenure)

createTenureDummies <- function(datalist){
  if(names(datalist[[1]][1]) == "gid"){
    for(i in 1:length(datalist))
    {
      datalist[[i]] <- dummy_cols(datalist[[i]], select_columns = "tenure")
    }
  }else{
    for(i in 1:length(datalist)) # for all temporal extents
    {
      for(j in 1:length(datalist[[i]]))
      {
        datalist[[i]][[j]] <- dummy_cols(datalist[[i]][[j]], select_columns = "tenure")
      }
    }
  }
  return(datalist)
} # has an argument for simple lists and one for nested lists. but this might not be necessary. 

# we create tenure dummies for each spatial and temporal extent in order to keep results separate in these 3 objects.
# these objects will be carried through the rest of the analysis
match_tempExtents <- createTenureDummies(match_tempExtents)
match_spaceExtents <- createTenureDummies(match_spaceExtents)
match_allExtents <- createTenureDummies(match_allExtents)

# create function to compare tenures: creates column where treatment is coded as 1, and control is coded as 0. everything else is coded as NA
compareTenures <- function(datalist, control, treatment){
  
  comparison_table <- datalist[,-grep("tenure_", colnames(datalist))]
  comparison_table[,paste0(control, "_vs_", treatment)] <-  ifelse(datalist[,paste0("tenure_", treatment)] == 1,1, 
                                                                   ifelse(datalist[,paste0("tenure_", control)] == 1,0,NA ) )
  comparison_table <- drop_na(comparison_table) 
  return(comparison_table) # returns a dataframe that keeps only the treatment and control observations (dropping all NA's)

}

# create function to apply compareTenures to all tenure forms 
# returns a table with column that specifies the control compared to the treatment. e.g. public_vs_private
createTable_control_vs_treatment <- function(match_list, control) {
  table_c_vs_t <- list()
  for(i in 1:length(match_list)) # for each extent (whether that's spatial or temporal)
  {
    for(j in 1:length(treatmentVars)) # for each tenure type (except the one you're comparing to)
    {
      if(treatmentVars[j] != control) {
        
        if(match(treatmentVars[j], gsub("tenure_", "", colnames(match_list[[i]])), nomatch = 0) != 0 ){
          
          table_c_vs_t[[length(table_c_vs_t)+1]] <- compareTenures(match_list[[i]], control, treatmentVars[j])
          names(table_c_vs_t)[length(table_c_vs_t)] <- paste0(names(match_list[i]),"_", control, "_", treatmentVars[j])
        }
      }
    }
  }
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


# apply these functions to the three objects that indicate three extents, and write as csvs in order to reference them as individual jobs on the HPC
spatialExtents_tablesforMatching <- loopThruControls(match_spaceExtents, controlVars)# this returns 2 lists of 34 dataframes that have the column comparisons of all tenure forms (6) vs all controls (2), for all biomes (6) = 72. we lose two comparisons because there are no communal lands in the Pampa

setwd(paste0(wd_data_formatching, "tables_formatching/spatialExtents"))

for(i in 1:length(spatialExtents_tablesforMatching))
{
  for(j in 1:length(spatialExtents_tablesforMatching[[i]]))
  {
    write_csv(spatialExtents_tablesforMatching[[i]][[j]], paste0(names(spatialExtents_tablesforMatching[[i]][j]), ".csv"))
    print(names(spatialExtents_tablesforMatching[[i]][j]))
  }
}
rm(spatialExtents_tablesforMatching)

temporalExtents_tablesforMatching <- loopThruControls(match_tempExtents, controlVars) # this returns 2 lists of 42 dataframes. 7 temporal extents * 6 tenure forms = 42. 
setwd(paste0(wd_data_formatching, "tables_formatching/temporalExtents"))

for(i in 1:length(temporalExtents_tablesforMatching))
{
  for(j in 1:length(temporalExtents_tablesforMatching[[i]]))
  {
    write_csv(temporalExtents_tablesforMatching[[i]][[j]], paste0(names(temporalExtents_tablesforMatching[[i]][j]), ".csv"))
    print(names(temporalExtents_tablesforMatching[[i]][j]))
  }
}
rm(temporalExtents_tablesforMatching)

allExtents_tablesforMatching <- list()
for(i in 1:length(match_allExtents)) 
{
  allExtents_tablesforMatching[[i]] <- loopThruControls(match_allExtents[[i]], controlVars) 
}
names(allExtents_tablesforMatching) <- names(match_allExtents)
setwd(paste0(wd_data_formatching, "tables_formatching/allExtents"))
for(i in 1:length(allExtents_tablesforMatching))
{
  for(j in 1:length(allExtents_tablesforMatching[[i]]))
  {
    for(k in 1:length(allExtents_tablesforMatching[[i]][[j]]))
    {
      write_csv(allExtents_tablesforMatching[[i]][[j]][[k]], paste0(names(allExtents_tablesforMatching[i]), "_", names(allExtents_tablesforMatching[[i]][[j]][k]), ".csv"))
      print(paste0(names(allExtents_tablesforMatching[i]), "_", names(allExtents_tablesforMatching[[i]][[j]][k])))
    }
  }
}
rm(allExtents_tablesforMatching)