# ************************************************************ #
# Prepare data for statisical analysis post forest cover extractions
# ************************************************************ #
# This script binds fixed and non-fixed covariate data to ONLY the observations which have been matched for each unique spatio-temporal extent.
# it relies on forest cover extractions that were preprocessed on the HPC (python script)
# output: tables for each glm that should be run.
setwd("/data/MAS-group-share/04_personal/Andrea/P1_analysis/code/")
source("00_Setting_Parameters.R")

# set wd's 
wd_ids_join <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/01_matched/gids_toJoin/"
wd_env_vars <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/for_matching/tables_formatching/"
wd_ext <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/01_matched/extractions/"
wd_out <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/01_matched/gids_toAnalyze/"

# function to rename biomes to one standard
renamebiomes2 <- function(list){
  n <- list
  n <- gsub("AMAZÔNIA", "Amazonia", n)
  n <- gsub("CAATINGA", "Caatinga", n)
  n <- gsub("CERRADO", "Cerrado", n)
  n <- gsub("MATA ATLÂNTICA", "MataAtlantica", n)
  n <- gsub("PAMPA", "Pampa", n)
  n <- gsub("PANTANAL", "Pantanal", n)
  list <- n
  return(list)
} 

# 1. spatial extents ----

# read in csv tables that have only the gid's that were matched:
setwd(wd_ids_join)
setwd(paste0(wd_ids_join, "spatialExtents"))
l <- list.files()
sp <- lapply(l, read_csv)
names(sp) <- gsub(".csv", "", l)
# read in environmental covariates:
setwd(paste0(wd_env_vars, "spatialExtents"))
spcov <- list.files()
spcov2 <- renamebiomes2(spcov) # rename biomes in order to be able to match with other files
# read in response variable
setwd(wd_ext)
f <- list.files()
f_sp_position <- grep(gsub("_", "-", temporalExtents[1]), f) # for spatial extents this will always be the first (global) temporal extent. i.e. 1985-2018, all years.
f_sp <- lapply(f[f_sp_position], read_csv)
names(f_sp) <- gsub(".csv", "", f[f_sp_position])
lapply(f_sp, summary)

for(i in 1:length(sp)) # for all spatial matches bind all covariate and response data
{
  # find the covariate file that matches it:
  myfileposition <- match(names(sp[i]), gsub(".csv", "", spcov2)) 
  # read the corresponding table
  setwd(paste0(wd_env_vars, "spatialExtents"))
  mytable <- read_csv(spcov[myfileposition], col_types = cols(biome = col_skip()))
  # do an inner join to only keep those obs that were matched
  sp[[i]] <- inner_join(sp[[i]], mytable, by = "gid")
  colnames(sp[[i]])[grep("[0-9]", colnames(sp[[i]]))]  <- "pop"
  
  # add response info: 
  setwd(wd_ext)
  # find the spatial extent that corresponds. i.e. which biome?
  mybiome <- gsub("(^[^_]*)_(.*$)","\\1", names(sp[i])) # get everything that is not an underscore, until it finds underscore, and the rest is the second (). give me the fist ()
  # find the corresponding biome
  f_sp_biomeposition <- grep(mybiome, names(f_sp), ignore.case = T)
  # create table of corresponding biome
  myresponse <- data.table("gid" = f_sp[[f_sp_biomeposition]]$gid, "for_orig" = f_sp[[f_sp_biomeposition]]$f_1985, "for_ch" = f_sp[[f_sp_biomeposition]]$`f-a_1985-2018`)
  # bind with rest
  sp[[i]] <- left_join(sp[[i]], myresponse, by = "gid")
  sp[[i]] <- sp[[i]][which(sp[[i]]$for_orig >= 0),]
  sp[[i]] <- drop_na(sp[[i]])
}
lapply(sp, summary)
length(sp)


for(i in 1:length(sp)) # write data tables for all spatial extents
{
  if(nrow(sp[[i]]) > 0){
    setwd(paste0(wd_out, "spatialExtents"))
    write_csv(sp[[i]], paste0(names(sp[i]), ".csv"))
  }
}
rm(sp)

# 2. temporal extents ----

setwd(paste0(wd_ids_join, "temporalExtents"))
l <- list.files()
tem <- lapply(l, read_csv)
names(tem) <- gsub(".csv", "", l)

setwd(paste0(wd_env_vars, "temporalExtents"))
tcov <- list.files()

for(i in 1:length(tem)) # for all temporal matches bind all covariate and response data
{
  # find the covariate file that matches it (this includes the year info):
  myfileposition <- match(names(tem[i]), gsub(".csv", "", tcov)) 
  # read the corresponding table
  setwd(paste0(wd_env_vars, "temporalExtents"))
  mytable <- read_csv(tcov[myfileposition], col_types = cols(biome = col_skip())) # skip the biome, we already have that
  # do an inner join to only keep those obs that were matched
  tem[[i]] <- inner_join(tem[[i]], mytable, by = "gid")
  colnames(tem[[i]])[grep("[0-9]", colnames(tem[[i]]))]  <- "pop"
  
  # add response info: 
  setwd(wd_ext)
  f <- list.files()
  # find corresponding year in response files 
  myyear <- gsub("([^_]*_[^_]*)_(.*)","\\1", names(tem[i]))
  myyear <- gsub("_", "-", myyear)
  f_tem_yearposition <- grep(myyear, f)
  f_tem <- lapply(f[f_tem_yearposition], read_csv) # there are six matches for these years (one for each biome), so i want to bind all of these in one (for all brazil)
  f_tem <- bind_rows(f_tem)
  
  # create table of corresponding biome
  myresponse <- data.table("gid" = f_tem$gid, "for_orig" = f_tem[,2], "for_ch" = f_tem[,6])
  colnames(myresponse) <- c("gid", "for_orig", "for_ch") # had to coerce because of column number above, I think. 
  # bind with rest
  tem[[i]] <- left_join(tem[[i]], myresponse, by = "gid")
  tem[[i]] <- tem[[i]][which(tem[[i]]$for_ch >= 0),] # only keep obs for which extractions were able to be conducted (i.e. not -1)
  tem[[i]] <- drop_na(tem[[i]])
}
length(l)
length(tem)
lapply(tem, summary)


for(i in 1:length(tem))
{
  if(nrow(tem[[i]]) > 0){
    setwd(paste0(wd_out, "temporalExtents"))
    write_csv(tem[[i]], paste0(names(tem[i]), ".csv"))
  }
} 
rm(tem)

# 3. all extents ----

# read in csv tables that have only the gid's that were matched:
setwd(paste0(wd_ids_join, "allExtents"))
l <- list.files() # NOTE: these are now only 469 (not 504) because we lose several comparisons to the non-existent lands in pampa and pantanal (communal and sustainable use)
all <- lapply(l, read_csv)
names(all) <- gsub(".csv", "", l)

# read in environmental covariates:
setwd(paste0(wd_env_vars, "allExtents"))
acov <- list.files()
acov2 <- renamebiomes2(acov)

#read in response:
setwd(wd_ext)
f <- list.files()


for(i in 1:length(all)) # for all spatial matches
{
  # find the covariate file that matches "all":
  myfileposition <- match(names(all[i]), gsub(".csv", "", acov2)) 
  # read the corresponding table
  setwd(paste0(wd_env_vars, "allExtents"))
  mytable <- read_csv(acov[myfileposition], col_types = cols(biome = col_skip()))
  # do an inner join to only keep those obs that were matched
  all[[i]] <- inner_join(all[[i]], mytable, by = "gid")
  colnames(all[[i]])[grep("[0-9]", colnames(all[[i]]))] <- "pop"
  
  # add response info:
  setwd(wd_ext)
  # find the spatial and temporal extent that corresponds: i.e. which biome? and which year?
  f <- list.files()
  mybiome_year <- gsub("([^_]*_[^_]*_[^_]*)_(.*)","\\1", names(all[i])) 
  mybiome <- gsub("[^a-z]", "",mybiome_year, ignore.case = T )
  myyear <- gsub("([^_]*_[^_]*)_(.*)","\\1", names(all[i]))
  myyear <- gsub("_", "-", myyear)
  mybiome_year <- paste0(mybiome, "_", myyear)
  mybiome_year_position <- grep(mybiome_year, f, ignore.case = T)
  f_all <- read_csv(f[mybiome_year_position])
  myresponse <- data.table("gid" = f_all$gid, "for_orig" = f_all[,2], "for_ch" = f_all[,6])
  colnames(myresponse) <- c("gid", "for_orig", "for_ch")
  
  all[[i]] <- left_join(all[[i]], myresponse, by = "gid")
  all[[i]] <- nrow(all[[i]][which(all[[i]]$for_ch >= 0),])
  all[[i]] <- drop_na(all[[i]])
}

lapply(all, summary)

for(i in 1:length(all))
{
  if(nrow(all[[i]]) > 0){ # if there is even a table, 
    setwd(paste0(wd_out, "allExtents"))
    write_csv(all[[i]], paste0(names(all[i]), ".csv"))
  }
}



