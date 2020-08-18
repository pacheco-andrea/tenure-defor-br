# ************************************************************ #
# Extract gids from Matching
# ************************************************************ #
setwd("~/data/MAS-group-share/04_personal/Andrea/P1_analysis/code/")
source("00_Setting_Parameters.R")
# this script reads in files from all the matches that have been conducted
# in these files, the vector containing info on the match (T/F) is bound to the original dataframe that was used for matching
# these are all unlisted and duplicates are removed in order to pass on to the cluster a set of unique parcel id's to extract forest cover information 
# outputs:
  # csv's for all temporal extents, split by biomes (for cluster)
  # csv's of all tables to be used for statistical analysis

# set wd's 
wd_tables <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/for_matching/tables_formatching/"
wd_matches <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/matches/"
wd_ids_out <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/01_matched/ids"
wd_ids_join <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/01_matched/gids_toJoin/"
wd_shp <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/shp/"


# extract matches ----
get_onlygids <- function(extents) {
  
  setwd(paste0(wd_tables, extents))
  a <- list.files()
  # gets only matched gids
  extents_tablelist <- list()
  for(i in 1:length(a))
  {
    setwd(paste0(wd_tables, extents))
    tb <- read_csv(a[i] , col_types = cols_only(gid = col_double(), biome = col_character())) 
    setwd(paste0(wd_matches, extents, "/"))
    b <- list.files()
    a2 <- gsub( ".csv", ".rds", a)
    if(file.exists(a2[i])){
      m_tmp <- readRDS(b[grep(paste0(a2[i]), b)]) # read the match that corresponds with the name in the dataframe csv's 
      if(class(m_tmp) == "cem.match"){ 
        extents_tablelist[[i]] <- data.frame("gid" = tb$gid, "matched"= m_tmp$matched, "biome" = tb$biome) # join these into a dataframe
        extents_tablelist[[i]] <- extents_tablelist[[i]][which(extents_tablelist[[i]]$matched == TRUE),] # keep only those observations which were matched
      }
      else(extents_tablelist[[i]] <- "failed match (probably due to too few observations)")
    }
    else{extents_tablelist[[i]] <- "match not yet completed"}
  }
  names(extents_tablelist) <- gsub(".csv", "", a) 
  return(extents_tablelist)
}

# get gids and remove duplicates for all extents we're working with (spatial, temporal, and all extents) ----
sp <- get_onlygids("spatialExtents")
sp2 <- do.call("c", lapply(1:length(sp), function(i) {
  a = as.data.frame(sp[[i]])
  a$gid
}))

tempo <- get_onlygids("temporalExtents")
tempo2 <- do.call("c", lapply(1:length(tempo), function(i) {
  a = as.data.frame(tempo[[i]])
  a$gid
}))

all <- get_onlygids("allExtents")
all2 <- do.call("c", lapply(1:length(all), function(i) {
  a = as.data.frame(all[[i]])
  a$gid
}))

gids <- unique(c(unique(sp2), unique(tempo2), unique(all2))) # first version
length(gids)


# join with biomes and write ----
# (because extractions will be conducted on per biome basis)
# read original data with biome information
setwd(wd_shp)
a <- list.files()
b <- grep(".dbf", a)
c <- lapply(a[b], read.dbf)
names <- a[b]
names <- gsub(".dbf", "", names)
names(c) <- names
# create biome column using the biome names
for(i in 1:length(c))
{
  c[[i]]$biome <- names(c)[i]
}
lapply(c, summary)
d <- bind_rows(c)
nrow(d)
head(d)
summary(d)
d$gid <- as.numeric(d$gid)

gids2 <- data.frame("gid" = gids)
nrow(gids2)
# join df with unique gid's to biome information
extract_here <- inner_join(gids2, d, by = "gid") 
summary(extract_here) 
# split it by biome
extract_here2 <- split(extract_here, extract_here$biome)
lapply(extract_here2, nrow) # check number of unique matches per biome
# create data frames for each biome containing only the unique gid
mygids <- list()
for(i in 1:length(extract_here2))
{
  mygids[[i]] <- data.frame("gid" = extract_here2[[i]]$gid)
}
names(mygids) <- names(extract_here2)
# write dfs
setwd(wd_ids_out)
for(i in 1:length(mygids))
{
  for(j in 1:length(temporalExtents)) # paste each temporal "chunk" as part of the name
  {
    write_csv(mygids[[i]], paste0(temporalExtents[j], "_", names(mygids)[[i]], ".csv"))
  }
}

# matched tables ----
# write csvs of all gid tables: these will eventually be joined to forest extractions used in stats analysis

write_myTables <- function(list, extent)
{
  setwd(wd_ids_join)
  dir.create(extent)
  setwd(paste0(wd_ids_join, extent))
  for(i in 1:length(list))
  {
    if(class(list[[i]]) == "data.frame"){
      write_csv(as.data.frame(list[[i]]), paste0(names(list)[[i]], ".csv"))
    }
  }
}
# needed to manually rename to deal with encoding issues on HPC consistently
renamebiomes <- function(list){
  n <- names(list)
  n <- gsub("AMAZÔNIA", "Amazonia", n)
  n <- gsub("CAATINGA", "Caatinga", n)
  n <- gsub("CERRADO", "Cerrado", n)
  n <- gsub("MATA ATLÂNTICA", "MataAtlantica", n)
  n <- gsub("PAMPA", "Pampa", n)
  n <- gsub("PANTANAL", "Pantanal", n)
  names(list) <- n
  return(list)
} 

sp <- renamebiomes(sp)
all <- renamebiomes(all)

write_myTables(sp, "spatialExtents")
write_myTables(tempo, "temporalExtents")
write_myTables(all, "allExtents")











