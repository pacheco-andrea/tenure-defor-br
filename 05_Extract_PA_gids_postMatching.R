# ************************************************************ #
# Extract gids from Matching
# ************************************************************ #

# this script reads in .rds files from all the matches that have been conducted
# in these rds files, the vector containing info on the match is bound to the original dataframe that was used 
# (examplefile$matched is bound to original table)
# these are all unlisted and duplicates are removed in order to pass on to Ruben - and the cluster - a set of unique parcel id's to extract forest cover information on. 

# outputs:
# csv's for all temporal extents, split by biomes (for forest extractions)
library(dplyr)
library(readr)
library(foreign)

# set wd's 
# wd_main <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/"
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"

# read in csv tables that have only the gid's that were matched:
setwd(paste0(wd_main, "outputs/MatchedDatasets_onlymatches_PA"))
l <- list.files()

# get tables with only matched gid's ----
extents_tablelist <- list()
for(i in 1:length(l))
{
  # read datasets that only include matched id's
  extents_tablelist[[i]] <- read_csv(l[i], col_types = cols_only(gid = col_double(),mun = col_character(), biome = col_character())) 
}
length(extents_tablelist)
extract_here <- do.call(rbind, extents_tablelist)
extract_here <- unique(as.data.frame(extract_here))
nrow(extract_here)
unique(extract_here$biome)

# There's an error where there's an imperfect overlap in biomes between mapbiomas and imaflora
# it was producing some NA values for the comodoro municipality
# i reclassify these as belonging to cerrado - only for this extraction we need to conduct using mapbiomas
# but i stick to the original classification of the biome as per imaflora

extract_here2 <- extract_here
extract_here2$biome[which(extract_here2$mun == "COMODORO")] <- "CERRADO"

# continue as normal
extract_here2 <- split(extract_here2, extract_here2$biome)
lapply(extract_here2, head)

renamebiomes <- function(vector){
  n <- vector
  n <- gsub("AMAZÔNIA", "amazonia", n)
  n <- gsub("CAATINGA", "caatinga", n)
  n <- gsub("CERRADO", "cerrado", n)
  n <- gsub("MATA ATLÂNTICA", "mataatlantica", n)
  n <- gsub("PAMPA", "pampa", n)
  n <- gsub("PANTANAL", "pantanal", n)
  return(n)
}

mygids <- list()
for(i in 1:length(extract_here2))
{
  mygids[[i]] <- data.frame("gid" = extract_here2[[i]]$gid)
}
names(mygids) <- renamebiomes(names(extract_here2))

temporalExtents = c("1985_2018", "1985_1990", "1991_1995", "1996_1999", "2000_2004", "2005_2012", "2013_2018") # vector with temporal ranges (all chunks + total)

setwd(paste0(wd_main, "early2020jobs/yearlyChangePA/input"))
for(i in 1:length(mygids))
{
  for(j in 1:length(temporalExtents)) # paste each temporal "chunk" as part of the name
  {
    write_csv(mygids[[i]], paste0(gsub("_", "-", temporalExtents[j]), "_", names(mygids)[[i]], ".csv"))
  }
}











