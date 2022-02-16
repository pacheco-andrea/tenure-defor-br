# ************************************************************ #
# Extract gids from Matching AND from stratified rep sample
# ************************************************************ #

# outputs:
# csv's for all temporal extents, split by biomes (for forest extractions)

# libraries needed
library(dplyr)
library(readr)
library(foreign)

# set wd's 
#wd_main <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/"
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"

# get all my sample data 
setwd(paste0(wd_main, "outputs/representativeSamplesPop"))
l <- list.files()
sampleData <- list()
for(i in 1:length(l))
{
  sampleData[[i]] <- read_csv(l[i], col_types = cols_only(gid = col_double(),
                                                    mun = col_character(),
                                                    biome = col_character()))
}
sampleData <- do.call(rbind, sampleData)
uniqSampData <- unique(as.data.frame(sampleData))
nrow(uniqSampData)
# check how many per biome
unique(uniqSampData$biome)


# make sure no error for comodoro municipality

# There's an error where there's an imperfect overlap in biomes between mapbiomas and imaflora
# it was producing some NA values for the comodoro municipality
# i reclassify these as belonging to cerrado - only for this extraction we need to conduct using mapbiomas
# but i stick to the original classification of the biome as per imaflora
extract_here2 <- uniqSampData
extract_here2$biome[which(extract_here2$mun == "COMODORO")]
extract_here2$biome[which(extract_here2$mun == "COMODORO")] <- "CERRADO"

renamebiomes1 <- function(vector){
  n <- vector
  ama_all <- grep("^AMAZ", n)
  n[ama_all] <- "AMAZONIA"
  mata_all <- grep("^MATA", n)
  n[mata_all] <- "MATA ATLANTICA"
  return(n)
}
extract_here2$biome <- renamebiomes1(extract_here2$biome)
unique(extract_here2$biome)
# continue as normal
extract_here3 <- split(extract_here2, extract_here2$biome)
# lapply(extract_here3, head)
# lapply(extract_here3, nrow)

# rename to coincide with ruben's regions
renamebiomes <- function(vector){
  n <- vector
  n <- gsub("AMAZONIA", "amazonia", n)
  n <- gsub("CAATINGA", "caatinga", n)
  n <- gsub("CERRADO", "cerrado", n)
  n <- gsub("MATA ATLANTICA", "mataatlantica", n)
  n <- gsub("PAMPA", "pampa", n)
  n <- gsub("PANTANAL", "pantanal", n)
  return(n)
}



# ----
mygids <- list()
for(i in 1:length(extract_here3))
{
  mygids[[i]] <- data.frame("gid" = extract_here3[[i]]$gid)
}
names(mygids) <- renamebiomes(names(extract_here3))

temporalExtents = c("1985_2018", "1985_1990", "1991_1995", "1996_1999", "2000_2004", "2005_2012", "2013_2018") # vector with temporal ranges (all chunks + total)

setwd(paste0(wd_main, "/early2020jobs/yearlyChange/input_repSamplegids"))
for(i in 1:length(mygids))
{
  for(j in 1:length(temporalExtents)) # paste each temporal "chunk" as part of the name
  {
    write_csv(mygids[[i]], paste0(gsub("_", "-", temporalExtents[j]), "_", names(mygids)[[i]], ".csv"))
  }
}






