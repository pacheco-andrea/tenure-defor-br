# ************************************************************ #
# Prepare data for Statisical Analysis 
# this happens after matching and calculating imbalance metrics
# ************************************************************ #

# This script binds fixed and non-fixed covariate data to ONLY the observations which have been matched for each unique spatio-temporal extent.
# output: tables for each glm that should be run.
#setwd("/data/MAS-group-share/04_personal/Andrea/P1_analysis/code/")
#source("00_Setting_Parameters.R")
library(dplyr)
# set wd's 
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"

wd_ext <- paste0(wd_main, "early2020jobs/yearlyChange/output")
wd_ext2 <- paste0(wd_main, "early2020jobs/yearlyChange/output_pixcounts")
wd_out <- paste0(wd_main, "inputs/01_matched/forStatsAnalysis")

# read in csv tables that have only the gid's that were matched:
setwd(paste0(wd_main, "outputs/MatchedDatasets_onlymatches"))
l <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID')) # 553

matched <- read.csv(l[i])

# read in response variable
setwd(wd_ext)
f <- list.files()
# what temporal extent are we using?
yrmatched <- gsub("([^_]*_[^_]*)_(.*)","\\1", l[i])
# find all data that corresponds to this temporal extent
yrposition <- grep(yrmatched, gsub("-", "_", gsub(".csv", "", f))) 
# read
yrforestdata <- lapply(f[yrposition], read.csv)
# bind together all biome data for the year in question
yrforestdata <- do.call(rbind, yrforestdata)
yrf2agdata <- data.frame("gid"=yrforestdata$gid, "for_orig"=yrforestdata[,2], "for_ch"=yrforestdata[,6])

# read in area in pixels info
setwd(wd_ext2)
f <- list.files()
# read
areaData <- lapply(f, read.csv)
# bind together
areaData <- do.call(rbind, areaData)
areaData <- distinct(data.frame("gid"=areaData$gid, "count"=areaData$count))


# join all my data
mydata1 <- inner_join(matched, areaData, by="gid")
mydata <- inner_join(mydata1, yrf2agdata, by= "gid")
nrow(matched)
nrow(mydata)

# write out
setwd(wd_out)
if(nrow(matched)!=nrow(mydata)){
  write.csv(mydata, paste0(gsub(".csv", "_PROBLEM.csv",l[i]))) # problem would indicate we're missing extractions for certain obs
}else{write.csv(mydata, paste0(l[i]))}

