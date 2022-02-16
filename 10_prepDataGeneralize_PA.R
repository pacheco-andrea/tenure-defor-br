# ************************************************************ #
# Prep all different data sources to run generalize analysis on the cluster
# ************************************************************ #


# library i need
library(dplyr)

wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"



# 1. set up job as the amount of successful representative samples (easiest)
setwd(paste0(wd_main, "outputs/representativeSamplesPop_PA"))
f <- list.files()
i=as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID')) # set job as 1-174
f2 <- gsub("csv", "rds",f)

# 2. read match
setwd(paste0(wd_main, "outputs/MatchedDatasets_PA"))
m <- readRDS(f2[i])
print(f2[i])
print(i)
name <- gsub(".rds", "", f2[i])  

# 3. get tables with all unmatched gid's to bind the match vector as a column
setwd(paste0(wd_main, "outputs/MatchedPADatasets_yearCreatedSubset"))
df <- read.csv(gsub(".rds", "_YrSubset.csv", f2[i])) # ensure get match that corresponds
# bind  match vector as column in original dataframe
df$matched <- m$matched 

# 4. this dataframe should then be pruned to only contain observations that were 1) matched, or 2) part of the representative sample
mdata <- df[which(df$matched == TRUE),]
# get representative sample
setwd(paste0(wd_main, "outputs/representativeSamplesPop_PA"))
repdata <- read.csv(gsub(".rds", ".csv", f2[i]))

mydata <- rbind(mdata, repdata)


# 5. bind response var data for both rep samples and matches
# rep samples forest data
setwd(paste0(wd_main, "/early2020jobs/yearlyChange/output_repSamplegids"))
f <- list.files()
# what temporal extent are we using?
yrmatched <- gsub("([^_]*_[^_]*)_(.*)","\\1", f2[i])
# find all data that corresponds to this temporal extent
yrposition <- grep(yrmatched, gsub("-", "_", gsub(".csv", "", f))) 
yrforestdata <- lapply(f[yrposition], read.csv)
yrforestdata <- do.call(rbind, yrforestdata)
# matched samples forest data
setwd(paste0(wd_main, "/early2020jobs/yearlyChange/output")) # i think this should only be original output(it's the matched gid's extraction)
mf <- list.files()
# what temporal extent are we using?
myrmatched <- gsub("([^_]*_[^_]*)_(.*)","\\1", f2[i])
# find all data that corresponds to this temporal extent
myrposition <- grep(myrmatched, gsub("-", "_", gsub(".csv", "", mf))) 
myrforestdata <- lapply(mf[myrposition], read.csv)
myrforestdata <- do.call(rbind, myrforestdata)

forestData <- unique(rbind(yrforestdata, myrforestdata))

# create response df
respData <- data.frame("gid"=forestData$gid, "for_orig"=forestData[,2], "for_ch"=forestData[,6])


# join all my data ----

myData <- left_join(mydata, respData, by="gid")

# deal with rare extraction ERRORS
myData$for_orig[which(myData$for_orig < 0)] <- NA
myData$for_ch[which(myData$for_ch < 0)] <- NA

# create outcome var (as a proportion!)
myData$outcome <- ((myData$for_orig-myData$for_ch)/myData$for_orig)
myData$outcome[which(myData$outcome == "NaN")] <- 0 # fix proportions which divide 0/0

# population column (to simplify later steps)
myData$pop <- myData[,6]

### write out these data
setwd(paste0(wd_main, "/outputs/toAssessGeneralizability_1_PA"))
write.csv(myData, paste0(name, ".csv"), row.names=F)

