# ************************************************************ #
# Prep all different data sources to run generalize analysis on the cluster
# ************************************************************ #


# library i need
library(dplyr)

##### run job ----
# main dir
wdmain <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1"

# set up job
setwd(paste0(wdmain, "/outputs/MatchedDatasets/")) # job will be for every matched dataset
input <- list.files()
input_no <- grep("try-error", input)
input2 <- input[-input_no]
i=as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID')) # will now be 553 FILES!
#i=537
print(i)

# data ----
# get vector of matches
mdata <- readRDS(input2[i])
name <- gsub(".rds", "", input2[i])
print(name)
# get covariate data with all the population
setwd(paste0(wdmain, "/inputs/00_data/for_matching/forMatchAnalysisCEM"))
popdatalist <- list.files()
popdata <- read.csv(popdatalist[-input_no][i])
popname <- gsub(".csv", "", popdatalist[i])
# establish matching covariates
covs <- c(colnames(popdata[2:5]), "area") # this includes population
# check same
# nrow(popdata)
# length(mdata$matched)
# put together data
mydata <- popdata
mydata$matched <- mdata$matched
mydata$trial <- as.integer(mydata$matched)



# step 2: ----

# need to bind response information first for both rep samples and matched
# because i need to make sure that i have the forest information for matched gid's (minimum)
# plus the representative sample
# this is not (necessarily/probably) == nrow(mydata), because this is just the total population at that scale
# so: 

# 2.1 first get representative sample forest data extractions
setwd(paste0(wdmain, "/early2020jobs/yearlyChange/output_repSamplegids"))
f <- list.files()
# what temporal extent are we using?
yrmatched <- gsub("([^_]*_[^_]*)_(.*)","\\1", input2[i])
# find all data that corresponds to this temporal extent
yrposition <- grep(yrmatched, gsub("-", "_", gsub(".csv", "", f))) 
yrforestdata <- lapply(f[yrposition], read.csv)
yrforestdata <- do.call(rbind, yrforestdata)


# matched forest data extractions
setwd(paste0(wdmain, "/early2020jobs/yearlyChange/output")) # i think this should only be original output(it's the matched gid's extraction)
mf <- list.files()
# what temporal extent are we using?
myrmatched <- gsub("([^_]*_[^_]*)_(.*)","\\1", input2[i])
# find all data that corresponds to this temporal extent
myrposition <- grep(myrmatched, gsub("-", "_", gsub(".csv", "", mf))) 
myrforestdata <- lapply(mf[myrposition], read.csv)
myrforestdata <- do.call(rbind, myrforestdata)

# bind both forest data together
forestData <- unique(rbind(yrforestdata, myrforestdata))

# create response df
respData <- data.frame("gid"=forestData$gid, "for_orig"=forestData[,2], "for_ch"=forestData[,6])


# join all my data ----

# trim forest data to ONLY INCLUDE the 
# 1) unmatched data for the specific comparison analyzed here e.g. public_private
setwd(paste0(wdmain, "/outputs/representativeSamplesPop"))
r <- list.files()
# gsub(".rds", "", input2) == gsub(".csv", "", r) all same, so, 
vsData_repsamp <- read.csv(r[i])

# 2) matched data for the specific comparison
setwd(paste0(wdmain, "/outputs/MatchedDatasets_onlymatches"))
m <- list.files()
# gsub(".rds", "", input2) == gsub(".csv", "", m) all same, so, 
vsData_matched <- read.csv(m[i])


# precise gid's i need for this specific spatio-temporal comparison:
mygids <- data.frame("gid"=unique(c(vsData_repsamp$gid, vsData_matched$gid)))
data <- inner_join(mygids, mydata, by="gid")
myData <- inner_join(data, respData, by="gid") 

# deal with rare extraction ERRORS
myData$for_orig[which(myData$for_orig < 0)] <- NA
myData$for_ch[which(myData$for_ch < 0)] <- NA


# create outcome var (as a proportion!)
myData$outcome <- ((myData$for_orig-myData$for_ch)/myData$for_orig)
myData$outcome[which(myData$outcome == "NaN")] <- 0 # fix proportions which divide 0/0
# reference treatment column
treatment <- grep("_vs", colnames(myData))
#summary(myData)

# population column (to simplify later steps)
myData$pop <- myData[,5]

### write out these data
setwd(paste0(wdmain, "/outputs/toAssessGeneralizability_1"))
write.csv(myData, paste0(name, ".csv"), row.names=F)




















