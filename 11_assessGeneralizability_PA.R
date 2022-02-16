# ************************************************************ #
# assess generalizability of each model
# ************************************************************ ## 

# 3 outputs:
# generalizability scores
# ASMD tables
# trimmed population

# libraries i need
# upon first installation:
# library(devtools)
#setwd("/gpfs0/home/pacheco/R/")
# withr::with_libpaths(new = "/gpfs0/home/pacheco/R/", # i had to create this, i suspect this is what was deleted before
#                      install_github('benjamin-ackerman/generalize')) # this could be a list, e.g. install_github('rCharts', 'ramnathv'))
#library("generalize", lib.loc = "/gpfs0/home/pacheco/R")
library(generalize)
library(dplyr)

##### run job ----
# main dir
wdmain <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1"

setwd(paste0(wdmain, "/code")) # job will be for every matched dataset
source("16a_myGeneralizeFunct.R")

# set up job
setwd(paste0(wdmain, "/outputs/toAssessGeneralizability_1_PA"))
input <- list.files()
i=as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID')) # check how many (553?)
print(i)
myData <- read.csv(input[i])
name <- gsub(".csv", "", input[i])
print(name)

# assess generalizability ----
# assess and estimate the trial participation probabilities 

# establish covariates
covs <- c("elevation", "slope", "travel_time", "pop", "area")
myData$trial <- as.integer(myData$matched)

# assess generalizability with trimmed population 

assess_object <- try(myAssess(trial = "trial",
                        selection_covariates = covs,
                        data = myData,
                        selection_method = "lasso",
                        sl_library = NULL, 
                        survey_weights = FALSE, 
                        trim_weights = FALSE,
                        trim_pctile = 0.97,
                        is_data_disjoint = TRUE,
                        seed = 13783,
                        trimpop = TRUE))

# assess generalizability with UNtrimmed population 
assess_object2 <- try(myAssess(trial = "trial",
                               selection_covariates = covs,
                               data = myData,
                               selection_method = "lasso",
                               sl_library = NULL, 
                               survey_weights = FALSE, 
                               trim_weights = FALSE,
                               trim_pctile = 0.97,
                               is_data_disjoint = TRUE,
                               seed = 13783,
                               trimpop = F))

#summary(assess_object)

# get trimmed data ----
trim_pop <- trim_pop(trial="trial",
            selection_covariates=covs,
            data=myData)
trimmed_data <- trim_pop$trimmed_data


# add weights to trimmed data
if(class(assess_object) == "generalize_assess"){
  trimmed_data$weights <- assess_object$weights
}else{trimmed_data$weights <- NA}

# outputs ----

setwd(paste0(wdmain, "/outputs/toTATE_PA"))
write.csv(trimmed_data, paste0(name, ".csv"), row.names = F)

# write out g_index for all models in separate folder
setwd(paste0(wdmain, "/outputs/g_index_PA"))

g <- data.frame("stScale"=name, 
                "g_indexT"=NA,
                "g_indexF"=NA,
                "percExcl"=NA)
# write over if assessment worked
if(class(assess_object) == "generalize_assess"){
  g$g_indexT <- assess_object$g_index
  g$percExcl <- assess_object$n_excluded/nrow(myData[which(myData$trial==0),])
}
if(class(assess_object2) == "generalize_assess"){
  g$g_indexF <- assess_object2$g_index
}
write.csv(g, paste0(name, ".csv"), row.names = F)

# write out trim_pop: the idea would be to get only observations that were trimmed out of final dataset 
# and get average stats for both of these 
setwd(paste0(wdmain, "/outputs/trim_pop_PA"))
saveRDS(trim_pop, paste0(name, ".rds"))


