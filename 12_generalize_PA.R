# ************************************************************ #
# Estimate TATE using generalize
# ************************************************************ ## 

# libraries 
# devtools::install_github('benjamin-ackerman/generalize') 
library(generalize)
library(dplyr)
library(dbarts)
library(stats)
library(margins)
library(miceadds)


# job
# main dir
wdmain <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1"
setwd(paste0(wdmain, "/outputs/toTATE_PA"))
input <- list.files()
i=as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID')) #174
print(i)

myData <- read.csv(input[i])
name <- gsub(".csv", "", input[i])

#### my tweaked version of generalize function ----
setwd(paste0(wdmain, "/code")) # job will be for every matched dataset
source("09_myGeneralizeFunct.R")


##### MANUALLY run generalize ----
# head(myData)
treatment <- grep("vs", colnames(myData))
covs <- c("elevation", "slope", "travel_time", "pop", "area")

# (TRIMMED) TOTAL POPULATION AVERAGE TREATMENT EFFECTS (TATE) ----

data <- myData[which(myData$trial==1),] # trial==matched. 0==sampled population
# I used the matched population which is akin to the "trial population"

# if the dataset doesn't have enough observations
if(nrow(data) <= 40){
  setwd(paste0(wdmain, "/outputs/generalize_losses_PA"))
  write.csv(data, paste0(name,"_nrowLoss.csv"), row.names=FALSE)
}

# if the trimmed dataset doesn't have a counterfactual 
if(sum(data[,treatment])<1){
  setwd(paste0(wdmain, "/outputs/generalize_losses_PA"))
  write.csv(data, paste0(name,"_counterfactualLoss.csv"), row.names=FALSE)
}

if(sum(is.na(data$weights))>0){
  setwd(paste0(wdmain, "/outputs/generalize_losses_PA"))
  write.csv(data, paste0(name,"_noWeights.csv"), row.names=FALSE)
}

if(nrow(data) > 40 & sum(data[,treatment]) > 0 & sum(!is.na(data$trial))>0){ # if the data frame has more than 40 observations,
  
  # to prevent factor errors in calculating AME
  data$state <- as.character(data$state)
  
  if(length(unique(data$state)) > 1){ # if there is more than one state in this dataset, run the normal model:
    
    myglm <- miceadds::glm.cluster(data = data, 
                                   formula = as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",
                                                               names(data)[grep("_vs_", names(data))],
                                                               " + elevation + slope + travel_time + pop + area + state + weights")),
                                   cluster="mun", family = binomial(link = "logit"))
  }
  if(length(unique(data$state)) <= 1){ # if there is less than one state, run this adjusted model that does not control for state as a factor
    
    myglm <- miceadds::glm.cluster(data = data, 
                                   formula = as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",
                                                               names(data)[grep("_vs_", names(data))],
                                                               " + elevation + slope + travel_time + pop + area + weights")),
                                   cluster="mun", family = binomial(link = "logit"))
  }
  setwd(paste0(wdmain, "/outputs/generalize_glms_PA"))
  saveRDS(myglm, paste0(name, ".rds"))
  
  # calculate average marginal effects
  ame <- summary(margins(model=myglm$glm_res, vcov= myglm$vcov, data=data, type = "response"))  # to keep on response variable scale (%)
  ame$n_pop <- as.integer(nrow(data))
  ame$stScale <- name
  a <- grep("_vs_", ame$factor)
  
  setwd(paste0(wdmain, "/outputs/generalize_ames_PA"))
  write.csv(ame[a,], paste0(name, ".csv"), row.names = F)
}
