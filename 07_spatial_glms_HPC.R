# ****************************** #
# Spatial regression analysis 
# ****************************** #
# this script conducts spatial glm's incorporating racs's
# it is carried out on the HPC
library("stats")
library("margins")

# working dirs
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/spGLMs/"
wd_in <- paste0(wd_main, "input")
wd_logits <- paste0(wd_main, "output_logit")
wd_ame <- paste0(wd_main, "output_ame")

# job ----
setwd(wd_in)
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID'))

setwd(wd_in)
data <- read.csv(input[i])
n <- gsub(".csv", "", input[i])

# glms ----
 
if(nrow(data) > 1){ # if the data frame has more than one observation,
  
  if(length(unique(data$state)) > 1){ # if there is more than one federal state in this dataset, run the normal model:
    
    myglm <- glm(as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",names(data)[grep("_vs_", names(data))]," + elevation + slope + travel_time + pop + area + as.character(state) + rac")),
                   data = data, family = binomial(link = "logit"))
  }
  if(length(unique(data$state)) <= 1){ # if there is less than one federal state, run this adjusted model that does not control for state as a factor
    
    myglm <- glm(as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",names(data)[grep("_vs_", names(data))]," + elevation + slope + travel_time + pop + area + rac")),
                   data = data, family = binomial(link = "logit"))
  }
}
setwd(wd_logits)
saveRDS(myglm, paste0(n, "_logits_", ".rds"))

# calculate average marginal effects
ame <- summary(margins(myglm, type = "response"))  # make sure to keep on response variable scale (%)
setwd(wd_ame)
write.csv(ame, paste0(n, "_ame_", ".csv"))

