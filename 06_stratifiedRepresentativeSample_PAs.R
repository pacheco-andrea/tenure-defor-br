# ************************************************************ #
# Extract gids from Matching
# ************************************************************ #

# outputs:
# representative samples of each matched scale from the unmatched data
# based on https://gist.github.com/gianlucamalato/24f5d560cd27ded356f70843e22b79db#file-stratified-sampling-r
# blog: https://towardsdatascience.com/stratified-sampling-and-how-to-perform-it-in-r-8b753efde1ef

# libraries needed
library(dplyr)
library(readr)
library(foreign)
library(sqldf)

# set wd's 
#wd_main <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/"
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"


# 1. tables with matches ----
# this follows the imbalance script
setwd(paste0(wd_main, "outputs/MatchedDatasets_PA"))
input <- list.files()
i=as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID')) # set job as 1-196
m <- readRDS(input[i])
print(input[i])

if(class(m) == "cem.match"){
  
  name1 <- gsub(".rds", "", input[i])
  print(paste0(name1, " was a successful match object"))
  
  # 2. get tables with all unmatched gid's + variables needed to stratify ----
  # and add "matched" column to original dataframes
  setwd(paste0(wd_main, "outputs/MatchedPADatasets_yearCreatedSubset"))
  matcheslist <- list.files()
  # ensure get match that corresponds
  match_needed <- match(name1, gsub("_YrSubset.csv", "", matcheslist))
  df <- read.csv(matcheslist[match_needed])
  name2 <- gsub("_YrSubset.csv","", matcheslist[match_needed])
  
  df$matched <- m$matched
  unmatched_d <- df
  
  
  # 3. conduct stratification ----
  # vars to consider in stratification
  d1 <- grep("[0-9]", colnames(unmatched_d))
  d2 <- c("elevation", "slope", "travel_time", "area")
  dimensions <- c(d2, colnames(unmatched_d[d1]))
  
  # establish n sample
  n_sample <- nrow(unmatched_d[which(unmatched_d$matched == TRUE),])*10 
  # im multiplying by 10 because these subsets are just so few, i think this will help later on
  # in at least getting proper weights for the whole population

    # create empty table (to be filled in)
  generated <- head(as.data.frame(unmatched_d), 0)
  

  while (nrow(generated) < n_sample) {
    # For debug purposes
    cat(nrow(generated),"\n")
    flush.console()
    
    tmp = unmatched_d
    
    # Calculate the histogram for each dimension
    # and select one value at a time, slicing the
    # original dataset according to its histogram
    for (j in 1:length(dimensions)) { # for each variable
      
      colname = dimensions[j]
      if (class(unmatched_d[[colname]]) %in% c("numeric") && # if it's numeric
          sum(unmatched_d[[colname]] == as.integer(unmatched_d[[colname]]), na.rm = TRUE) == 0 # and if it sums to 0
      ) {
        # Numerical variable. Histogram with Rice's Rule
        
        # If there are NA's, stratify on those
        
        na_count = sum(is.na(tmp[[colname]]))
        not_na_count = length(tmp[[colname]]) - na_count
        
        s = sample(c(0,1),prob = c(not_na_count,na_count),1)
        
        if (s == 0) {
          # Histogram stratification based on breaks calculated on the
          # population
          
          n_breaks = floor(2*sum(!is.na(unmatched_d[[colname]]))**((1/3)))
          bar_size = (max(unmatched_d[[colname]],na.rm = TRUE)-min(unmatched_d[[colname]],na.rm = TRUE))/n_breaks
          
          breaks = sapply(0:n_breaks,function(i) {min(unmatched_d[[colname]],na.rm = TRUE) + i*bar_size})
          
          h = hist(tmp[[colname]],breaks=breaks,plot = F)
          
          # Select one bar of the histogram according to the density
          bar_id  = sample(1:length(h$mids),prob = h$counts,1)
          
          bar_start = h$breaks[bar_id]
          bar_end = h$breaks[bar_id + 1]
          
          tmp = tmp[tmp[[colname]] >= bar_start & tmp[[colname]] < bar_end & !is.na(tmp[[colname]]),]
        } else {
          # NA
          tmp = tmp[is.na(tmp[[colname]]),]
        }
        
      } else {
        # Categorical variable
        
        # Histogram for the selected dimension
        aggr = as.data.frame(table(tmp[,colname],useNA="ifany"))
        names(aggr) = c("dim","count")
        
        # Generate a value according to the histogram
        generated_value = sample(aggr$dim,prob=aggr$count,1)
        
        # Slice the actual multivariate histogram in order to
        # take only records with the selected value on the
        # selected dimension
        if (!is.na(generated_value)) {
          tmp = tmp[tmp[[colname]] == generated_value & !is.na(tmp[[colname]]),]
        }
        else {
          tmp = tmp[is.na(tmp[[colname]]),]
        }
        
      }
      
    }
    
    # Once the procedure finishes, we get a bulk of records
    # with the same values of each dimension. Let's take
    # one of these records uniformly
    random_index = sample(1:nrow(tmp),1)
    new_record = tmp[random_index,]
    
    # Let's remove duplicates
    inserted_record = sqldf("select * from new_record except select * from generated")
    
    # Insert in the "generated" data frame and repeat until desired sample size is reached
    generated = rbind(generated,inserted_record)
  }

  setwd(paste0(wd_main, "outputs/representativeSamplesPop_PA"))
  write.csv(generated, paste0(gsub("rds", "csv", input[i])), row.names = F)
  
}








