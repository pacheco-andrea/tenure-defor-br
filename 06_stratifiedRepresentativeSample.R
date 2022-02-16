# ************************************************************ #
# Get stratified representative sample of the entire population of land parcels
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
# install.packages("SamplingStrata")
# library(SamplingStrata)

# set wd's 
#wd_main <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/"
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"



# 1. tables with matches ----
# read in csv tables that have only the gid's that were matched:
setwd(paste0(wd_main, "outputs/MatchedDatasets_onlymatches"))
l <- list.files()
names <- gsub(".csv","", l)


# set job
i=as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
print(names[i])

matched <- read_csv(l[i], col_types = cols_only(gid = col_double(),
                                                mun = col_character(),
                                                biome = col_character()))

n_matched <- nrow(matched)
  


# 2. get tables with all unmatched gid's + variables needed to stratify ----
setwd(paste0(wd_main, "inputs/00_data/for_matching/forMatchAnalysisCEM"))
unmatched_d <- as.data.frame(read_csv(l[i], col_types = "?????__?_??_"))
head(unmatched_d)



# 3. conduct stratification ----
#i=1

# vars to consider in stratification
dimensions <- setdiff(names(unmatched_d), c("gid", "mun","biome"))
# establish n sample
n_sample <- n_matched # NOTE: would have to correspond exactly
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


setwd(paste0(wd_main, "outputs/representativeSamplesPop"))
write.csv(generated, paste0(names[i], ".csv"), row.names = F)






