# ************************************************************ #
# Run matching with CEM as job
# This is a sensitivity analysis to consider effects of PAs/Sust_use areas
# the difference is that this time we will only consider PAs and Sust use areas that existed ~time of temp scale considered
# ************************************************************ #

library("cem")
library("stringdist")

#working dirs
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1"
wd_in <- paste0(wd_main, "/inputs/00data/for_matching/forMatchAnalysisCEM")
wd_out <- paste0(wd_main, "/outputs/MatchedDatasets_PA")


travel_cut = c(0,240,360,720,1440) # by hours (0,2,6,12,24)
area_cut = c(0,2,5,15,50,100,500,1000,5000,10000,50000,100000,500000,1000000) # 14 classes of property sizes 
todrop = c("gid", "tenure", "name", "state", "mun", "biome") #edit

# job:
setwd(wd_in)
input <- list.files()

input2 <- grep("protected", input)
input3 <- grep("sustainable", input)

myinput <- input[c(input2,input3)] # length of 196

i=as.integer(Sys.getenv('SGE_TASK_ID'))

# read data
data <- read.csv(myinput[i])
n <- gsub(".csv", "", myinput[i])

# read data from the cadastre of PAs (CNUC)
setwd(paste0(wd_main, "/inputs/unidades_cons"))
uc <- read.csv("cnuc_2020_2-semestre.csv", sep=";")
uc2 <- data.frame("name" = uc$Nome.da.UC, "iucn"= uc$Categoria.IUCN, "yearCreate"= uc$Ano.de.Criação, "categ"=uc$Categoria.de.Manejo)
# add column for year created
data$yearCreated <- NA
b <- amatch(data$name[which(data[,12] == 1)], uc2$name, maxDist=10) # for differences in spelling w imaflora
length(data$name[which(data[,12] == 1)]) == length(uc2$name[b]) 
# fill the year in for these positions
data$yearCreated[which(data[,12] == 1)] <- as.numeric(uc2$yearCreate[b])

# subset dataset 
# get first and last years
first <- gsub("([^_]*)_(.*)","\\1", gsub("([^_]*_[^_]*)_(.*)","\\1", n))
last <- gsub("([^_]*)_(.*)","\\2", gsub("([^_]*_[^_]*)_(.*)","\\1", n))

# x = length of time chunk
x <- as.numeric(last)-as.numeric(first)
# y = number of years we need to sum to the first year in order to only include 20% of that particular time chunk
y <- (x*20)/100
# subset data to include only the PA's created in years which - at maximum - include only 20% of the time chunk in question
# (leaving an 80% of overlap in period in which the PA existed)
# OR for which we have no data on year of creation (no matches or non-pa's)
ydata <- subset(data, yearCreated <= round(as.numeric(first)+y) | is.na(yearCreated))

# need to write these subsetted dataframes 
# because i will need to precisely bind them with the matches produced later
setwd(paste0(wd_main, "/outputs/MatchedPADatasets_yearCreatedSubset"))
write.csv(ydata, paste0(n, "_YrSubset.csv"))

data <- ydata
#match
drop_this <- c(todrop, paste0(colnames(data)[grep("_vs_",colnames(data))]), "yearCreated")

tmp_match <- try(cem(treatment = colnames(data)[grep("_vs_",colnames(data))], 
                       data = as.data.frame(data),
                       drop = drop_this,
                       cutpoints = list(travel_time = travel_cut, area = area_cut),
                       keep.all = T,
                       k2k = T,
                       method = "euclidean"), silent = T) 
setwd(wd_out)

if(class(tmp_match) == "try-error"){saveRDS(tmp_match, paste0(n, "_try-error", ".rds"))}
if(class(tmp_match) == "cem.match"){saveRDS(tmp_match, paste0(n, ".rds"))}
