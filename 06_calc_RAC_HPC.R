# ****************************** #
# Calculating RAC
# ****************************** #
# this script calculates a residuals auto-covariate to be used in final models
# it is carried out on the HPC
library("stats")
library("readr")
library("tidyr")
library("sf")
library("foreign")
library("rgdal")
library("data.table")
library("ape")
library("sp")
library("spdep")

# dirs
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/nonSp_glms/"
wd_glms <- paste0(wd_main, "output")
wd_shapes <- paste0(wd_main, "shapes")
wd_out <- paste0(wd_main, "RAC")

# read in spatial data ----
setwd(wd_shapes)
s <- list.files()
s_pos <- grep(".shp", s)
shp <- s[s_pos]
s_n <- gsub(".shp", "", shp)

shapes <- lapply(shp, st_read)
shapes <- rbind(shapes[[1]], shapes[[2]],shapes[[3]],shapes[[4]],shapes[[5]],shapes[[6]]) # do not attempt to lapply rbind, it won't work right

# read in glms and establish which job to do ----
setwd(wd_glms)
glms <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID'))

glm <- readRDS(glms[i])
glm_n <- gsub(".rds", "", glms[i])

# add spatial data to glms data
myglm_data <- merge(shapes, glm$data, by = "gid")
# coordinate calculations:
cen_coords <- st_centroid(myglm_data$geometry)
myglm_data$coords <- cen_coords
my_coords <- st_coordinates(myglm_data$coords)

rm(shapes) # to keep things as light as possible when calculating the distances

# RAC calc ----

#define distance
mydist = 500 
r <- resid(glm)
# create new column in original glm data with Residuals auto-covariate calculation
glm$data$rac <- spdep::autocov_dist(r, my_coords, nbs = mydist, type = "inverse", zero.policy = T, style = "W", longlat = T)

# write new dataframes
setwd(wd_out)
write.csv(as.data.frame(glm$data), paste0(glm_n, ".csv"))



