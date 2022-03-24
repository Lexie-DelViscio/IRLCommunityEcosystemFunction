## Species Richness Data

library(readxl)
edited_community_experiment_data <- read_excel("edited_community_experiment_data.xlsx", 
                                               sheet = "panel community composition")
View(edited_community_experiment_data)
suspensionfeeders <- edited_community_experiment_data
library(vegan)
library(dummies)
head(suspensionfeeders)
species_id <- suspensionfeeders[c (0,1)]

#also suspensionfeeders$Species
## First we want to look at overall species abundance/richness among all panels
# Looking at simple species abundance rather than correlation with any other variable
uni_sp = unique(suspensionfeeders$Species)
uni_site = unique(panels)

A <- matrix(suspensionfeeders, nrow = 28)
panels <- A[c(2:28), ]










## Now we will examine species abundance per site (group/subset each of the 1-4 panels into a site location)

