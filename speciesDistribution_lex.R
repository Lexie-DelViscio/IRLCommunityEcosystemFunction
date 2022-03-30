## Species Richness Data

library("xlsx")
edited_community_experiment_data <- read.xlsx("edited_community_experiment_data.xlsx",3)
View(edited_community_experiment_data)
community_panel_data <- edited_community_experiment_data
library(vegan)
library(dummies)
head(community_panel_data)
species_id <- community_panel_data[c (0,1)]
species_id
#also suspensionfeeders$Species
## First we want to look at overall species abundance/richness among all panels
# Looking at simple species abundance rather than correlation with any other variable
library(tidyr)
?pivot_longer
par(mfrow=c(1,4))
Longer = community_panel_data %>%
  pivot_longer(everything(),
               names_to = c(".value"))
Longer


plot(community_panel_data$FC_01[which(community_panel_data$FC_01 > 0)])
text(community_panel_data$FC_01[which(community_panel_data$FC_01 > 0)], labels=community_panel_data$Species[which(community_panel_data$FC_01 > 0)], cex=0.9, font=1)


plot(community_panel_data$FC_02[which(community_panel_data$FC_02 > 0)])
text(community_panel_data$FC_02[which(community_panel_data$FC_02 > 0)], labels=community_panel_data$Species[which(community_panel_data$FC_02 > 0)], cex=0.9, font=1)


plot(community_panel_data$FC_03[which(community_panel_data$FC_03 > 0)])
text(community_panel_data$FC_03[which(community_panel_data$FC_03 > 0)], labels=community_panel_data$Species[which(community_panel_data$FC_03 > 0)], cex=0.9, font=1)


plot(community_panel_data$FC_04[which(community_panel_data$FC_04 > 0)])
text(community_panel_data$FC_04[which(community_panel_data$FC_04 > 0)], labels=community_panel_data$Species[which(community_panel_data$FC_04 > 0)], cex=0.9, font=1)




# Now we will examine species abundance per site (group/subset each of the 1-4 panels into a site location)

