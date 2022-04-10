## Species Richness Data

library("xlsx")
community_panel_data <- read.xlsx("edited_community_experiment_data.xlsx",3)
community_panel_data = subset(community_panel_data, select = -c(NA.))
library(vegan)
library(dummies)
head(community_panel_data)
species_id <- community_panel_data[c (0,1)]
species_id
## First we want to look at overall species abundance/richness among all panels
# Looking at simple species abundance rather than correlation with any other variable
library(tidyr)
?pivot_longer
community_panel_data

plot(community_panel_data$FC_01[which(community_panel_data$FC_01 > 0)])
text(community_panel_data$FC_01[which(community_panel_data$FC_01 > 0)], labels=community_panel_data$Species[which(community_panel_data$FC_01 > 0)], cex=0.9, font=1)


plot(community_panel_data$FC_02[which(community_panel_data$FC_02 > 0)])
text(community_panel_data$FC_02[which(community_panel_data$FC_02 > 0)], labels=community_panel_data$Species[which(community_panel_data$FC_02 > 0)], cex=0.9, font=1)


plot(community_panel_data$FC_03[which(community_panel_data$FC_03 > 0)])
text(community_panel_data$FC_03[which(community_panel_data$FC_03 > 0)], labels=community_panel_data$Species[which(community_panel_data$FC_03 > 0)], cex=0.9, font=1)


plot(community_panel_data$FC_04[which(community_panel_data$FC_04 > 0)])
text(community_panel_data$FC_04[which(community_panel_data$FC_04 > 0)], labels=community_panel_data$Species[which(community_panel_data$FC_04 > 0)], cex=0.9, font=1)



Long_community_panel = community_panel_data %>%
  pivot_longer(cols = c(FC_01, FC_02, FC_03, FC_04, HI_01, HI_02, HI_03, HI_04,ID_01, ID_03, IRL3_01, IRL3_02, IRL3_03, IRL3_04, MIM_01, MIM_02, MO_01, MO_02, MO_03, SCD_01,SCD_02, SCD_03, SMS_01, SMS_03, WP_01, WP_03, WP_04), names_to = c("Site"))
head(Long_community_panel)
head(Long_community_panel)
head(community_panel_data)

# Now we will examine species abundance per site (group/subset each of the 1-4 panels into a site location)


# I will also investigate 
# s.PIE
# Simpsons evenness index 
# abundance by sites for each panel we can get a measurement of evenness
# the measure of evenness can be plotted against the traits frannies is deriving from flow cytometry in the end


# Now we will examine species abundance per site (group/subset each of the 1-4 panels into a site location)


