# start

library("xlsx")
isotope_data <- read.xlsx("edited_community_isotope_data.xlsx",2)
head(isotope_data)
unique(isotope_data$Taxa)
?barplot

table(isotope_data$Species_ID)

#barplots by taxa means
mean_taxa_pedino = tapply(isotope_data$pedino_13C_enrichment, isotope_data$Taxa, mean)
mean_taxa_pedino_percent = tapply(isotope_data$atom_13C_percent,isotope_data$Taxa, mean)
barplot(mean_taxa_pedino,las=2)
barplot(mean_taxa_pedino_percent, las=2, ylim=c(0.0, 1.2))

mean_taxa_pico = tapply(isotope_data$pico_15N_enrichment, isotope_data$Taxa, mean)
mean_taxa_pico_percent = tapply(isotope_data$atom_15N_percent,isotope_data$Taxa, mean)
barplot(mean_taxa_pico,las=2)
barplot(mean_taxa_pico_percent, las=2)

#barplots by site
mean_site_pedino = tapply(isotope_data$pedino_13C_enrichment, isotope_data$Panel_Site, mean)
mean_site_pedino_percent = tapply(isotope_data$atom_13C_percent,isotope_data$Panel_Site, mean)
barplot(mean_site_pedino,las=2)
barplot(mean_site_pedino_percent, las=2, ylim=c(0.0, 1.2))

mean_site_pico = tapply(isotope_data$pico_15N_enrichment, isotope_data$Panel_Site, mean)
mean_site_pico_percent = tapply(isotope_data$atom_15N_percent,isotope_data$Panel_Site, mean)
barplot(mean_site_pico,las=2)
barplot(mean_site_pico_percent, las=2)

library(ggplot2)
ggplot(data = isotope_data) + 
  geom_boxplot(mapping = aes(x = Taxa, y = pedino_13C_enrichment)) +  
  labs(x = 'Taxa', y = 'pedino_13C_enrichment', title = 'taxa vs pedino_13C_enrichment') 
ggplot(data = isotope_data) + 
  geom_boxplot(mapping = aes(x = Taxa, y = pico_15N_enrichment)) +  
  labs(x = 'Taxa', y = 'pico_15N_enrichment', title = 'taxa vs pico_15N_enrichment') 

pedinotaxa = lm(isotope_data$pedino_13C_enrichment ~ -1 + isotope_data$Taxa)
pedinosite = lm(isotope_data$pedino_13C_enrichment ~ -1 + isotope_data$Panel_Site)

unique(isotope_data$Panel_Site)
summary(pedinosite)
summary(pedinotaxa)

pedinospecies = lm(isotope_data$pedino_13C_enrichment ~ -1 + isotope_data$Species_ID)
summary(pedinospecies)


picotaxa = lm(isotope_data$pico_15N_enrichment ~ -1 + isotope_data$Taxa)
picosite = lm(isotope_data$pico_15N_enrichment ~ -1 + isotope_data$Panel_Site)

summary(picotaxa)
summary(picosite)
head(isotope_data)

picospecies = lm(isotope_data$pico_15N_enrichment ~ -1 + isotope_data$Species_ID)

length(unique(isotope_data$Species_ID))

plot(table(isotope_data$Species_ID), las=2)
summary(picospecies)
#str(dune_mds)

# subset analysis of all the isotope species 
# species consistently showing small or weak dependency
# deterministic vs stochastic 

unique(isotope_data$Panel_Site)

# subsets by Panel
FC_isotope <- subset(isotope_data, isotope_data$Panel_Site == "FC")
head(FC_isotope)
length(unique(FC_isotope$Species_ID))
ggplot(data = FC_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment FC') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = FC_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment FC') + theme(axis.text.x = element_text(angle = 90))


HI_isotope <- subset(isotope_data, isotope_data$Panel_Site == "HI")
head(HI_isotope)
length(unique(HI_isotope$Species_ID))
ggplot(data = HI_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment HI') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = HI_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment HI') + theme(axis.text.x = element_text(angle = 90))



ID_isotope <- subset(isotope_data, isotope_data$Panel_Site == "ID")
head(ID_isotope)
length(unique(ID_isotope$Species_ID))
ggplot(data = ID_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment ID') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = ID_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment ID') + theme(axis.text.x = element_text(angle = 90))



IRL3_isotope <- subset(isotope_data, isotope_data$Panel_Site == "IRL3")
head(IRL3_isotope)
length(unique(IRL3_isotope$Species_ID))
ggplot(data = IRL3_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment IRL3') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = IRL3_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment IRL3') + theme(axis.text.x = element_text(angle = 90))



MIM_isotope <- subset(isotope_data, isotope_data$Panel_Site == "MIM")
head(MIM_isotope)
length(unique(MIM_isotope$Species_ID))
ggplot(data = MIM_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment MIM') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = MIM_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment MIM') + theme(axis.text.x = element_text(angle = 90))



MO2_isotope <- subset(isotope_data, isotope_data$Panel_Site == "MO2")
head(MO2_isotope)
length(unique(MO2_isotope$Species_ID))
ggplot(data = MO2_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment MO2') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = MO2_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment MO2') + theme(axis.text.x = element_text(angle = 90))



SCD_isotope <- subset(isotope_data, isotope_data$Panel_Site == "SCD")
head(SCD_isotope)
length(unique(SCD_isotope$Species_ID))
ggplot(data = SCD_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment SCD') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = SCD_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment SCD') + theme(axis.text.x = element_text(angle = 90))



SMS_isotope <- subset(isotope_data, isotope_data$Panel_Site == "SMS")
head(SMS_isotope)
length(unique(SMS_isotope$Species_ID))
ggplot(data = SMS_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment SMS') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = SMS_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment SMS') + theme(axis.text.x = element_text(angle = 90))



WP_isotope <- subset(isotope_data, isotope_data$Panel_Site == "WP")
head(WP_isotope)
length(unique(WP_isotope$Species_ID))
ggplot(data = WP_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pedino_13C_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pedino_13C_enrichment', title = 'species vs pedino_13C_enrichment WP') + theme(axis.text.x = element_text(angle = 90))

ggplot(data = WP_isotope) + 
  geom_boxplot(mapping = aes(x = Species_ID, y = pico_15N_enrichment, color=Taxa)) +  
  labs(x = 'species_id', y = 'pico_15N_enrichment', title = 'species vs pico_15N_enrichment WP') + theme(axis.text.x = element_text(angle = 90))


library(tidyr)
SMSandWP = subset(isotope_data, (isotope_data$Panel_Site == "SMS" | isotope_data == "WP"))
SMSandWP = drop_na(data=SMSandWP)
tail(SMSandWP)
library(dplyr)
SMSandWP_group_by <- SMSandWP %>% group_by(Panel_Site)
SMSandWP_group_by
by_Panel_Site <- isotope_data %>% group_by(Panel_Site)
group_isotope

# priceTools start-
# ask Prof. about what we should eb grouping by and what the function should be-

library(priceTools)
SMSandWP_pairwise_price = pairwise.price(x = SMSandWP_group_by, species = "Species_ID", func = "pedino_13C_enrichment")
SMSandWP_dist = get.dist.mats(SMSandWP_pairwise_price)
SMSandWP_pairwise_price = drop_na(SMSandWP_pairwise_price)

processed_SMSandWP = process.data.price(SMSandWP_pairwise_price)
priceTools::leap.zig.price(processed_SMSandWP)

by_Panel_Site_pairwise_13 = pairwise.price(x = by_Panel_Site, species = "Species_ID", func = "pedino_13C_enrichment")
by_Panel_Site_dist_13 = get.dist.mats(by_Panel_Site_pairwise_13)
by_Panel_Site_pairwise_13 = drop_na(by_Panel_Site_pairwise_13)
processed_by_site_13 = process.data.price(by_Panel_Site_pairwise_13)
leap.zig.price(processed_by_site_13)

head(isotope_data)
isotope_data

# questions about wider
pedino_subset <- isotope_data[c('Site_and_Panel',"Species_ID","pedino_13C_enrichment","atom_13C_percent")]
head(pedino_subset)
class(pedino_subset$pedino_13C_enrichment[0])
pico_subset <- isotope_data[c('Site_and_Panel',"Species_ID","pico_15N_enrichment")]

pico_subset_edited <- pico_subset
pico_subset_edited$pico_15N_enrichment = as.numeric(pico_subset_edited$pico_15N_enrichment)
pico_subset_edited$Species_ID = as.factor(pico_subset_edited$Species_ID)
pico_subset_edited$Site_and_Panel = as.factor(pico_subset_edited$Site_and_Panel)

pico_subset_edited[,"pico_15N_enrichment"] <- sapply(pico_subset_edited[,"pico_15N_enrichment"], as.factor)

head(pedino_subset)

library(tidyr)
# fish_encounters %>% pivot_wider(
#   names_from = station, 
#   values_from = seen,
#   values_fill = 0

?pivot_wider
vignette("pivot")

?dbl


comm_pedino <- with(pedino_subset, tapply(atom_13C_percent, list(Site_and_Panel, Species_ID), sum))
comm_pedino <- ifelse(is.na(comm), 0, comm)
head(comm_pedino)

comm_pico <- with(pico_subset, tapply(atom_15N_percent, list(Site_and_Panel, Species_ID, sum)))
comm_pico <- ifelse(is.na(comm), 0, comm)
head(comm_pico)
