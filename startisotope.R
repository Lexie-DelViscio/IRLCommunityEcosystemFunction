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

HI_isotope <- subset(isotope_data, isotope_data$Panel_Site == "HI")
head(HI_isotope)
length(unique(HI_isotope$Species_ID))

ID_isotope <- subset(isotope_data, isotope_data$Panel_Site == "ID")
head(ID_isotope)
length(unique(ID_isotope$Species_ID))

IRL3_isotope <- subset(isotope_data, isotope_data$Panel_Site == "IRL3")
head(IRL3_isotope)
length(unique(IRL3_isotope$Species_ID))

MIM_isotope <- subset(isotope_data, isotope_data$Panel_Site == "MIM")
head(MIM_isotope)
length(unique(MIM_isotope$Species_ID))

MO2_isotope <- subset(isotope_data, isotope_data$Panel_Site == "MO2")
head(MO2_isotope)
length(unique(MO2_isotope$Species_ID))

SCD_isotope <- subset(isotope_data, isotope_data$Panel_Site == "SCD")
head(SCD_isotope)
length(unique(SCD_isotope$Species_ID))

SMS_isotope <- subset(isotope_data, isotope_data$Panel_Site == "SMS")
head(SMS_isotope)
length(unique(SMS_isotope$Species_ID))

WP_isotope <- subset(isotope_data, isotope_data$Panel_Site == "WP")
head(WP_isotope)
length(unique(WP_isotope$Species_ID))

library("priceTools")


#NOTES

# pico_cytometry <- cytometry_file[ -c(0,14:25) ]
# pico_cytometry
# pedino_cytometry <- cytometry_file[ -c(0,2:13) ]
# pedino_cytometry
# 
# 
# time_values = list(5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180)
#   site by species 
# role of envirnomnemt - portfolio affect 
# 

    