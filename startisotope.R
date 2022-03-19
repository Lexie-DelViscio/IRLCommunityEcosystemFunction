# start

library("xlsx")
isotope_data <- read.xlsx("edited_community_isotope_data.xlsx",2)
head(isotope_data)
unique(isotope_data$Taxa)
?barplot

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



unique(isotope_data$Panel_Site)
summary(pedinosite)


# pico_cytometry <- cytometry_file[ -c(0,14:25) ]
# pico_cytometry
# pedino_cytometry <- cytometry_file[ -c(0,2:13) ]
# pedino_cytometry
# 
# 
# time_values = list(5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180)
# 
# 
#   
  
    