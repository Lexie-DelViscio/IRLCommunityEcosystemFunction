library("xlsx")
isotope_data <- read.xlsx("edited_community_isotope_data.xlsx",2)

library("xlsx")
cytometry_data <- read.xlsx("edited_community_experiment_data.xlsx",5)



head(cytometry_data)
cytometry_data$Panel[1]
pico_cytometry <- cytometry_data[ -c(0,14:25) ]
pico_cytometry
pedino_cytometry <- cytometry_data[ -c(0,2:13) ]
pedino_cytometry
time_values = list(5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180)
