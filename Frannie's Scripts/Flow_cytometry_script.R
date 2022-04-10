file <- read.csv("edited_community_experiment_data.csv")
file
file <- na.omit(file)
file

pico_subset <- file[,c("Panel","Pico5","Pico10","Pico15","Pico20",
                       "Pico30","Pico40","Pico50","Pico60","Pico90","Pico120","Pico150","Pico180")]
head(pico_subset)

pedino_subset <- file[,c("Panel","Pedino5","Pedino10","Pedino15","Pedino20",
                       "Pedino30","Pedino40","Pedino50","Pedino60","Pedino90","Pedino120","Pedino150","Pedino180")]
head(pedino_subset)


