# start.frannie.cyto
# install.packages ("readxl"")

library("read_excel")
cytometry_file = read.xl("edited_community_experiment_data.xlsx",)
head(cytometry_file)
cytometry_file$Panel[1]

pico_cytometry <- cytometry_file[ -c(0,14:25) ]
pico_cytometry
pedino_cytometry <- cytometry_file[ c(0,14:25) ]
pedino_cytometry

plot(Panel[1] ~ Pico5 + Pico10 + Pico15 + Pico20 + Pico30 + Pico40 + Pico50 + Pico60 + Pico90 + Pico120 + Pico150 + Pico180, data = cytometry_file)
time_values = list(5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180)
plot( time_values, cytometry_file[2,])

Cell_Proportions <- Picocytometry_file[ c(0, 3:14)]
boxplot(Cell_Proportions, xlab = 'Time Intervals', ylab = 'Average Cell Counts')

## Read.xlsx function not working for me so -- 

library(readxl)
edited_community_experiment_data <- read_excel("edited_community_experiment_data.xlsx", 
                                               +     sheet = "Pico Cell Removal Flow CYTO")

## load in new excel sheet with just time values 0-180 minutes, panels shown as #'s and cell proportions
names(Picocytometry_file)
Picocytometry_file$...1
#panels delimited as numbers for ease of exploratory modeling

## Panels     "1"         "2"         "3"         "4"         "5"        
##"6"         "7"         "8"         "9"         "10"        "11"       
##"12"        "13"        "14"        "15"        "16"        "17"       
##"18"        "19"        "20"        "21"        "22"        "23"       
##"24"        "25"        NA          "Control 1" "Control 2" "Control 3"

plot(Picocytometry_file$...1 ~ Picocytometry_file$`5`, xlab = 'Cell Proportions 5 Minutes', ylab = 'Panels')
plot(Picocytometry_file$...1 ~ Picocytometry_file$`10`, xlab = 'Cell Proportions 10 Minutes', ylab = 'Panels')
plot(Picocytometry_file$...1 ~ Picocytometry_file$`15`, xlab = 'Cell Proportions 15 Minutes', ylab = 'Panels')
plot(Picocytometry_file$...1 ~ Picocytometry_file$`20`, xlab = 'Cell Proportions 20 Minutes', ylab = 'Panels')
plot(Picocytometry_file$...1 ~ Picocytometry_file$`30`, xlab = 'Cell Proportions 30 Minutes', ylab = 'Panels')
plot(Picocytometry_file$...1 ~ Picocytometry_file$`150`, xlab = 'Cell Proportions 150 Minutes', ylab = 'Panels')
plot(Picocytometry_file$...1 ~ Picocytometry_file$`180`, xlab = 'Cell Proportions 180 Minutes', ylab = 'Panels')
## Exploratory models examing over each time interval  

library(readxl)
edited_community_experiment_data <- read_excel("edited_community_experiment_data.xlsx", 
                                               +     sheet = "Pedino Cell Removal Flow CYTO")
View(edited_community_experiment_data)  
pedino_cytometry = edited_community_experiment_data <- read_excel("edited_community_experiment_data.xlsx", sheet = "Pedino Cell Removal Flow CYTO")
Cell_Proportions_Pedino <- pedino_cytometry[ c(0, 3:14)]
boxplot(Cell_Proportions_Pedino, xlab = 'Time Intervals', ylab = 'Average Cell Counts')

pedino_FC = pedino_cytometry[c(2:4),]
pedino_HI = pedino_cytometry[c(5:7),]
pedino_ID = pedino_cytometry[c(8:9),]
pedinoIRL3 = pedino_cytometry[c(10:13),]
pedinoMIM = pedino_cytometry[c(14:15),]
pedinoMO = pedino_cytometry[c(16:17),]
pedinoMO = pedino_cytometry[c(16:18),]
pedinoSCD = pedino_cytometry[c(19:21),]
pedinoSMS = pedino_cytometry[c(22:23),]
pedinoWP = pedino_cytometry[c(24:26),]
pedinoControl = pedino_cytometry[c(28:30),]
matplot(t(pedino_FC), type = "l")
matplot(t(pedino_HI), type = "l")
matplot(t(pedino_ID), type = "l")
matplot(t(pedinoIRL3), type = "l")
matplot(t(pedinoMIM), type = "l")
matplot(t(pedinoMO), type = "l")
matplot(t(pedinoSCD), type = "l")
matplot(t(pedinoSMS), type = "l")
matplot(t(pedinoWP), type = "l")
matplot(t(pedinoControl), type = "l")

