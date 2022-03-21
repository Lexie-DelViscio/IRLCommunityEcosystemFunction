# start
# install.packages ("readxl"")

library("xlsx")
cytometry_file = read.xlsx("edited_community_experiment_data.xlsx",5)
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

Picocytometry_file$...1
#panels delimited as numbers for ease of exploratory modeling

Panels     "1"         "2"         "3"         "4"         "5"        
"6"         "7"         "8"         "9"         "10"        "11"       
"12"        "13"        "14"        "15"        "16"        "17"       
"18"        "19"        "20"        "21"        "22"        "23"       
"24"        "25"        NA          "Control 1" "Control 2" "Control 3"

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




  
    