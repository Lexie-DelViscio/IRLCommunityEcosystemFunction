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
boxplot(Cell_Proportions, xlab = 'Time (Minutes)', ylab = 'Cell proportion from original', main = 'Cell Removal over 3 hour period for Picocyanobacteria')

 
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
Time = list(pedino_cytometry$`5`, pedino_cytometry$`10`, pedino_cytometry$`15`, pedino_cytometry$`20`, pedino_cytometry$`30`, pedino_cytometry$`40`, pedino_cytometry$`50`, pedino_cytometry$`60`, pedino_cytometry$`90`, pedino_cytometry$`120`, pedino_cytometry$`150`, pedino_cytometry$`180`)
Cell_Proportions_Pedino <- pedino_cytometry[ c(0, 3:14)]
boxplot(Cell_Proportions_Pedino, xlab = 'Time Intervals', ylab = 'Average Cell Counts')
# OR boxplot(Time, Panels_Pedino, xlab = "Time Intervals 1-12", ylab = "Cell Proportions Pedino Among All Panels & Sites")
TimePico = list(pico_cytometry$Pico5, pico_cytometry$Pico10, pico_cytometry$Pico15, pico_cytometry$Pico20, pico_cytometry$Pico30, pico_cytometry$Pico40, pico_cytometry$Pico50, pico_cytometry$Pico60, pico_cytometry$Pico90, pico_cytometry$Pico120, pico_cytometry$Pico150, pico_cytometry$Pico180)
head(TimePico)

headpedino_FC = pedino_cytometry[c(2:4),]
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
Pico_FC = pico_cytometry[c(2:4),]
Pico_HD = pico_cytometry[c(5:7),]
Pico_ID = pico_cytometry[c(8:9),]
Pico_IRL3 = pico_cytometry[c(10:13),]
Pico_MIM = pico_cytometry[c(14:15),]
Pico_MO = pico_cytometry[c(16:18),]
Pico_SCD = pico_cytometry[c(19:21),]
Pico_SMS = pico_cytometry[c(22:23),]
Pico_WP = pico_cytometry[c(24:26),]
Pico_Control = pico_cytometry[c(28:30),]
matplot(t(pedino_FC), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedino_HI), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedino_ID), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedinoIRL3), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedinoMIM), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedinoMO), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedinoSCD), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedinoSMS), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedinoWP), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte FC Panel Cell Removal")
matplot(t(pedinoControl), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Pedinophyte Control Panel Cell Removal")
legend("top right", as.character(nr), col = cols, cex = 0.5, lty = ltyp, ncol = 3)
matplot(t(Pico_FC), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Picocyanobacteria FC Panel Cell Removal")
matplot(t(Pico_HI), type = "l")
matplot(t(Pico_ID), type = "l")
matplot(t(Pico_IRL3), type = "l")
matplot(t(Pico_MIM), type = "l")
matplot(t(Pico_MO), type = "l")
matplot(t(Pico_SCD), type = "l")
matplot(t(Pico_SMS), type = "l")
matplot(t(Pico_WP), type = "l")
matplot(t(Pico_Control), type = "l", xlab = "Time Interval", ylab = "Cell Proportions", main = "Picocyanobacteria Control Panel Cell Removal")
## do i need to skew data here? I feel like they are the most explanatory, even more than if timew was labeled 1-180 b/c they are simple.. but just need a legend for the lines
install.packages(ggplot2)
library(ggplot2)




#Scatterplot using ggplot2


SP<-ggplot(x = Pico_Control, aes(x=time_values, y=Panels, colour = "green")) + geom_point(aes(size = 2.5), alpha=0.7) + ggtitle("Pico Cell Removal on Control Panel over 180 Minute Time Frame")
SP + scale_color_gradient(low="blue", high="yellow")+ theme_bw()
 
#ggsave( width = 10, height = 6, dpi = 300,"scatter_ggplot.png")


#Regression using ggplot2
theme_set(theme_bw)
smooth <- ggplot(data = pedino_cytometry, aes(x = time_values, y = Panels_Pedino))
geom_point(aes(shape=Panels), size = 2.5) + xlab("Time Values 1-180 Minutes" + ylab("Panels"))

## (would do this probably with 2 of the highest diversity/richness
## and the control panel. compare 3 regressions, 3 R-squared values for ease.. need to talk to lexie


##question -- how do i do a legend for the lines

##dpyr::pivot_to_long - this will allow for changing the time 
##add rate of change
##characterize community response at ecosystem - combine with panel data to see any strong species effect
## X label/axis is time intervals (1-12) rather than exact minute intervals

#BEF - CAFE - need to use for flow_cyto? could use the temporal part of it
install.packages("remotes")
remotes::install_github("ctkremer/priceTools")
library("priceTools")

install.packages("tidyverse")
library(tidyr)
?pivot_longer_spec
spec <- time_values %>% build_longer_spec(cols = !time, names_to = "cell removal over time", values_to = "minutes 1-180")    




