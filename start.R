# start

library("xlsx")
cytometry_file = read.xlsx("edited_community_experiment_data.xlsx",5)
head(cytometry_file)
cytometry_file$Panel[1]

pico_cytometry <- cytometry_file[ -c(0,14:25) ]
pico_cytometry
pedino_cytometry <- cytometry_file[ -c(0,2:13) ]
pedino_cytometry

plot(Panel[1] ~ Pico5 + Pico10 + Pico15 + Pico20 + Pico30 + Pico40 + Pico50 + Pico60 + Pico90 + Pico120 + Pico150 + Pico180, data = cytometry_file)
time_values = list(5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180)
plot()



  
  
  
    