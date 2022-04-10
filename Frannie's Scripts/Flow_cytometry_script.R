file <- read.csv("Frannie's Scripts/edited_community_experiment_data.csv")
file
file <- na.omit(file)
file

library(tidyr)

pico_subset <- file[,c("Panel","Pico5","Pico10","Pico15","Pico20",
                       "Pico30","Pico40","Pico50","Pico60","Pico90","Pico120","Pico150","Pico180")]
head(pico_subset)

pedino_subset <- file[,c("Panel","Pedino5","Pedino10","Pedino15","Pedino20",
                       "Pedino30","Pedino40","Pedino50","Pedino60","Pedino90","Pedino120","Pedino150","Pedino180")]
head(pedino_subset)

Long_pedino = pedino_subset %>%
  pivot_longer(cols = c("Pedino5","Pedino10","Pedino15","Pedino20",
               "Pedino30","Pedino40","Pedino50","Pedino60","Pedino90","Pedino120","Pedino150","Pedino180"), names_to = c("Time"))

head(Long_pedino)

comm_pedino_data <- with(Long_pedino, tapply(value, list(Time, Panel), as.integer))
comm_pedino_data <- ifelse(is.na(comm_pedino_data), 0, comm_pedino_data)
class(comm_pedino_data)
colnames(comm_pedino_data)
plot(comm_pedino_data$FC_01)

comm_pedino_data<- as.data.frame(comm_pedino_data)
comm_pedino_data

mydf <- cbind(rownames(comm_pedino_data),comm_pedino_data)
rownames(mydf) <- NULL
colnames(mydf) <- c(names(mydf)) #to not write all the column names

colnames(mydf)[1] <- "Time" 
names(mydf)



mydf

ggplot(data=mydf, aes(x=Time, group = 1)) +
  geom_line(aes(y=FC_01))



