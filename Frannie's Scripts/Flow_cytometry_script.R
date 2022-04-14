file <- read.csv("Frannie's Scripts/edited_community_experiment_data.csv")
file
file <- na.omit(file)
file

library(tidyr)
library(ggplot2)

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
comm_pedino_data
comm_pedino_data<- as.data.frame(comm_pedino_data)
comm_pedino_data
# my_factor <- factor(sample(c('Pedino10', 'Pedino120', 'Pedino15', 'Pedino150', 'Pedino180', 'Pedino20', 'Pedino30', 'Pedino40', 'Pedino5', 'Pedino50', 'Pedino60', 'Pedino90'), size = 12, replace = T))
# my_factor

comm_pedino_data

mydf <- cbind(rownames(comm_pedino_data),comm_pedino_data)
rownames(mydf) <- NULL
colnames(mydf) <- c(names(mydf)) #to not write all the column names
colnames(mydf)[1] <- "Time" 
class(mydf$Time)
mydf$Time <- factor(mydf$Time)
class(mydf$Time)
mydf$Time <- factor(mydf$Time, levels = c('Pedino5', 'Pedino10', 'Pedino15', 'Pedino20', 'Pedino30', 'Pedino40', 'Pedino50', 'Pedino60', 'Pedino90', 'Pedino120', 'Pedino150', 'Pedino180'))


# low 
ggplot(data=mydf) +
  geom_line(aes(x = Time, y=SCD_01, color= "SCD_01",group = 1))+
  geom_line(aes(x = Time, y=FC_02, color = "FC_02", group = 1))+
  geom_line(aes(x = Time, y=IRL3_02, color= "IRL3_02",group = 1))+
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (add units)")


# High
ggplot(data=mydf) +
  geom_line(aes(x = Time, y=ID_03, color= "ID_03",group = 1))+
  geom_line(aes(x = Time, y=HI_04, color = "HI_04", group = 1))+
  geom_line(aes(x = Time, y=FC_03, color= "FC_03",group = 1))+ 
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (add units)")

# control
ggplot(data=mydf) +
  geom_line(aes(x = Time, y=`Control 1`, color= "Control 1",group = 1))+
  geom_line(aes(x = Time, y=`Control 2`, color = "Control 2", group = 1))+
  geom_line(aes(x = Time, y=`Control 3`, color= "Control 3",group = 1))+
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (add units)")


# hi hi low low and control 
ggplot(data=mydf) +
  geom_line(aes(x = Time, y=ID_03, color= "ID_03",group = 1))+
  geom_line(aes(x = Time, y=SCD_01, color = "SCD_01", group = 1))+
  geom_line(aes(x = Time, y=`Control 1`, color= "Control 1",group = 1))+ 
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (add units)")



summary(lm(mydf$ID_03~ -1 + mydf$Time))

  ggplot(data=mydf, aes(x=Time, group = 1)) + 
  geom_line(aes(y=))

# low diversity score - SCD_01, FC_02, IRL3_02, 
# high diversity scores- ID_03, HI_04, and FC_03
# control- control 1, control 2, control 3, 





