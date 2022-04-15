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


comm_pedino_data

mydf <- cbind(rownames(comm_pedino_data),comm_pedino_data)
rownames(mydf) <- NULL
colnames(mydf) <- c(names(mydf)) 
colnames(mydf)[1] <- "Time" 
class(mydf$Time)
mydf$Time <- factor(mydf$Time)
class(mydf$Time)
mydf$Time <- factor(mydf$Time, levels = c('Pedino5', 'Pedino10', 'Pedino15', 'Pedino20', 'Pedino30', 'Pedino40', 'Pedino50', 'Pedino60', 'Pedino90', 'Pedino120', 'Pedino150', 'Pedino180'))


## Pedinophyte Cell Removal Linear Models
# Low diversity score - SCD_01, FC_02, IRL3_02, 
# High diversity scores- ID_03, HI_04, and FC_03
# Control- control 1, control 2, control 3


# Low diversity index panels
ggplot(data=mydf) +
  geom_line(aes(x = Time, y=SCD_01, color= "SCD_01",group = 1))+geom_label(aes(x=12,y=50,label="0.14"), fill="white")+
  geom_line(aes(x = Time, y=FC_02, color = "FC_02", group = 1))+geom_label(aes(x=10,y=78,label="0.21"), fill="white")+
  geom_line(aes(x = Time, y=IRL3_02, color= "IRL3_02",group = 1))+geom_label(aes(x=12,y=80,label="0.3"), fill="white")+
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (/mL)")


# High diversity index panels
ggplot(data=mydf) +
  geom_line(aes(x = Time, y=ID_03, color= "ID_03",group = 1))+
  geom_line(aes(x = Time, y=HI_04, color = "HI_04", group = 1))+
  geom_line(aes(x = Time, y=FC_03, color= "FC_03",group = 1))+ 
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (/mL)")

# Control Panel cell removal
ggplot(data=mydf) +
  geom_line(aes(x = Time, y=`Control 1`, color= "Control 1",group = 1))+
  geom_line(aes(x = Time, y=`Control 2`, color = "Control 2", group = 1))+
  geom_line(aes(x = Time, y=`Control 3`, color= "Control 3",group = 1))+
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (/mL)")


# High diversity (ID_03), Low diversity (SCD_01), & Control 1 Panels For Pedinophyte Cell Removal 
ggplot(data=mydf) +
  geom_line(aes(x = Time, y=ID_03, color= "ID_03",group = 1))+geom_label(aes(x=12,y=100,label="0.908"), fill="white")+
  geom_line(aes(x = Time, y=SCD_01, color = "SCD_01", group = 1))+geom_label(aes(x=12,y=55,label="0.147"), fill="white")+
  geom_line(aes(x = Time, y=`Control 1`, color= "Control 1",group = 1))+ 
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (/mL)")
  
  
Long_Pico = pico_subset %>% 
  pivot_longer(cols = c("Pico5","Pico10","Pico15","Pico20","Pico30","Pico40","Pico50","Pico60","Pico90","Pico120","Pico150","Pico180"), names_to = c("Time"))
head(Long_Pico)


comm_pico_data <- with(Long_Pico, tapply(value, list(Time, Panel), as.integer))
comm_pico_data <- ifelse(is.na(comm_pico_data), 0, comm_pico_data)
class(comm_pico_data)
colnames(comm_pico_data)
comm_pico_data
comm_pico_data<- as.data.frame(comm_pico_data)
comm_pico_data

mydf2 <- cbind(rownames(comm_pico_data),comm_pico_data)
rownames(mydf2) <- NULL
colnames(mydf2) <- c(names(mydf2)) 
colnames(mydf2)[1] <- "Time" 
class(mydf2$Time)
mydf2$Time <- factor(mydf2$Time)
class(mydf2$Time)
mydf2$Time <- factor(mydf2$Time, levels = c('Pico5', 'Pico10', 'Pico15', 'Pico20', 'Pico30', 'Pico40', 'Pico50', 'Pico60', 'Pico90', 'Pico120', 'Pico150', 'Pico180'))

## Picocyanobacteria Cell Removal Linear Models

ggplot(data=mydf2) +
  geom_line(aes(x = Time, y=SCD_01, color= "SCD_01",group = 1))+
  geom_line(aes(x = Time, y=FC_02, color = "FC_02", group = 1))+
  geom_line(aes(x = Time, y=IRL3_02, color= "IRL3_02",group = 1))+
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Picocyanobacteria cells removed (/mL)")


ggplot(data=mydf2) +
  geom_line(aes(x = Time, y=ID_03, color= "ID_03",group = 1))+ geom_label(aes(x=12,y=100,label="0.9"), fill="white")+
  geom_line(aes(x = Time, y=HI_04, color = "HI_04", group = 1))+ geom_label(aes(x=11,y=89,label="0.88"),fill="white")+
  geom_line(aes(x = Time, y=FC_03, color= "FC_03",group = 1))+ geom_label(aes(x=11,y=50,label="0.76"),fill="white")+
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Picocyanobacteria cells removed (/mL)")


ggplot(data=mydf2) +
  geom_line(aes(x = Time, y=`Control 1`, color= "Control 1",group = 1))+
  geom_line(aes(x = Time, y=`Control 2`, color = "Control 2", group = 1))+
  geom_line(aes(x = Time, y=`Control 3`, color= "Control 3", group = 1, ))+
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Picocyanobacteria cells removed (/mL)")


ggplot(data=mydf2) +
  geom_line(aes(x = Time, y=ID_03, color= "ID_03",group = 1))+geom_label(aes(x=11.5,y=94,label="0.908"), fill="white")+
  geom_line(aes(x = Time, y=SCD_01, color = "SCD_01", group = 1))+geom_label(aes(x=12,y=30,label="0.147"), fill="white")+
  geom_line(aes(x = Time, y=`Control 1`, color= "Control 1",group = 1))+ 
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Picocyanobacteria cells removed (/mL)")

coef(lm(mydf2$`Control 1` ~ mydf2$Time))

install.packages("zoo")
yes
library(zoo)
library(ggplot2)
autoplot(zoo(mydf), facet = NULL)

# ggplot(data=mydf) +
#   geom_line(aes(x = Time["Pedino180"], y=ID_03, color= "ID_03",group = 1))+
  
subset_plot_pedino <- subset(mydf, Time == "Pedino180" | Time == "Pedino150")
subset_plot_pedino

ggplot(data=subset_plot_pedino) +
  geom_line(aes(x = Time, y=`Control 1`, color= "Control 1",group = 1))+
  geom_line(aes(x = Time, y=`Control 2`, color ="Control 2", group = 1))+
  geom_line(aes(x = Time, y=`Control 3`, color = "Control 3", group = 1))+
  geom_line(aes(x = Time, y=FC_01, color = "FC_01", group = 1))+
  geom_line(aes(x = Time, y=FC_02, color = "FC_02", group = 1))+
  geom_line(aes(x = Time, y=FC_03, color = "FC_03", group = 1))+
  geom_line(aes(x = Time, y=HI_02, color = "HI_02", group = 1))+
  geom_line(aes(x = Time, y=HI_03, color = "HI_03", group = 1))+
  geom_line(aes(x = Time, y=HI_04, color = "HI_04", group = 1))+
  geom_line(aes(x = Time, y=ID_01, color = "ID_01", group = 1))+
  geom_line(aes(x = Time, y=ID_03, color= "ID_03", group = 1))+ 
  geom_line(aes(x = Time, y=IRL3_01, color = "IRL3_01", group = 1))+
  geom_line(aes(x = Time, y=IRL3_02, color = "IRL3_02", group = 1))+
  geom_line(aes(x = Time, y=IRL3_03, color = "IRL3_03", group = 1))+
  geom_line(aes(x = Time, y=IRL3_04, color= "IRL3_04", group = 1))+ 
  geom_line(aes(x = Time, y=MIM_01, color = "MIM_01", group = 1))+
  geom_line(aes(x = Time, y=MIM_02, color= "MIM_02", group = 1))+ 
  geom_line(aes(x = Time, y=MO_01, color= "MO_01", group = 1))+ 
  geom_line(aes(x = Time, y=MO_02, color = "MO_02", group = 1))+
  geom_line(aes(x = Time, y=MO_03, color= "MO_03", group = 1))+ 
  geom_line(aes(x = Time, y=SCD_01, color = "SCD_01", group = 1))+
  geom_line(aes(x = Time, y=SCD_02, color= "SCD_02", group = 1))+ 
  geom_line(aes(x = Time, y=SCD_03, color= "SCD_03", group = 1))+ 
  geom_line(aes(x = Time, y=SMS_01, color = "SMS_01", group = 1))+
  geom_line(aes(x = Time, y=SMS_03, color = "SMS_03", group = 1))+
  geom_line(aes(x = Time, y=WP_01, color= "WP_01", group = 1))+ 
  geom_line(aes(x = Time, y=WP_03, color = "WP_03", group = 1))+
  geom_line(aes(x = Time, y=WP_04, color= "WP_04", group = 1))+ 
  ylim(NA,100)+
  xlab("Time (minutes)")+
  ylab("Proportion of Original Pedinophtye cells removed (/mL)")

