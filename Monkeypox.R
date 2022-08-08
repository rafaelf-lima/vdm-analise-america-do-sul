library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggtext)
library(showtext)


font_add_google("Montserrat", "montserrat")
font_add_google("Josefin Sans", "josefinsans")
font_add_google("Karma", "karma")
showtext_auto()
set.seed(123)

df <- read.csv("C:\\Users\\ritad\\timeseries-country-confirmed.csv")
view(df)


sacountries <- c("Brazil", "Argentina", "Venezuela", "Chile", "Uruguay", "Paraguay", "Bolivia", "Colombia", "Peru", "Ecuador")


str(df)

df$Date <- ymd(df$Date)

str(df)

dfs <- df %>% 
  filter(Country %in% sacountries) %>% 
  arrange(desc(Cumulative_cases)) 
         


sam <- ggplot(dfs, aes(x = Date, y = Cumulative_cases, color = Country, label = Country))+
  geom_line(size = .9)+
  scale_color_manual(labels = c("Argentina", "Brasil", "Chile", "Colômbia", "Chile", "Equador", "Peru", "Venezuela"), values = c("#6CACE4", "#FFDF00", "#d52b1e", "#FCD116", "#ffdd00", "#D91023", "#722F37"))+
  scale_x_date(date_breaks = "1 week", date_labels = "%d de %B", limits = c(min(dfs$Date), max = max(dfs$Date)),
               expand=c(0,0))+
  labs(title = 'Varíola dos Macacos: Casos acumulados confirmados na América do Sul (até 29 de agosto)',  
  subtitle  =  "O Brasil é o país mais atingido no continente e também foi o primeiro a registrar morte pela zoonose viral",
  caption  = c("@rafaelf_lima", "Dados: https://t.co/HleY6a09BD"))+
  theme(
    plot.background = element_rect(fill = "#F2DFCE", color = "#F2DFCE"),
    panel.background =element_rect(fill = "#F2DFCE", color = "#F2DFCE"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#FFF1E0"),
    axis.text = element_text(colour = "#514438", family = "montserrat", size = 12, face = "bold"),
    axis.line.y = element_line(colour = '#FFF1E0', size=0.5, linetype='solid'),
    legend.position = "none",
    axis.title =element_blank(),
    plot.title = element_text(size = 20, colour = "#0D7680", family = "josefinsans", face = "bold"),
    plot.subtitle = element_text(size = 15, colour = "#0D7680",  family = "josefinsans", face = "bold"),
    plot.margin = unit(c(1,3,1,1), "cm"),
    plot.caption = element_text(size = 8, family = "josefinsans", colour = "#514438", hjust = c(0, 1))
  )
   
  
sam + annotate(geom="text", x= as.Date("2022-07-29", format = "%Y-%m-%d"), y=1000, label="Brasil",
               color="#514438", family = "montserrat", size = 3.2, hjust = -0.2) +
  annotate(geom="text", x= as.Date("2022-07-29", format = "%Y-%m-%d"), y=270, label="Peru",
           color="#514438", family = "montserrat", size = 3.2, hjust = -0.2) +
  annotate(geom="text", x= as.Date("2022-07-29", format = "%Y-%m-%d"), y=65, label="Chile",
           color="#514438", family = "montserrat", size = 3.2, hjust = -0.2)+
annotate(geom="text", x= as.Date("2022-07-29", format = "%Y-%m-%d"), y=35, label="Argentina",
         color="#514438", family = "montserrat", size = 3.2, hjust = -0.1) +
  annotate(geom="text", x= as.Date("2022-07-29", format = "%Y-%m-%d"), y=11, label="Colômbia",
           color="#514438",family = "montserrat", size = 3.2, hjust = -0.1)+
  annotate(geom="text", x= as.Date("2022-07-29", format = "%Y-%m-%d"), y=-10, label="Venezuela",
           color="#514438", family = "montserrat", size = 3.2,hjust = -0.1)+
  coord_cartesian(clip = "off")

