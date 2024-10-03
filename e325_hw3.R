# Dominic Tanelli
# Prof. Kropp
# ENVST 325
# 3 October 2024

# Homework 3: Intro to data visualization
# In-Class Prompts
# Starter Code
# install.packages(c("dplyr", "lubridate", "ggplot2", "gridExtra"))
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
setwd("~/Downloads/activity03")
datCO2 <- read.csv("~/Downloads/activity03/annual-co-emissions-by-region.csv")
colnames(datCO2)[4] <- "CO2"
climate_change <- read.csv("~/Downloads/activity03/climate-change.csv")
climate_change$date <- ymd(climate_change$Day)

# Prompt 1
# Starter Code
NorthernH <- climate_change[climate_change$Entity == "Northern Hemisphere",]
SouthernH <- climate_change[climate_change$Entity == "Southern Hemisphere",]

# Base R
cat("\nPrompt 1 Answer (Base R): \n")
plot(NorthernH$date, NorthernH$temperature_anomaly, type = "b", pch = 19, ylab = "Temperature Anomaly", 
     xlab = "Year", yaxt = "n", col = "red", main = "Temperature Anomaly by Hemisphere From 1880-2021")
axis(2, seq(0,6000000000, by=2000000000), seq(0,6, by = 2), las=2)
points(SouthernH$date, SouthernH$temperature_anomaly, type = "b", pch = 19, col= "blue")
legend("topleft", c("Northern Hemisphere", "Southern Hemisphere"), col=c("red", "blue"), pch=19, bty= "n")

# ggplot
cat("\nPrompt 1 Answer (ggplot): \n")
NS_H <- rbind(NorthernH, SouthernH) # Northern and Southern Hemisphere Combined Data Frame
ggplot(data = NS_H, aes(x = date, y = temperature_anomaly, color = Entity ) ) + geom_point() + geom_line() + 
  labs(x = "Year", y = "Temperature Anomaly") + 
  ggtitle("Temperature Anomaly by Hemisphere From 1880-2021") + theme_classic() + 
  scale_color_manual(values = c("#7FB3D555","#34495E55")) 

# Prompt 2 + Optional Challenge
cat("\nPrompt 2 + Optional Challenge Answer: \n")
NorthA <- datCO2[datCO2$Entity == "United States" | datCO2$Entity == "Canada" | datCO2$Entity == "Mexico", ]
ggplot(data = NorthA, aes(x = Year, y = CO2, color = Entity)) + geom_point() + geom_line() + labs(x = "Year", 
       y = expression("Fossil Fuel"~CO[2]~"Emissions (tons)")) + 
  ggtitle(expression("Fossil Fuel"~CO[2]~"Emissions in North America (US, Canada, Mexico)")) + theme_classic()

# Homework
# Question 1
cat("\nQuestion 1 Answer: \n")
my_countries <- datCO2[datCO2$Entity == "Germany" | datCO2$Entity == "Ireland" | datCO2$Entity == "Italy" |
                         datCO2$Entity == "United States" | datCO2$Entity == "United Kingdom", ]
ggplot(data = my_countries, aes(x = Year, y = CO2, color = Entity)) + geom_point() + geom_line() + 
  labs(x = "Year", y = expression("Fossil Fuel"~CO[2]~"Emissions (tons)")) + 
  ggtitle(expression("Fossil Fuel"~CO[2]~"Emissions in US, UK, Germany, Ireland, and Italy")) + theme_classic()

# Question 2
# Plot 1: World CO2 Emissions
world_CO2 <- datCO2[datCO2$Entity == "World", ]
world_CO2_plot <- ggplot(data = world_CO2, aes(x = Year, y = CO2)) + geom_line(color = "green") + 
  geom_point(color = "green") + labs(x = "Year", y = expression(CO[2]~"Emissions (tons)")) + 
  ggtitle("Global CO2 Emissions") + theme_classic()

# Plot 2: World Air Temperature Anomalies
world_climate_change <- climate_change[climate_change$Entity == "World", ]
world_climate_change_plot <- ggplot(data = world_climate_change, aes(x = date, y = temperature_anomaly)) + 
  geom_line(color = "orange") + geom_point(color = "orange") + labs(x = "Year", y = "Temperature Anomaly ") + 
  ggtitle("Global Air Temperature Anomalies") + theme_classic()

# Arrange the two plots side by side
grid.arrange(world_CO2_plot, world_climate_change_plot, ncol = 2)

# Question 3

# Question 4
cat("\nQuestion 4 Answer: See Word PDF \n")
