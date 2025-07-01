library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggforce)
library(gcookbook)
library(magrittr)
library(reshape)
library(GGally)
library(readxl)
library(corrplot)


#Importing the data
NBAStats <- read_excel("NBA_Player_Stats.xlsx")
head(NBAStats)

#Cleaning the data
NBAStats <- NBAStats %>%
  na.omit()

#Exploratory analysis
ExploratoryData <- NBAStats %>%
  group_by(Pos) %>%
  summarise(FTAsum = sum(FTA))

ggplot(ExploratoryData, aes(x = Pos, y = FTAsum)) + 
  geom_bar(stat = "identity") +
  labs(title = "Total Free Throw Attempts by Position",
       x = "Position",
       y = "Total Free Throw Attempts") +
  theme_minimal()

#Preparing the data
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

freeThrows <- NBAStats %>%
  group_by(Player) %>%
  summarize(FreeThrowAttempts = sum(FTA),
            FreeThrowPercentage = mean(`FT%`),
            Pos = get_mode(Pos)) %>%
  filter(Pos %in% c("C", "PF", "SF", "SG", "PG"),
         FreeThrowAttempts >= 1)

#Creating the label for the average
average_stats <- freeThrows %>%
  group_by(Pos) %>%
  summarise(
    average_attempts = mean(FreeThrowAttempts),
    average_percentage = mean(FreeThrowPercentage))

average_stats <- average_stats %>%
  mutate(label = paste(Pos, " -> Avg. Percentage: ", sprintf("%.1f%%", average_percentage * 100), sep = ""))

freeThrows <- freeThrows %>%
  left_join(average_stats, by = "Pos")

#Creating the visualization
ggplot(freeThrows, aes(x = FreeThrowAttempts, y = FreeThrowPercentage, color = label)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Pos, ncol = 2)
  scale_color_discrete(name = "Position", labels = average_stats$label) +
  labs(title = "NBA Free Throw Accuracy based on the Attemps",
       subtitle = "by each Player",
       x = "Free Throw Attempts \n(Sum of all Seasons)",
       y = "Free Throw Percentage \n(Mean of all Seasons)",
       color = "Position") +
  theme_bw() +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 13),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

  #Creating the visualization
ggplot(freeThrows, aes(x = FreeThrowAttempts, y = FreeThrowPercentage, color = label)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Pos, ncol = 2) +
  labs(title = "NBA Free Throw Accuracy based on the Attemps",
       subtitle = "by each Player",
       x = "Free Throw Attempts \n(Sum of all Seasons)",
       y = "Free Throw Percentage \n(Mean of all Seasons)",
       color = "Position") +
  theme_bw() +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 13),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none")









