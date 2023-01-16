# Load in Data 
setwd("/Users/estrella/Downloads")
games <- read.csv("games-features-edit.csv")

library(tidyverse)
library(stringr)

#tibble with only games 
games_t <- games %>%
  filter(GenreIsNonGame == FALSE) %>%
  select(ReleaseDate, GenreIsIndie, GenreIsAction, 
         GenreIsAdventure, GenreIsRPG, GenreIsSimulation, 
         GenreIsSports, GenreIsRacing, RecommendationCount) %>%
  mutate(
    # formatting the year
    ReleaseDate = as.numeric(str_sub(ReleaseDate, start= -4)),
    
    # Retreiving all the genres
    Genre = case_when(
      GenreIsIndie == TRUE ~ "Indie",
      GenreIsAction == TRUE ~ "Action",
      GenreIsAdventure == TRUE ~ "Adventure",
      GenreIsRPG == TRUE ~ "RPG",
      GenreIsSimulation == TRUE ~ "Simulation",
      GenreIsSports == TRUE ~ "Sports",
      GenreIsRacing == TRUE ~ "Racing"
    )
  ) %>%
    select(c(ReleaseDate, Genre, RecommendationCount)) %>%
  na.omit() # getting rid of empty data 

# save as data frame, and retrieve only 4 total years 
games_df <- as.data.frame(games_t)
games_df_year <- subset(games_df, ReleaseDate == c(2011, 2012, 2013, 2014, 2015, 2016))


# create stackd bar
library(ggplot2)
library(viridis)
library(hrbrthemes)

ggplot(games_df_year,
       aes(fill = Genre, y = RecommendationCount, x=ReleaseDate)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis_d(option="C") +
  ggtitle("Released Years of Games on Steam by Recommendations") +
  
  theme_bw() +
  theme(legend.position = "left",
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(x = "Year of Release",
       y = "Percentage (%) of Recomendation Counts")


#------------------------------------------------------------------

#create bar graph 

games_price <- games %>%
  filter (GenreIsNonGame == FALSE) %>%
  select(GenreIsIndie, GenreIsAction, GenreIsAdventure, GenreIsRPG, PriceInitial, Metacritic) %>%
  # Retreiving all the genres
  mutate (
    PriceInitial = round(PriceInitial, digits = 2), # rounding to the nearest dollar
    Genre = case_when(
      GenreIsIndie == TRUE ~ "Indie",
      GenreIsAction == TRUE ~ "Action",
      GenreIsAdventure == TRUE ~ "Adventure",
      GenreIsRPG == TRUE ~ "RPG"
    )
  ) %>%
  select(c(Metacritic, PriceInitial, Genre)) %>%
  na.omit() 


games_df_price <- as.data.frame(subset(games_price, PriceInitial != 0)) 
games_df_price2 <- subset(games_df_price, Metacritic != 0)
games_df_price3 <- subset(games_df_price2, PriceInitial < 100)

ggplot(games_df_price3,
       aes(x = Metacritic, color = Genre)) +
  geom_bar() +
  labs(title = "Number of Games for Metacritic score by Genre") +
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(x = "Metacritic score (out of 100)",
       y = "Number of Games")


# -----------------------------------------------------------
# bubble plot interactive 
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(gapminder)

games_price <- games %>%
  filter (GenreIsNonGame == FALSE) %>%
  filter (IsFree == FALSE) %>%
  select(GenreIsIndie, GenreIsAdventure, GenreIsAction, PriceInitial, ReleaseDate, ResponseName, RecommendationCount) %>%

  mutate (
    ReleaseDate = as.numeric(str_sub(ReleaseDate, start= -4)),
    PriceInitial = round(PriceInitial, digits = 2), # rounding to the nearest dollar
    Genre = case_when(
      GenreIsIndie == TRUE ~ "Indie",
      GenreIsAction == TRUE ~ "Action",
      GenreIsAdventure == TRUE ~ "Adventure"
    )
  ) %>%
 select(c(ReleaseDate, PriceInitial, Genre, ResponseName, RecommendationCount)) %>%
  na.omit() %>%
  top_n(100) %>% #select only the first 100 values in the dataset
  subset(RecommendationCount < 70000)
  
  # ggplot 
ggplot(games_price, aes(x = ReleaseDate, y = RecommendationCount, color = Genre, size = PriceInitial)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1.4, 10), name = "Price of Game (in $USD)") +
  scale_color_viridis(discrete = TRUE, guide = FALSE) +
  theme(legend.position = "left",
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(
    title="Top Steam Genres Release Date by Recommendation and Price of Game",
    x = "Year of Release",
    y = "Number of Recommendations"
  )

  games_price_df = as.data.frame(games_price)

  plotly_json(games_price)
  # turn interactive 
ggplotly(sub_games_price, tooltip = "text")

